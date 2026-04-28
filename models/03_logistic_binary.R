# ================================================================
# Logistic Regression: xG + Odds + Form + Elo + Player Quality
# (Player stats from JaseZiv worldfootballR_data, multi-season)
# ================================================================

# ---- Packages ----
needed <- c(
  "dplyr", "readr", "tibble", "tidyr", "stringr", "lubridate",
  "ggplot2", "janitor", "slider", "glmnet",
  "data.table", "stringi", "worldfootballR", "viridis"
)

new <- setdiff(needed, rownames(installed.packages()))
if (length(new)) {
  install.packages(new, repos = "https://cloud.r-project.org")
}
invisible(lapply(needed, library, character.only = TRUE))

# ---- Settings ----
fig_dir <- "outputs/figures"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Helper functions ----

# Rolling mean (recent form)
rolling_mean <- function(x, n = 5) {
  slider::slide_dbl(x, mean, .before = n - 1, .complete = TRUE)
}

# Pseudo-R² for GLM
pseudo_r2 <- function(m) {
  1 - (m$deviance / m$null.deviance)
}

# Logloss for binary outcomes
logloss <- function(p, y) {
  p <- pmin(pmax(p, 1e-12), 1 - 1e-12)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

# AUC (manual)
auc <- function(p, y) {
  o   <- order(p)
  p   <- p[o]
  y   <- y[o]
  n1  <- sum(y == 1)
  n0  <- sum(y == 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  ranks <- rank(p)
  (sum(ranks[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# Standardise team names (used for Elo, players, BT, etc.)
std_key <- function(x) {
  x <- tolower(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")           # strip accents
  x <- gsub("\\(.*?\\)|\\[.*?\\]", "", x)                      # remove bracketed bits
  x <- gsub("[^a-z0-9 ]", " ", x)                              # keep letters/digits/space
  x <- gsub("\\b(fc|afc|cf|sc|club|u\\d{2})\\b", " ", x)       # drop suffixes
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  
  # Harmonise common EPL variants
  x <- sub("^manchester united$", "man united", x)
  x <- sub("^manchester utd$",   "man united", x)
  x <- sub("^manchester city$",  "man city",   x)
  x <- sub("^wolverhampton wanderers$", "wolves", x)
  x <- sub("^tottenham hotspur$", "tottenham", x)
  x <- sub("^brighton and hove albion$", "brighton", x)
  x <- sub("^west ham united$", "west ham", x)
  x <- sub("^newcastle united$", "newcastle", x)
  x <- sub("^newcastle utd$",    "newcastle", x)   
  x <- sub("^nottingham forest$", "nottm forest", x)
  x <- sub("^sheffield united$", "sheffield utd", x)
  x <- sub("^leicester city$", "leicester", x)
  x <- sub("^leeds united$", "leeds", x)
  x <- sub("^norwich city$", "norwich", x)
  x
}

# ================================================================
# 1) Load merged CSV (xG + goals + odds)
# ================================================================

processed_path <- file.path("data", "processed", "epl_matches_with_xg_odds.csv")
if (!file.exists(processed_path)) {
  stop(
    "File not found: ", processed_path,
    "\nPlease run your update_odds / merge script first."
  )
}

dat <- readr::read_csv(processed_path, show_col_types = FALSE) %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    date      = as.Date(date),
    home_win  = as.integer(home_goals > away_goals),
    xg_diff   = home_xg - away_xg
  ) %>%
  dplyr::arrange(date)

# ------------------------------------------------
# Assign season_end_year (safety, though it exists)
# ------------------------------------------------
dat <- dat %>%
  dplyr::mutate(
    season_end_year = dplyr::if_else(
      lubridate::month(date) >= 7L,
      lubridate::year(date) + 1L,
      lubridate::year(date)
    )
  ) %>%
  # restrict to 22/23, 23/24, 24/25
  dplyr::filter(season_end_year %in% c(2023L, 2024L, 2025L)) %>%
  dplyr::mutate(
    home_team = factor(home_team),
    away_team = factor(away_team)
  )

cat("Rows after load + season filter:", nrow(dat), "\n")

# ================================================================
# 2) Recent form (last 5 matches)
# ================================================================

rolling_mean <- function(x, n) {
  slider::slide_dbl(x, mean, .before = n, .complete = TRUE)
  # .before = n means: current + previous n values, BUT we will lag first,
  # so it becomes previous n matches only.
}

add_form_features <- function(df, k = 5) {
  stopifnot(all(c("date","home_team","away_team","home_goals","away_goals","home_xg","away_xg") %in% names(df)))
  
  # --- Home-team form based on previous HOME matches in your dataset ---
  # (This matches what you were doing, but now leakage-free via lag)
  home_form <- df %>%
    dplyr::group_by(home_team) %>%
    dplyr::arrange(date, .by_group = TRUE) %>%
    dplyr::mutate(
      home_xg_diff_form   = rolling_mean(dplyr::lag(home_xg - away_xg), k),
      home_goal_diff_form = rolling_mean(dplyr::lag(home_goals - away_goals), k),
      home_win_form       = rolling_mean(dplyr::lag(as.integer(home_goals > away_goals)), k)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, home_team, home_xg_diff_form, home_goal_diff_form, home_win_form)
  
  # --- Away-team form based on previous AWAY matches ---
  away_form <- df %>%
    dplyr::group_by(away_team) %>%
    dplyr::arrange(date, .by_group = TRUE) %>%
    dplyr::mutate(
      away_xg_diff_form   = rolling_mean(dplyr::lag(away_xg - home_xg), k),
      away_goal_diff_form = rolling_mean(dplyr::lag(away_goals - home_goals), k),
      away_win_form       = rolling_mean(dplyr::lag(as.integer(away_goals > home_goals)), k)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, away_team, away_xg_diff_form, away_goal_diff_form, away_win_form)
  
  df %>%
    dplyr::left_join(home_form, by = c("date","home_team")) %>%
    dplyr::left_join(away_form, by = c("date","away_team"))
}

# Choose window length here (try 5, 10, 15)
k_form <- 5
dat <- add_form_features(dat, k = k_form)

cat("Rows after form merge (k = ", k_form, "): ", nrow(dat), "\n", sep = "")

# =========================================================
# 3) Robust ClubElo merge: elo_diff = home_elo - away_elo
# =========================================================

elo_path <- "data/elo/clubelo_all.csv"
if (!file.exists(elo_path)) {
  stop("ClubElo file not found at: ", elo_path)
}

elo_raw <- readr::read_csv(elo_path, show_col_types = FALSE)

# Detect columns in ClubElo file
club_col <- grep("club|team", names(elo_raw), ignore.case = TRUE, value = TRUE)[1]
elo_col  <- grep("elo", names(elo_raw),  ignore.case = TRUE, value = TRUE)[1]
from_col <- grep("^from$", names(elo_raw), ignore.case = TRUE, value = TRUE)[1]
stopifnot(!is.na(club_col), !is.na(elo_col), !is.na(from_col))

parse_any_date <- function(x) {
  suppressWarnings(dplyr::coalesce(
    lubridate::ymd(x),
    lubridate::dmy(x),
    lubridate::mdy(x)
  ))
}

elo_all <- elo_raw %>%
  dplyr::transmute(
    team_raw = .data[[club_col]],
    date     = parse_any_date(.data[[from_col]]),
    elo      = as.numeric(.data[[elo_col]])
  ) %>%
  dplyr::filter(!is.na(team_raw), !is.na(date), !is.na(elo)) %>%
  dplyr::mutate(team_key = std_key(team_raw)) %>%
  dplyr::filter(nchar(team_key) > 0) %>%
  dplyr::arrange(team_key, date) %>%
  dplyr::distinct()

# Build keys in match data
dat <- dat %>%
  dplyr::mutate(
    date     = as.Date(date),
    home_key = std_key(as.character(home_team)),
    away_key = std_key(as.character(away_team))
  )

needed_keys <- sort(unique(c(dat$home_key, dat$away_key)))
elo_keep <- elo_all %>% dplyr::filter(team_key %in% needed_keys)

cat(
  "Teams in matches:", length(needed_keys),
  "  Teams present in Elo:", length(unique(elo_keep$team_key)), "\n"
)

missing_in_elo <- setdiff(needed_keys, unique(elo_keep$team_key))
if (length(missing_in_elo)) {
  cat("⚠️ Teams NOT found in Elo after normalisation:\n")
  print(missing_in_elo)
}

# Rolling join with data.table
elo_dt <- data.table::as.data.table(elo_all)
data.table::setkey(elo_dt, team_key, date)

dat_dt <- data.table::as.data.table(dat)

# home_elo: last Elo on/before date; fallback to next after date
dat_dt[, home_elo := elo_dt[
  .SD,
  on = .(team_key = home_key, date = date),
  roll = Inf,
  x.elo
], .SDcols = c("home_key", "date")]

dat_dt[, home_elo_next := elo_dt[
  .SD,
  on = .(team_key = home_key, date = date),
  roll = -Inf,
  x.elo
], .SDcols = c("home_key", "date")]

dat_dt[, home_elo := data.table::fifelse(is.na(home_elo), home_elo_next, home_elo)]
dat_dt[, home_elo_next := NULL]

# away_elo
dat_dt[, away_elo := elo_dt[
  .SD,
  on = .(team_key = away_key, date = date),
  roll = Inf,
  x.elo
], .SDcols = c("away_key", "date")]

dat_dt[, away_elo_next := elo_dt[
  .SD,
  on = .(team_key = away_key, date = date),
  roll = -Inf,
  x.elo
], .SDcols = c("away_key", "date")]

dat_dt[, away_elo := data.table::fifelse(is.na(away_elo), away_elo_next, away_elo)]
dat_dt[, away_elo_next := NULL]

dat <- as.data.frame(dat_dt) %>%
  dplyr::mutate(elo_diff = home_elo - away_elo)

cat(
  "Elo coverage → missing home_elo:", sum(is.na(dat$home_elo)),
  "  missing away_elo:", sum(is.na(dat$away_elo)), "\n"
)

# ================================================================
# 3B) Time plots for evolving team strength
# ================================================================

teams_to_plot <- std_key(c("Manchester City", "Arsenal", "Chelsea", "Burnley"))

team_time <- dplyr::bind_rows(
  dat %>%
    dplyr::transmute(
      date,
      season_end_year,
      team = home_key,
      opponent = away_key,
      venue = "Home",
      elo = home_elo,
      xg_diff_form = home_xg_diff_form,
      goal_diff_form = home_goal_diff_form,
      win_form = home_win_form
    ),
  dat %>%
    dplyr::transmute(
      date,
      season_end_year,
      team = away_key,
      opponent = home_key,
      venue = "Away",
      elo = away_elo,
      xg_diff_form = away_xg_diff_form,
      goal_diff_form = away_goal_diff_form,
      win_form = away_win_form
    )
) %>%
  dplyr::arrange(team, date)

elo_plot_data <- team_time %>%
  dplyr::filter(team %in% teams_to_plot)

print(dplyr::count(elo_plot_data, team))

p_elo_time <- ggplot2::ggplot(
  elo_plot_data,
  ggplot2::aes(x = date, y = elo, colour = team)
) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::facet_wrap(~ season_end_year, scales = "free_x") +
  ggplot2::labs(
    title = "Pre-match Elo Ratings Over Time",
    subtitle = "Selected Premier League teams by season",
    x = "Date",
    y = "Elo rating",
    colour = "Team"
  ) +
  ggplot2::theme_minimal(base_size = 12)

print(p_elo_time)

ggplot2::ggsave(
  filename = file.path(fig_dir, "elo_time_plot_selected_teams.png"),
  plot = p_elo_time,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

p_xg_form_time <- ggplot2::ggplot(
  elo_plot_data,
  ggplot2::aes(x = date, y = xg_diff_form, colour = team)
) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::facet_wrap(~ season_end_year, scales = "free_x") +
  ggplot2::labs(
    title = "Rolling xG Difference Form Over Time",
    subtitle = "Previous 5 same-venue matches, selected Premier League teams",
    x = "Date",
    y = "Rolling xG difference form",
    colour = "Team"
  ) +
  ggplot2::theme_minimal(base_size = 12)

print(p_xg_form_time)

ggplot2::ggsave(
  filename = file.path(fig_dir, "xg_form_time_plot_selected_teams.png"),
  plot = p_xg_form_time,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

cat("\nSaved figures:\n",
    "- outputs/figures/elo_time_plot_selected_teams.png\n",
    "- outputs/figures/xg_form_time_plot_selected_teams.png\n")

# ================================================================
# 4) Player stats from JaseZiv worldfootballR_data
#    -> team × season_quality (team_quality, quality_diff)
# ================================================================

season_vec <- sort(unique(dat$season_end_year))
cat("Seasons in match data:", paste(season_vec, collapse = ", "), "\n")

players_raw <- worldfootballR::load_fb_big5_advanced_season_stats(
  season_end_year = season_vec,
  stat_type       = "standard",
  team_or_player  = "player"
)

cat("✅ Loaded", nrow(players_raw), "player-season rows from worldfootballR_data.\n")

# ============================================================
# Player stats -> team-level quality (using minutes / xG_Expected)
# ============================================================

players_clean <- players_raw %>%
  janitor::clean_names()
# names(players_clean) now:
# season_end_year, squad, comp, player, nation, pos, age, born,
# mp_playing, starts_playing, min_playing, mins_per_90_playing,
# gls, ast, g_a, g_minus_pk, pk, pkatt, crd_y, crd_r,
# gls_per, ast_per, g_a_per, g_minus_pk_per,
# xg_expected, npxg_expected, xag_expected, npxg_xag_expected, ...

players_team <- players_raw %>%
  janitor::clean_names() %>%
  # Premier League only
  dplyr::filter(comp == "Premier League") %>%
  dplyr::mutate(
    team_key = std_key(squad),             # normalised team name
    nineties = mins_per_90_playing         # THIS is the correct column
  ) %>%
  dplyr::filter(nineties > 0) %>%
  dplyr::group_by(season_end_year, team_key) %>%
  dplyr::summarise(
    total_nineties = sum(nineties,      na.rm = TRUE),
    total_xg       = sum(x_g_expected,  na.rm = TRUE),   # CORRECT NAME
    total_xag      = sum(x_ag_expected, na.rm = TRUE),   # CORRECT NAME
    .groups        = "drop"
  ) %>%
  dplyr::mutate(
    xg_per90     = total_xg  / total_nineties,
    xag_per90    = total_xag / total_nineties,
    team_quality = as.numeric(scale(xg_per90 + xag_per90))
  )

cat("\n✔ Player-quality table built successfully. Sample:\n")
print(head(players_team))

cat("\n✅ Player Team Quality (Top 5 by team_quality):\n")
print(players_team %>% dplyr::arrange(dplyr::desc(team_quality)) %>% head(5))


# ================================================================
# 5) Build modelling dataset (with Elo + player quality)
# ================================================================

compare_vars_base <- c(
  "home_win", "xg_diff", "p_home",
  "home_goal_diff_form", "away_goal_diff_form",
  "home_win_form", "away_win_form",
  "home_xg_diff_form", "away_xg_diff_form",
  "elo_diff"
)

dat_elo_cc <- dat %>%
  tidyr::drop_na(dplyr::all_of(compare_vars_base)) %>%
  dplyr::mutate(
    home_team_key = std_key(as.character(home_team)),
    away_team_key = std_key(as.character(away_team))
  )

# Merge player-based team quality by (season_end_year, team_key)
dat_elo_cc <- dat_elo_cc %>%
  # join home team quality
  dplyr::left_join(
    players_team %>%
      dplyr::select(season_end_year, team_key, team_quality),
    by = c("season_end_year" = "season_end_year",
           "home_team_key"  = "team_key")
  ) %>%
  dplyr::rename(home_team_quality = team_quality) %>%
  
  # join away team quality
  dplyr::left_join(
    players_team %>%
      dplyr::select(season_end_year, team_key, team_quality),
    by = c("season_end_year" = "season_end_year",
           "away_team_key"  = "team_key")
  ) %>%
  dplyr::rename(away_team_quality = team_quality) %>%
  
  dplyr::mutate(
    quality_diff = home_team_quality - away_team_quality
  )

cat("\n✅ Merge complete. Sample rows with team quality:\n")
print(
  dat_elo_cc %>%
    dplyr::select(
      date, season_end_year,
      home_team, away_team,
      home_team_quality, away_team_quality, quality_diff
    ) %>%
    head()
)

# Unified modelling dataset with all features available
model_vars <- c(
  "home_win",
  "xg_diff", "p_home",
  "home_goal_diff_form", "away_goal_diff_form",
  "home_win_form", "away_win_form",
  "home_xg_diff_form", "away_xg_diff_form",
  "elo_diff",
  "quality_diff"
)

dat_model <- dat_elo_cc %>%
  dplyr::filter(
    stats::complete.cases(dplyr::across(dplyr::all_of(model_vars)))
  )

cat("\nRows available for full model (all features):", nrow(dat_model), "\n")

# ================================================================
# 6) Stepwise GLMs: add feature blocks one by one
# ================================================================

# 1) xG only
m1_xg <- glm(
  home_win ~ xg_diff,
  data   = dat_model,
  family = binomial()
)

# 2) xG + odds
m2_xg_odds <- glm(
  home_win ~ xg_diff + p_home,
  data   = dat_model,
  family = binomial()
)

# 3) + form
m3_form <- glm(
  home_win ~ xg_diff + p_home +
    home_goal_diff_form + away_goal_diff_form +
    home_win_form       + away_win_form,
  data   = dat_model,
  family = binomial()
)

# 4) + Elo
m4_form_elo <- glm(
  home_win ~ xg_diff + p_home +
    home_goal_diff_form + away_goal_diff_form +
    home_win_form       + away_win_form +
    elo_diff,
  data   = dat_model,
  family = binomial()
)

# 5) + player-based team quality
m5_form_elo_q <- glm(
  home_win ~ xg_diff + p_home +
    home_goal_diff_form + away_goal_diff_form +
    home_win_form       + away_win_form +
    elo_diff + quality_diff,
  data   = dat_model,
  family = binomial()
)

fits <- list(
  xg                    = m1_xg,
  xg_odds               = m2_xg_odds,
  xg_odds_form          = m3_form,
  xg_odds_form_elo      = m4_form_elo,
  xg_odds_form_elo_qual = m5_form_elo_q
)

insample_summary <- tibble::tibble(
  model     = names(fits),
  k_params  = vapply(fits, function(m) length(stats::coef(m)), integer(1)),
  AIC       = vapply(fits, AIC, numeric(1)),
  pseudo_r2 = vapply(fits, pseudo_r2, numeric(1))
) %>%
  dplyr::arrange(AIC)

cat("\n=== In-sample comparison (AIC, pseudo-R^2) ===\n")
print(insample_summary)

dir.create("outputs/models", recursive = TRUE, showWarnings = FALSE)

saveRDS(
  list(
    m1_xg = m1_xg,
    m2_xg_odds = m2_xg_odds,
    m3_form = m3_form,
    m4_form_elo = m4_form_elo,
    m5_form_elo_q = m5_form_elo_q
  ),
  "outputs/models/logit_models_main.rds"
)
# ================================================================
# 7) Out-of-sample: logloss + AUC at each step
# ================================================================

set.seed(123)
idx   <- sample(seq_len(nrow(dat_model)), floor(0.3 * nrow(dat_model)))
train <- dat_model[-idx, ]
test  <- dat_model[idx, ]

fit_train <- list(
  xg                    = glm(formula(m1_xg),         data = train, family = binomial()),
  xg_odds               = glm(formula(m2_xg_odds),    data = train, family = binomial()),
  xg_odds_form          = glm(formula(m3_form),       data = train, family = binomial()),
  xg_odds_form_elo      = glm(formula(m4_form_elo),   data = train, family = binomial()),
  xg_odds_form_elo_qual = glm(formula(m5_form_elo_q), data = train, family = binomial())
)

oos_summary <- tibble::tibble(
  model   = names(fit_train),
  logloss = NA_real_,
  auc     = NA_real_
)

for (i in seq_along(fit_train)) {
  mname <- names(fit_train)[i]
  fit   <- fit_train[[i]]
  p_hat <- predict(fit, newdata = test, type = "response")
  
  oos_summary$logloss[oos_summary$model == mname] <- logloss(p_hat, test$home_win)
  oos_summary$auc[oos_summary$model == mname]     <- auc(p_hat, test$home_win)
}

cat("\n=== Out-of-sample comparison (logloss, AUC) ===\n")
print(oos_summary %>% dplyr::arrange(logloss))

# ================================================================
# 8) LASSO: xG + odds + form + Elo + player quality + team effects
# ================================================================

df_lasso <- dat_model %>%
  dplyr::select(
    home_win,
    xg_diff, p_home,
    home_xg_diff_form, away_xg_diff_form,
    home_win_form, away_win_form,
    elo_diff, quality_diff,
    home_team, away_team
  ) %>%
  dplyr::mutate(
    home_team = droplevels(factor(home_team)),
    away_team = droplevels(factor(away_team))
  )

X <- model.matrix(
  home_win ~ xg_diff + p_home +
    home_xg_diff_form + away_xg_diff_form +
    home_win_form + away_win_form +
    elo_diff + quality_diff +
    home_team + away_team,
  data = df_lasso
)[, -1]   # drop intercept

y <- df_lasso$home_win

cat("\nLASSO design matrix: rows =", nrow(X), " cols =", ncol(X), "\n")

set.seed(42)
cv_lasso <- glmnet::cv.glmnet(
  X, y,
  family = "binomial",
  alpha  = 1
)

best_lambda <- cv_lasso$lambda.min
cat("Best lambda (LASSO with team + quality) =", best_lambda, "\n")

lasso_best <- glmnet::glmnet(
  X, y,
  family = "binomial",
  alpha  = 1,
  lambda = best_lambda
)

nz <- coef(lasso_best)
nz_df <- as.matrix(nz) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("feature") %>%
  dplyr::rename(coef = 2) %>%
  dplyr::mutate(coef = as.numeric(coef)) %>%
  dplyr::filter(!is.na(coef), coef != 0) %>%
  dplyr::arrange(dplyr::desc(abs(coef)))

cat("\nLASSO (with Elo + player quality + team effects) — non-zero coefficients (top 30):\n")
print(head(nz_df, 30), row.names = FALSE)

# Save compact LASSO coefficient table for dissertation
# Save compact LASSO coefficient table for dissertation
main_vars <- c(
  "xg_diff",
  "p_home",
  "home_xg_diff_form",
  "away_xg_diff_form",
  "home_win_form",
  "away_win_form",
  "elo_diff",
  "quality_diff"
)

lasso_main_table <- tibble::tibble(
  feature = main_vars
) %>%
  dplyr::left_join(
    nz_df %>% dplyr::select(feature, coef),
    by = "feature"
  ) %>%
  dplyr::mutate(
    selected_by_lasso = ifelse(is.na(coef), "No", "Yes"),
    lasso_coef_raw = coef,
    coef = ifelse(is.na(coef), 0, coef),
    Variable = dplyr::recode(
      feature,
      "xg_diff" = "xG difference",
      "p_home" = "Implied home probability",
      "home_xg_diff_form" = "Home recent xG difference",
      "away_xg_diff_form" = "Away recent xG difference",
      "home_win_form" = "Home recent win rate",
      "away_win_form" = "Away recent win rate",
      "elo_diff" = "Elo difference",
      "quality_diff" = "Player quality difference"
    ),
    lasso_coef = dplyr::case_when(
      is.na(lasso_coef_raw) ~ "0",
      abs(lasso_coef_raw) < 0.001 ~ "<0.001",
      TRUE ~ sprintf("%.3f", lasso_coef_raw)
    )
  ) %>%
  dplyr::select(Variable, lasso_coef, selected_by_lasso)

team_effect_summary <- tibble::tibble(
  Variable = c("Home-team fixed effects retained", "Away-team fixed effects retained"),
  lasso_coef = c(
    as.character(sum(grepl("^home_team", nz_df$feature))),
    as.character(sum(grepl("^away_team", nz_df$feature)))
  ),
  selected_by_lasso = c("Count", "Count")
)

lasso_table_out <- dplyr::bind_rows(
  lasso_main_table,
  team_effect_summary
)

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(lasso_table_out, "outputs/tables/lasso_selection_summary.csv")

cat("\nSaved: outputs/tables/lasso_selection_summary.csv\n")

# ================================================================
# 9) GLM vs LASSO: out-of-sample comparison on full feature set
# ================================================================

set.seed(123)
idx2   <- sample(seq_len(nrow(dat_model)), floor(0.3 * nrow(dat_model)))
train2 <- dat_model[-idx2, ]
test2  <- dat_model[idx2, ]

# GLM with all features (same as m5_form_elo_q, but refit on train2)
glm_tr <- glm(
  home_win ~ xg_diff + p_home +
    home_goal_diff_form + away_goal_diff_form +
    home_win_form       + away_win_form +
    elo_diff + quality_diff,
  data   = train2,
  family = binomial()
)

p_glm_te <- predict(glm_tr, newdata = test2, type = "response")

# LASSO on train2
X_tr <- model.matrix(
  home_win ~ xg_diff + p_home +
    home_xg_diff_form + away_xg_diff_form +
    home_win_form + away_win_form +
    elo_diff + quality_diff +
    home_team + away_team,
  data = train2
)[, -1]

y_tr <- train2$home_win

X_te <- model.matrix(
  home_win ~ xg_diff + p_home +
    home_xg_diff_form + away_xg_diff_form +
    home_win_form + away_win_form +
    elo_diff + quality_diff +
    home_team + away_team,
  data = test2
)[, -1]

y_te <- test2$home_win

set.seed(123)
cv_lasso_te <- glmnet::cv.glmnet(
  X_tr, y_tr,
  family = "binomial",
  alpha  = 1
)

p_lasso_te <- predict(
  cv_lasso_te,
  newx = X_te,
  s    = "lambda.min",
  type = "response"
)

res_eval <- tibble::tibble(
  model   = c("GLM: xg+odds+form+Elo+players",
              "LASSO: xg+odds+form+Elo+players+teams"),
  logloss = c(
    logloss(p_glm_te,            y_te),
    logloss(as.numeric(p_lasso_te), y_te)
  ),
  auc     = c(
    auc(p_glm_te,                y_te),
    auc(as.numeric(p_lasso_te),  y_te)
  )
)

cat("\nOut-of-sample performance (GLM vs LASSO, full feature set):\n")
print(res_eval)

# ================================================================
# 10) Visualise effect of adding player quality (final GLM)
# ================================================================

dat_model$pred_prob_final <- predict(m5_form_elo_q, newdata = dat_model, type = "response")

p <- ggplot(
  dat_model,
  aes(
    x      = xg_diff,
    y      = pred_prob_final,
    colour = p_home
  )
) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE, colour = "black") +
  viridis::scale_colour_viridis(
    name   = "Implied Home Prob (Odds)",
    option = "C"
  ) +
  labs(
    title    = "Predicted Home Win Probability vs xG Difference",
    subtitle = "GLM: xG + odds + form + Elo + player-based team quality",
    x        = "xG Difference (Home - Away)",
    y        = "Predicted P(Home Win)"
  ) +
  theme_minimal(base_size = 12)

print(p)

ggsave(
  file.path(fig_dir, "pred_prob_xg_colored_by_odds_players.png"),
  p,
  width  = 9,
  height = 6,
  dpi    = 300,
  bg     = "white"
)

cat("\nSaved figure: outputs/figures/pred_prob_xg_colored_by_odds_players.png\n")
