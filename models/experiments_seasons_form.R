# ================================================================
# EXPERIMENTS: Binary LR (Home Win)
# Compare: seasons (3/5/8 most recent) x form window (5/10/15)
# Features: xG diff + odds + form + Elo diff + player quality
# Evaluation: Temporal split (70/30), LogLoss + AUC
# Output: outputs/tables/binary_experiments_seasons_form.csv
# ================================================================

# ---- Packages ----
needed <- c(
  "dplyr","readr","tibble","tidyr","stringr","lubridate",
  "janitor","slider","data.table","stringi","worldfootballR"
)
new <- setdiff(needed, rownames(installed.packages()))
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
invisible(lapply(needed, library, character.only = TRUE))

# ---- Paths ----
processed_path <- file.path("data", "processed", "epl_matches_with_xg_odds.csv")
elo_path       <- file.path("data", "elo", "clubelo_all.csv")

if (!file.exists(processed_path)) stop("Missing: ", processed_path)
if (!file.exists(elo_path))       stop("Missing: ", elo_path)

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# ================================================================
# Helper functions
# ================================================================

std_key <- function(x) {
  x <- tolower(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- gsub("\\(.*?\\)|\\[.*?\\]", "", x)
  x <- gsub("[^a-z0-9 ]", " ", x)
  x <- gsub("\\b(fc|afc|cf|sc|club|u\\d{2})\\b", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  
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

pseudo_r2 <- function(m) 1 - (m$deviance / m$null.deviance)

logloss <- function(p, y) {
  p <- pmin(pmax(p, 1e-12), 1 - 1e-12)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

auc <- function(p, y) {
  o <- order(p)
  p <- p[o]
  y <- y[o]
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  ranks <- rank(p)
  (sum(ranks[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# ---- leakage-free rolling mean over PREVIOUS k matches ----
rolling_mean_prev_k <- function(x, k) {
  slider::slide_dbl(dplyr::lag(x), mean, .before = k - 1, .complete = TRUE)
}

add_form_features <- function(df, k = 5) {
  stopifnot(all(c("date","home_team","away_team","home_goals","away_goals","home_xg","away_xg") %in% names(df)))
  
  home_form <- df %>%
    dplyr::group_by(home_team) %>%
    dplyr::arrange(date, .by_group = TRUE) %>%
    dplyr::mutate(
      home_xg_diff_form   = rolling_mean_prev_k(home_xg - away_xg, k),
      home_goal_diff_form = rolling_mean_prev_k(home_goals - away_goals, k),
      home_win_form       = rolling_mean_prev_k(as.integer(home_goals > away_goals), k)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, home_team, home_xg_diff_form, home_goal_diff_form, home_win_form)
  
  away_form <- df %>%
    dplyr::group_by(away_team) %>%
    dplyr::arrange(date, .by_group = TRUE) %>%
    dplyr::mutate(
      away_xg_diff_form   = rolling_mean_prev_k(away_xg - home_xg, k),
      away_goal_diff_form = rolling_mean_prev_k(away_goals - home_goals, k),
      away_win_form       = rolling_mean_prev_k(as.integer(away_goals > home_goals), k)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, away_team, away_xg_diff_form, away_goal_diff_form, away_win_form)
  
  df %>%
    dplyr::left_join(home_form, by = c("date","home_team")) %>%
    dplyr::left_join(away_form, by = c("date","away_team"))
}

# ---- Elo rolling join (last on/before, fallback next) ----
merge_elo <- function(df, elo_path) {
  elo_raw <- readr::read_csv(elo_path, show_col_types = FALSE)
  
  club_col <- grep("club|team", names(elo_raw), ignore.case = TRUE, value = TRUE)[1]
  elo_col  <- grep("elo",       names(elo_raw), ignore.case = TRUE, value = TRUE)[1]
  from_col <- grep("^from$",    names(elo_raw), ignore.case = TRUE, value = TRUE)[1]
  stopifnot(!is.na(club_col), !is.na(elo_col), !is.na(from_col))
  
  parse_any_date <- function(x) suppressWarnings(dplyr::coalesce(lubridate::ymd(x), lubridate::dmy(x), lubridate::mdy(x)))
  
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
  
  df <- df %>%
    dplyr::mutate(
      home_key = std_key(as.character(home_team)),
      away_key = std_key(as.character(away_team))
    )
  
  elo_dt <- data.table::as.data.table(elo_all)
  data.table::setkey(elo_dt, team_key, date)
  
  dt <- data.table::as.data.table(df)
  
  dt[, home_elo := elo_dt[.SD, on = .(team_key = home_key, date = date), roll = Inf,  x.elo], .SDcols = c("home_key","date")]
  dt[, home_elo_next := elo_dt[.SD, on = .(team_key = home_key, date = date), roll = -Inf, x.elo], .SDcols = c("home_key","date")]
  dt[, home_elo := data.table::fifelse(is.na(home_elo), home_elo_next, home_elo)]
  dt[, home_elo_next := NULL]
  
  dt[, away_elo := elo_dt[.SD, on = .(team_key = away_key, date = date), roll = Inf,  x.elo], .SDcols = c("away_key","date")]
  dt[, away_elo_next := elo_dt[.SD, on = .(team_key = away_key, date = date), roll = -Inf, x.elo], .SDcols = c("away_key","date")]
  dt[, away_elo := data.table::fifelse(is.na(away_elo), away_elo_next, away_elo)]
  dt[, away_elo_next := NULL]
  
  out <- as.data.frame(dt) %>%
    dplyr::mutate(elo_diff = home_elo - away_elo)
  
  out
}

# ---- Player quality by season (worldfootballR_data) ----
build_player_quality <- function(season_vec) {
  players_raw <- worldfootballR::load_fb_big5_advanced_season_stats(
    season_end_year = season_vec,
    stat_type       = "standard",
    team_or_player  = "player"
  )
  
  players_team <- players_raw %>%
    janitor::clean_names() %>%
    dplyr::filter(comp == "Premier League") %>%
    dplyr::mutate(
      team_key = std_key(squad),
      nineties = mins_per_90_playing
    ) %>%
    dplyr::filter(!is.na(season_end_year), nineties > 0) %>%
    dplyr::group_by(season_end_year, team_key) %>%
    dplyr::summarise(
      total_nineties = sum(nineties, na.rm = TRUE),
      total_xg       = sum(x_g_expected,  na.rm = TRUE),
      total_xag      = sum(x_ag_expected, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      xg_per90     = total_xg  / total_nineties,
      xag_per90    = total_xag / total_nineties,
      team_quality = as.numeric(scale(xg_per90 + xag_per90))
    )
  
  players_team
}

# ---- Build modelling dataset for a given seasons subset + form window ----
build_dat_model <- function(dat_all, seasons_keep, k_form, elo_path) {
  df <- dat_all %>%
    dplyr::filter(season_end_year %in% seasons_keep) %>%
    dplyr::arrange(date)
  
  # add leakage-free form
  df <- add_form_features(df, k = k_form)
  
  # add Elo diff
  df <- merge_elo(df, elo_path = elo_path)
  
  # player quality per season
  players_team <- build_player_quality(sort(unique(df$season_end_year)))
  
  # modelling vars needed before player merge
  base_vars <- c(
    "home_win","xg_diff","p_home",
    "home_goal_diff_form","away_goal_diff_form",
    "home_win_form","away_win_form",
    "home_xg_diff_form","away_xg_diff_form",
    "elo_diff"
  )
  
  df_cc <- df %>%
    tidyr::drop_na(dplyr::all_of(base_vars)) %>%
    dplyr::mutate(
      home_team_key = std_key(as.character(home_team)),
      away_team_key = std_key(as.character(away_team))
    )
  
  df_cc <- df_cc %>%
    dplyr::left_join(players_team %>% dplyr::select(season_end_year, team_key, team_quality),
                     by = c("season_end_year" = "season_end_year", "home_team_key" = "team_key")) %>%
    dplyr::rename(home_team_quality = team_quality) %>%
    dplyr::left_join(players_team %>% dplyr::select(season_end_year, team_key, team_quality),
                     by = c("season_end_year" = "season_end_year", "away_team_key" = "team_key")) %>%
    dplyr::rename(away_team_quality = team_quality) %>%
    dplyr::mutate(quality_diff = home_team_quality - away_team_quality)
  
  model_vars <- c(
    "home_win",
    "xg_diff","p_home",
    "home_goal_diff_form","away_goal_diff_form",
    "home_win_form","away_win_form",
    "elo_diff","quality_diff"
  )
  
  df_model <- df_cc %>%
    dplyr::filter(stats::complete.cases(dplyr::across(dplyr::all_of(model_vars)))) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      home_team = factor(home_team),
      away_team = factor(away_team)
    )
  
  df_model
}

# ---- Fit stepwise GLMs and evaluate on temporal split ----
fit_eval_stepwise <- function(df_model) {
  df_model <- df_model %>% dplyr::arrange(date)
  
  split_point <- floor(0.7 * nrow(df_model))
  train <- df_model[1:split_point, ]
  test  <- df_model[(split_point + 1):nrow(df_model), ]
  
  # Stepwise specifications
  m1 <- glm(home_win ~ xg_diff, data = train, family = binomial())
  m2 <- glm(home_win ~ xg_diff + p_home, data = train, family = binomial())
  m3 <- glm(home_win ~ xg_diff + p_home +
              home_goal_diff_form + away_goal_diff_form +
              home_win_form + away_win_form,
            data = train, family = binomial())
  m4 <- glm(home_win ~ xg_diff + p_home +
              home_goal_diff_form + away_goal_diff_form +
              home_win_form + away_win_form +
              elo_diff,
            data = train, family = binomial())
  m5 <- glm(home_win ~ xg_diff + p_home +
              home_goal_diff_form + away_goal_diff_form +
              home_win_form + away_win_form +
              elo_diff + quality_diff,
            data = train, family = binomial())
  
  models <- list(
    xg = m1,
    xg_odds = m2,
    xg_odds_form = m3,
    xg_odds_form_elo = m4,
    xg_odds_form_elo_quality = m5
  )
  
  # Evaluate
  out <- purrr::imap_dfr(models, function(m, name) {
    p <- predict(m, newdata = test, type = "response")
    tibble::tibble(
      model = name,
      n_train = nrow(train),
      n_test  = nrow(test),
      logloss = logloss(p, test$home_win),
      auc     = auc(p, test$home_win),
      AIC_train = AIC(m),
      pseudo_r2_train = pseudo_r2(m)
    )
  })
  
  out
}

# ================================================================
# 0) Load base merged dataset (xG + goals + odds)
# ================================================================
dat_all <- readr::read_csv(processed_path, show_col_types = FALSE) %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    date = as.Date(date),
    season_end_year = dplyr::if_else(
      lubridate::month(date) >= 7L,
      lubridate::year(date) + 1L,
      lubridate::year(date)
    ),
    home_win = as.integer(home_goals > away_goals),
    xg_diff  = home_xg - away_xg
  ) %>%
  dplyr::arrange(date)

# Most recent seasons available in file
season_levels <- sort(unique(dat_all$season_end_year))
season_levels <- season_levels[!is.na(season_levels)]
cat("Seasons available in merged data:", paste(season_levels, collapse = ", "), "\n")

# ================================================================
# RUN EXPERIMENTS
# ================================================================

# most recent N seasons (recommended)
n_seasons_grid <- c(3, 5, 8)
k_form_grid    <- c(5, 10, 15)

results <- tibble::tibble()

for (n_seasons in n_seasons_grid) {
  seasons_keep <- tail(season_levels, n_seasons)
  
  for (k_form in k_form_grid) {
    cat("\n--- Running: n_seasons =", n_seasons,
        " seasons_keep =", paste(seasons_keep, collapse = ","),
        " k_form =", k_form, "---\n")
    
    df_model <- build_dat_model(
      dat_all      = dat_all,
      seasons_keep = seasons_keep,
      k_form       = k_form,
      elo_path     = elo_path
    )
    
    cat("Rows in df_model:", nrow(df_model), "\n")
    
    # guard against too-small data after complete-case filtering
    if (nrow(df_model) < 300) {
      warning("Too few rows after filtering (", nrow(df_model), "). Skipping this setting.")
      next
    }
    
    res <- fit_eval_stepwise(df_model) %>%
      dplyr::mutate(
        n_seasons = n_seasons,
        seasons_keep = paste(seasons_keep, collapse = ","),
        k_form = k_form
      )
    
    results <- dplyr::bind_rows(results, res)
  }
}

# Save results
out_path <- "outputs/tables/binary_experiments_seasons_form.csv"
readr::write_csv(results, out_path)

cat("\n✅ Saved experiment results to:", out_path, "\n")

# Also print a compact “best models by logloss” view
cat("\nTop 15 rows by logloss:\n")
print(results %>% dplyr::arrange(logloss) %>% dplyr::select(n_seasons, k_form, model, logloss, auc) %>% head(15))
