# ================================================================
# 05_random_forest_binary_walkforward.R
# Walk-forward / forward-test evaluation for RF (ranger)
# Train on past seasons, test on the next season.
# Saves:
#   1) outputs/tables/rf_walkforward_results.csv
#   2) outputs/tables/rf_walkforward_summary_by_form.csv
#   3) outputs/tables/rf_walkforward_preds.csv
# ================================================================

needed <- c(
  "dplyr", "readr", "janitor", "lubridate", "stringi", "slider",
  "data.table", "ranger", "yardstick", "tibble", "ggplot2"
)
new <- setdiff(needed, rownames(installed.packages()))
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
invisible(lapply(needed, library, character.only = TRUE))

set.seed(123)

# -------------------------------
# Settings
# -------------------------------
data_path <- file.path("data", "processed", "epl_matches_with_xg_odds.csv")
elo_path  <- file.path("data", "elo", "clubelo_all.csv")

out_table_dir <- file.path("outputs", "tables")
dir.create(out_table_dir, recursive = TRUE, showWarnings = FALSE)

SEASONS_END_ALL   <- 2018:2025
TRAIN_WINDOW      <- 5      # set to Inf for expanding window
MIN_TRAIN_SEASONS <- 3
FORM_GRID         <- c(5, 10, 15)

RF_NUM_TREES <- 1000
RF_MIN_NODE  <- 10

# -------------------------------
# Helpers
# -------------------------------
rolling_mean <- function(x, n = 5) {
  slider::slide_dbl(x, mean, .before = n - 1, .complete = TRUE)
}

std_key <- function(x) {
  x <- tolower(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- gsub("\\(.*?\\)|\\[.*?\\]", "", x)
  x <- gsub("[^a-z0-9 ]", " ", x)
  x <- gsub("\\b(fc|afc|cf|sc|club|u\\d{2})\\b", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  
  x <- sub("^manchester united$", "man united", x)
  x <- sub("^manchester utd$", "man united", x)
  x <- sub("^manchester city$", "man city", x)
  x <- sub("^wolverhampton wanderers$", "wolves", x)
  x <- sub("^tottenham hotspur$", "tottenham", x)
  x <- sub("^brighton and hove albion$", "brighton", x)
  x <- sub("^west ham united$", "west ham", x)
  x <- sub("^newcastle utd$", "newcastle", x)
  x <- sub("^newcastle united$", "newcastle", x)
  x <- sub("^nottingham forest$", "nottm forest", x)
  x <- sub("^sheffield united$", "sheffield utd", x)
  x <- sub("^leicester city$", "leicester", x)
  x <- sub("^leeds united$", "leeds", x)
  x <- sub("^norwich city$", "norwich", x)
  x
}

logloss <- function(p, y) {
  p <- pmin(pmax(p, 1e-12), 1 - 1e-12)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

# -------------------------------
# 1) Load base match + odds data
# -------------------------------
stopifnot(file.exists(data_path))

dat0 <- readr::read_csv(data_path, show_col_types = FALSE) |>
  janitor::clean_names() |>
  mutate(
    date = as.Date(date),
    season_end_year = if_else(month(date) >= 7L, year(date) + 1L, year(date)),
    home_team = std_key(home_team),
    away_team = std_key(away_team),
    home_win = as.integer(home_goals > away_goals)
  ) |>
  filter(season_end_year %in% SEASONS_END_ALL) |>
  arrange(date)

cat("Rows after load + season filter:", nrow(dat0), "\n")

# -------------------------------
# 2) Elo merge
# -------------------------------
stopifnot(file.exists(elo_path))

elo_raw <- readr::read_csv(elo_path, show_col_types = FALSE)

club_col <- grep("club|team", names(elo_raw), ignore.case = TRUE, value = TRUE)[1]
elo_col  <- grep("elo", names(elo_raw), ignore.case = TRUE, value = TRUE)[1]
from_col <- grep("^from$", names(elo_raw), ignore.case = TRUE, value = TRUE)[1]

stopifnot(!is.na(club_col), !is.na(elo_col), !is.na(from_col))

parse_any_date <- function(x) {
  suppressWarnings(dplyr::coalesce(ymd(x), dmy(x), mdy(x)))
}

elo_all <- elo_raw |>
  transmute(
    team_raw = .data[[club_col]],
    date     = parse_any_date(.data[[from_col]]),
    elo      = as.numeric(.data[[elo_col]])
  ) |>
  filter(!is.na(team_raw), !is.na(date), !is.na(elo)) |>
  mutate(team_key = std_key(team_raw)) |>
  arrange(team_key, date) |>
  distinct()

elo_dt <- data.table::as.data.table(elo_all)
data.table::setkey(elo_dt, team_key, date)

dat_dt0 <- data.table::as.data.table(dat0)
dat_dt0[, home_key := std_key(home_team)]
dat_dt0[, away_key := std_key(away_team)]

dat_dt0[
  ,
  home_elo := elo_dt[.SD, on = .(team_key = home_key, date = date), roll = Inf, x.elo],
  .SDcols = c("home_key", "date")
]
dat_dt0[
  ,
  home_elo_next := elo_dt[.SD, on = .(team_key = home_key, date = date), roll = -Inf, x.elo],
  .SDcols = c("home_key", "date")
]
dat_dt0[, home_elo := fifelse(is.na(home_elo), home_elo_next, home_elo)]
dat_dt0[, home_elo_next := NULL]

dat_dt0[
  ,
  away_elo := elo_dt[.SD, on = .(team_key = away_key, date = date), roll = Inf, x.elo],
  .SDcols = c("away_key", "date")
]
dat_dt0[
  ,
  away_elo_next := elo_dt[.SD, on = .(team_key = away_key, date = date), roll = -Inf, x.elo],
  .SDcols = c("away_key", "date")
]
dat_dt0[, away_elo := fifelse(is.na(away_elo), away_elo_next, away_elo)]
dat_dt0[, away_elo_next := NULL]

dat_dt0[, elo_diff := home_elo - away_elo]
dat0 <- as.data.frame(dat_dt0)

cat("Missing elo_diff:", sum(is.na(dat0$elo_diff)), "\n")

# -------------------------------
# 3) Predictor set
# Removed xg_diff to avoid using current-match xG
# -------------------------------
feature_cols <- c(
  "p_home",
  "home_goal_diff_form", "away_goal_diff_form",
  "home_win_form", "away_win_form",
  "elo_diff"
)

# -------------------------------
# 4) One walk-forward run
# -------------------------------
run_one <- function(dat_base, form_n, train_seasons, test_season) {
  
  dat <- dat_base |>
    arrange(date)
  
  home_form <- dat |>
    group_by(home_team) |>
    arrange(date, .by_group = TRUE) |>
    mutate(
      home_goal_diff_form = rolling_mean(home_goals - away_goals, form_n),
      home_win_form       = rolling_mean(as.integer(home_goals > away_goals), form_n)
    ) |>
    ungroup() |>
    select(date, home_team, home_goal_diff_form, home_win_form)
  
  away_form <- dat |>
    group_by(away_team) |>
    arrange(date, .by_group = TRUE) |>
    mutate(
      away_goal_diff_form = rolling_mean(away_goals - home_goals, form_n),
      away_win_form       = rolling_mean(as.integer(away_goals > home_goals), form_n)
    ) |>
    ungroup() |>
    select(date, away_team, away_goal_diff_form, away_win_form)
  
  dat <- dat |>
    left_join(home_form, by = c("date", "home_team")) |>
    left_join(away_form, by = c("date", "away_team"))
  
  seasons_used <- c(train_seasons, test_season)
  
  n_before_model <- dat |>
    filter(season_end_year %in% seasons_used) |>
    nrow()
  
  df <- dat |>
    select(
      date, season_end_year, home_team, away_team, home_win,
      all_of(feature_cols)
    ) |>
    filter(season_end_year %in% seasons_used) |>
    filter(complete.cases(across(all_of(c("home_win", feature_cols))))) |>
    arrange(date)
  
  n_after_model <- nrow(df)
  n_removed <- n_before_model - n_after_model
  pct_removed <- round(100 * n_removed / n_before_model, 2)
  
  cat("Rows before complete-case filtering:", n_before_model, "\n")
  cat("Rows after complete-case filtering :", n_after_model, "\n")
  cat("Rows removed                      :", n_removed, "\n")
  cat("Percent removed                   :", pct_removed, "%\n")
  
  train <- df |> filter(season_end_year %in% train_seasons)
  test  <- df |> filter(season_end_year == test_season)
  
  if (nrow(train) == 0 || nrow(test) == 0) {
    return(NULL)
  }
  
  train_rf <- train |> mutate(home_win = factor(home_win, levels = c(0, 1)))
  test_rf  <- test  |> mutate(home_win = factor(home_win, levels = c(0, 1)))
  
  rf_fit <- ranger::ranger(
    formula = as.formula(paste("home_win ~", paste(feature_cols, collapse = " + "))),
    data = train_rf,
    probability = TRUE,
    num.trees = RF_NUM_TREES,
    mtry = floor(sqrt(length(feature_cols))),
    min.node.size = RF_MIN_NODE,
    importance = "permutation",
    seed = 123
  )
  
  p_hat <- predict(rf_fit, data = test_rf)$predictions[, "1"]
  
  y_test <- as.integer(as.character(test_rf$home_win))
  
  ll <- logloss(p_hat, y_test)
  auc <- yardstick::roc_auc_vec(
    truth = test_rf$home_win,
    estimate = p_hat,
    event_level = "second"
  )
  brier <- mean((p_hat - y_test)^2)
  
  metrics_out <- tibble::tibble(
    model = "rf_ranger",
    form_n = form_n,
    train_seasons = paste(range(train_seasons), collapse = "-"),
    test_season = test_season,
    n_train = nrow(train),
    n_test = nrow(test),
    logloss = ll,
    auc = as.numeric(auc),
    brier = brier
  )
  
  preds_out <- tibble::tibble(
    date = test$date,
    season_end_year = test$season_end_year,
    home_team = test$home_team,
    away_team = test$away_team,
    p_home = test$p_home,
    p_model = p_hat,
    actual_home_win = y_test,
    model = "rf_ranger",
    form_n = form_n,
    train_seasons = paste(range(train_seasons), collapse = "-"),
    test_season = test_season
  )
  
  list(
    metrics = metrics_out,
    preds = preds_out
  )
}

# -------------------------------
# 5) Run walk-forward
# -------------------------------
all_seasons <- sort(unique(dat0$season_end_year))
test_seasons <- all_seasons[all_seasons > min(all_seasons)]

results <- list()
pred_rows <- list()

for (form_n in FORM_GRID) {
  for (t in test_seasons) {
    
    if (is.infinite(TRAIN_WINDOW)) {
      train_seasons <- all_seasons[all_seasons < t]
    } else {
      train_seasons <- all_seasons[all_seasons < t]
      train_seasons <- tail(train_seasons, TRAIN_WINDOW)
    }
    
    if (length(train_seasons) < MIN_TRAIN_SEASONS) next
    
    cat(
      "Running RF | form =", form_n,
      "| train =", paste(train_seasons, collapse = ","),
      "| test =", t, "\n"
    )
    
    out <- run_one(dat0, form_n, train_seasons, t)
    
    if (!is.null(out)) {
      results[[length(results) + 1]] <- out$metrics
      pred_rows[[length(pred_rows) + 1]] <- out$preds
    }
  }
}

res_df <- dplyr::bind_rows(results) |>
  arrange(form_n, test_season)

pred_df <- dplyr::bind_rows(pred_rows) |>
  arrange(form_n, test_season, date, home_team, away_team)

print(res_df)

# -------------------------------
# 6) Save outputs
# -------------------------------
out_csv_results <- file.path(out_table_dir, "rf_walkforward_results.csv")
readr::write_csv(res_df, out_csv_results)
cat("\nSaved walk-forward results to:", out_csv_results, "\n")

summary_df <- res_df |>
  group_by(form_n) |>
  summarise(
    mean_logloss = mean(logloss),
    mean_auc = mean(auc),
    mean_brier = mean(brier),
    .groups = "drop"
  ) |>
  arrange(form_n)

print(summary_df)

out_csv_summary <- file.path(out_table_dir, "rf_walkforward_summary_by_form.csv")
readr::write_csv(summary_df, out_csv_summary)
cat("Saved summary to:", out_csv_summary, "\n")

out_csv_preds <- file.path(out_table_dir, "rf_walkforward_preds.csv")
readr::write_csv(pred_df, out_csv_preds)
cat("Saved match-level predictions to:", out_csv_preds, "\n")