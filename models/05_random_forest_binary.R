# ================================================================
# 05_random_forest_binary.R
# Random Forest (Binary): Home Win vs Not Home Win
# Mirrors the feature engineering used in your LR pipeline:
# xG diff, implied odds prob, recent form, Elo diff, player quality diff
# ================================================================

# -------------------------------
# 0) Packages
# -------------------------------
needed <- c(
  "dplyr","readr","janitor","lubridate","stringi","slider",
  "data.table","ranger","yardstick","tibble","ggplot2"
)

new <- setdiff(needed, rownames(installed.packages()))
if(length(new)) install.packages(new, repos = "https://cloud.r-project.org")
invisible(lapply(needed, library, character.only = TRUE))

# -------------------------------
# 1) Settings
# -------------------------------
set.seed(123)

data_path <- file.path("data", "processed", "epl_matches_with_xg_odds.csv")
elo_path  <- file.path("data", "elo", "clubelo_all.csv")

fig_dir <- file.path("writeup", "outputs", "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# Choose seasons + form window (make these easy to change)
SEASONS_END <- 2023:2025          # e.g. 3 seasons baseline
FORM_N      <- 5                 # try 5, 10, 15 etc.

# Train/test choice:
# (A) temporal split within the same seasons (default)
TEST_FRAC   <- 0.30

# (B) leave-one-season-out (advisor-friendly):
# TRAIN_END <- 2024
# TEST_END  <- 2025

# -------------------------------
# 2) Helper functions
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
  x <- gsub("\\s+", " ", x); x <- trimws(x)
  
  x <- sub("^manchester united$", "man united", x)
  x <- sub("^manchester utd$",   "man united", x)
  x <- sub("^manchester city$",  "man city",   x)
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

# Metrics
logloss <- function(p, y) {
  p <- pmin(pmax(p, 1e-12), 1 - 1e-12)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

# -------------------------------
# 3) Load base match+odds+xG data
# -------------------------------
stopifnot(file.exists(data_path))
dat <- readr::read_csv(data_path, show_col_types = FALSE) |>
  janitor::clean_names() |>
  mutate(
    date = as.Date(date),
    season_end_year = if_else(month(date) >= 7L, year(date) + 1L, year(date)),
    home_team = std_key(home_team),
    away_team = std_key(away_team),
    home_win  = as.integer(home_goals > away_goals),
    xg_diff   = home_xg - away_xg
  ) |>
  filter(season_end_year %in% SEASONS_END) |>
  arrange(date)

cat("Rows after load+season filter:", nrow(dat), "\n")

# -------------------------------
# 4) Recent form features (FORM_N)
# -------------------------------
home_form <- dat |>
  group_by(home_team) |>
  arrange(date, .by_group = TRUE) |>
  mutate(
    home_xg_diff_form   = rolling_mean(home_xg - away_xg, FORM_N),
    home_goal_diff_form = rolling_mean(home_goals - away_goals, FORM_N),
    home_win_form       = rolling_mean(as.integer(home_goals > away_goals), FORM_N)
  ) |>
  ungroup() |>
  select(date, home_team, home_xg_diff_form, home_goal_diff_form, home_win_form)

away_form <- dat |>
  group_by(away_team) |>
  arrange(date, .by_group = TRUE) |>
  mutate(
    away_xg_diff_form   = rolling_mean(away_xg - home_xg, FORM_N),
    away_goal_diff_form = rolling_mean(away_goals - home_goals, FORM_N),
    away_win_form       = rolling_mean(as.integer(away_goals > home_goals), FORM_N)
  ) |>
  ungroup() |>
  select(date, away_team, away_xg_diff_form, away_goal_diff_form, away_win_form)

dat <- dat |>
  left_join(home_form, by = c("date","home_team")) |>
  left_join(away_form, by = c("date","away_team"))

# -------------------------------
# 5) Elo merge -> elo_diff
# -------------------------------
stopifnot(file.exists(elo_path))
elo_raw <- readr::read_csv(elo_path, show_col_types = FALSE)

club_col <- grep("club|team", names(elo_raw), ignore.case = TRUE, value = TRUE)[1]
elo_col  <- grep("elo",       names(elo_raw), ignore.case = TRUE, value = TRUE)[1]
from_col <- grep("^from$",    names(elo_raw), ignore.case = TRUE, value = TRUE)[1]
stopifnot(!is.na(club_col), !is.na(elo_col), !is.na(from_col))

parse_any_date <- function(x) suppressWarnings(dplyr::coalesce(ymd(x), dmy(x), mdy(x)))

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

dat_dt <- data.table::as.data.table(dat) |>
  within({
    home_key <- std_key(home_team)
    away_key <- std_key(away_team)
  })

dat_dt[, home_elo := elo_dt[.SD, on=.(team_key=home_key, date=date), roll=Inf, x.elo], .SDcols=c("home_key","date")]
dat_dt[, home_elo_next := elo_dt[.SD, on=.(team_key=home_key, date=date), roll=-Inf, x.elo], .SDcols=c("home_key","date")]
dat_dt[, home_elo := fifelse(is.na(home_elo), home_elo_next, home_elo)]
dat_dt[, home_elo_next := NULL]

dat_dt[, away_elo := elo_dt[.SD, on=.(team_key=away_key, date=date), roll=Inf, x.elo], .SDcols=c("away_key","date")]
dat_dt[, away_elo_next := elo_dt[.SD, on=.(team_key=away_key, date=date), roll=-Inf, x.elo], .SDcols=c("away_key","date")]
dat_dt[, away_elo := fifelse(is.na(away_elo), away_elo_next, away_elo)]
dat_dt[, away_elo_next := NULL]

dat_dt[, elo_diff := home_elo - away_elo]

dat <- as.data.frame(dat_dt)

cat("Missing elo_diff:", sum(is.na(dat$elo_diff)), "\n")

# -------------------------------
# 6) Player-quality merge (optional)
# -------------------------------
# If you already created a team-quality table elsewhere, read it in here.
# Expected columns: season_end_year, team_key, team_quality
# Then compute quality_diff.
#
# Example (uncomment if you saved it):
# qual_path <- "data/derived/team_quality_by_season.csv"
# qual <- read_csv(qual_path, show_col_types = FALSE) |> mutate(team_key = std_key(team_key))
# dat <- dat |>
#   left_join(qual, by=c("season_end_year","home_team"="team_key")) |>
#   rename(home_team_quality = team_quality) |>
#   left_join(qual, by=c("season_end_year","away_team"="team_key")) |>
#   rename(away_team_quality = team_quality) |>
#   mutate(quality_diff = home_team_quality - away_team_quality)

# For now, assume quality_diff already exists in your merged csv OR skip it
# dat$quality_diff <- NA_real_

# -------------------------------
# 7) Build modelling frame
# -------------------------------
feature_cols <- c(
  "xg_diff","p_home",
  "home_goal_diff_form","away_goal_diff_form",
  "home_win_form","away_win_form",
  "elo_diff"
  # ,"quality_diff"
)

df <- dat |>
  select(date, season_end_year, home_win, all_of(feature_cols)) |>
  filter(complete.cases(across(all_of(c("home_win", feature_cols))))) |>
  arrange(date)

cat("Rows usable for RF:", nrow(df), "\n")

# -------------------------------
# 8) Train/Test split (temporal)
# -------------------------------
# Option A: temporal split within chosen seasons
n <- nrow(df)
cut <- floor((1 - TEST_FRAC) * n)
train <- df[1:cut, ]
test  <- df[(cut+1):n, ]

cat("Train rows:", nrow(train), " Test rows:", nrow(test), "\n")

# (Option B) Leave-one-season-out:
# train <- df |> filter(season_end_year <= TRAIN_END)
# test  <- df |> filter(season_end_year == TEST_END)

# -------------------------------
# 9) Fit Random Forest (ranger)
# -------------------------------
# ranger expects outcome as factor for classification
train_rf <- train |> mutate(home_win = factor(home_win, levels=c(0,1)))
test_rf  <- test  |> mutate(home_win = factor(home_win,  levels=c(0,1)))

rf_fit <- ranger::ranger(
  formula = as.formula(paste("home_win ~", paste(feature_cols, collapse = " + "))),
  data    = train_rf,
  probability = TRUE,
  num.trees   = 1000,
  mtry        = floor(sqrt(length(feature_cols))),
  min.node.size = 10,
  importance  = "permutation",
  seed        = 123
)

print(rf_fit)

# -------------------------------
# 10) Predict + Evaluate
# -------------------------------
p_hat <- predict(rf_fit, data = test_rf)$predictions[, "1"]

ll <- logloss(p_hat, as.integer(as.character(test_rf$home_win)))
roc_auc <- yardstick::roc_auc_vec(truth = test_rf$home_win, estimate = p_hat, event_level = "second")

cat("\n=== Random Forest (Binary) Performance ===\n")
cat("Logloss:", round(ll, 4), "\n")
cat("AUC    :", round(roc_auc, 4), "\n")

# Variable importance
imp <- tibble::tibble(
  feature = names(rf_fit$variable.importance),
  importance = as.numeric(rf_fit$variable.importance)
) |> arrange(desc(importance))

print(imp)

# Plot importance
p_imp <- ggplot(imp, aes(x=reorder(feature, importance), y=importance)) +
  geom_col() +
  coord_flip() +
  labs(title="Random Forest Feature Importance (Permutation)", x=NULL, y="Importance") +
  theme_minimal()

ggsave(file.path(fig_dir, "rf_feature_importance.png"), p_imp, width=8, height=5, dpi=300, bg="white")

cat("\nSaved:", file.path(fig_dir, "rf_feature_importance.png"), "\n")
