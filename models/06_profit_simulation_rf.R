# ================================================================
# 06_profit_simulation.R
# Betting simulation using RF walk-forward predictions from 05
# Compares profitability across form windows
# ================================================================

needed <- c("dplyr","readr","janitor","lubridate","stringi","tibble","ggplot2","scales")
new <- setdiff(needed, rownames(installed.packages()))
if(length(new)) install.packages(new, repos="https://cloud.r-project.org")
invisible(lapply(needed, library, character.only = TRUE))

set.seed(123)

data_path <- file.path("data","processed","epl_matches_with_xg_odds.csv")
pred_path <- file.path("outputs","tables","rf_walkforward_preds.csv")
out_dir   <- file.path("outputs","tables")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

MIN_P <- 0.05
MAX_P <- 0.95
EDGE_THRESHOLD <- 0.06   # use your preferred threshold here

# -------------------------------
# Helpers
# -------------------------------
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
  x <- sub("^newcastle utd$", "newcastle", x)
  x <- sub("^newcastle united$", "newcastle", x)
  x <- sub("^nottingham forest$", "nottm forest", x)
  x <- sub("^sheffield united$", "sheffield utd", x)
  x <- sub("^leicester city$", "leicester", x)
  x <- sub("^leeds united$", "leeds", x)
  x <- sub("^norwich city$", "norwich", x)
  x
}

find_home_odds_col <- function(nms) {
  patterns <- c(
    "^b365h$", "^bwh$", "^psh$", "^whh$", "^vch$", "^iwh$", "^lbh$", "^sbh$",
    "^o_h$", "^home_odds$", "^odds_home$", "^h_odds$"
  )
  for (pat in patterns) {
    hit <- grep(pat, nms, ignore.case = TRUE, value = TRUE)
    if (length(hit) > 0) return(hit[1])
  }
  NA_character_
}

profit_unit <- function(y, odds_dec) ifelse(y == 1, odds_dec - 1, -1)

logloss_vec <- function(p, y) {
  p <- pmin(pmax(p, 1e-12), 1 - 1e-12)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

brier_vec <- function(p, y) mean((p - y)^2)

# -------------------------------
# 1) Load predictions from 05
# -------------------------------
stopifnot(file.exists(pred_path))
preds <- readr::read_csv(pred_path, show_col_types = FALSE) %>%
  janitor::clean_names() %>%
  mutate(
    date = as.Date(date),
    home_team = std_key(home_team),
    away_team = std_key(away_team)
  )

# -------------------------------
# 2) Load odds data
# -------------------------------
stopifnot(file.exists(data_path))
dat <- readr::read_csv(data_path, show_col_types = FALSE) %>%
  janitor::clean_names() %>%
  mutate(
    date = as.Date(date),
    home_team = std_key(home_team),
    away_team = std_key(away_team)
  )

odds_col <- find_home_odds_col(names(dat))
if (is.na(odds_col)) {
  stop("Could not find home-win decimal odds column.")
}

odds_df <- dat %>%
  select(date, home_team, away_team, all_of(odds_col)) %>%
  rename(home_odds = all_of(odds_col))

# -------------------------------
# 3) Merge predictions with odds
# -------------------------------
bet_df <- preds %>%
  left_join(odds_df, by = c("date","home_team","away_team"))

# -------------------------------
# 4) Profit simulation by form_n and season
# -------------------------------
by_season <- bet_df %>%
  mutate(
    edge = p_model - p_home,
    place_bet = (edge > EDGE_THRESHOLD) &
      (p_model >= MIN_P) & (p_model <= MAX_P) &
      is.finite(home_odds) & home_odds > 1,
    profit = ifelse(place_bet, profit_unit(actual_home_win, home_odds), 0),
    stake = ifelse(place_bet, 1, 0)
  ) %>%
  group_by(form_n, test_season) %>%
  summarise(
    n_bets = sum(place_bet),
    hit_rate = ifelse(sum(place_bet) > 0, mean(actual_home_win[place_bet] == 1), NA_real_),
    avg_edge = ifelse(sum(place_bet) > 0, mean(edge[place_bet]), NA_real_),
    avg_odds = ifelse(sum(place_bet) > 0, mean(home_odds[place_bet]), NA_real_),
    total_profit = sum(profit),
    total_stake = sum(stake),
    roi = ifelse(sum(stake) > 0, sum(profit) / sum(stake), NA_real_),
    logloss = logloss_vec(p_model, actual_home_win),
    brier = brier_vec(p_model, actual_home_win),
    .groups = "drop"
  ) %>%
  arrange(form_n, test_season)

readr::write_csv(by_season, file.path(out_dir, "rf_profit_by_form_by_season.csv"))

# -------------------------------
# 5) Summary by form_n
# -------------------------------
summary_by_form <- by_season %>%
  group_by(form_n) %>%
  summarise(
    n_seasons = n(),
    total_bets = sum(n_bets, na.rm = TRUE),
    total_profit = sum(total_profit, na.rm = TRUE),
    total_stake = sum(total_stake, na.rm = TRUE),
    roi = ifelse(total_stake > 0, total_profit / total_stake, NA_real_),
    mean_roi = mean(roi, na.rm = TRUE),
    mean_hit_rate = mean(hit_rate, na.rm = TRUE),
    mean_logloss = mean(logloss, na.rm = TRUE),
    mean_brier = mean(brier, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(form_n)

print(summary_by_form)

readr::write_csv(summary_by_form, file.path(out_dir, "rf_profit_summary_by_form.csv"))