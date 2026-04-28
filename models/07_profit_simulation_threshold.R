library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

# --- Paths ---
odds_path <- file.path("data", "processed", "epl_matches_with_xg_odds.csv")
pred_path <- file.path("outputs", "tables", "rf_walkforward_preds.csv")  
out_csv   <- file.path("outputs", "tables", "rf_profit_threshold_sweep.csv")
out_fig   <- file.path("outputs", "figures", "rf_profit_threshold_sweep.png")

stopifnot(file.exists(odds_path), file.exists(pred_path))

# --- Load odds/xG file ---
odds <- readr::read_csv(odds_path, show_col_types = FALSE) %>%
  mutate(
    date = as.Date(date),
    home_win = as.integer(home_goals > away_goals)
  )

pred <- readr::read_csv(pred_path, show_col_types = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  select(season_end_year, date, home_team, away_team, p_model)

df <- odds %>%
  inner_join(
    pred,
    by = c("season_end_year", "date", "home_team", "away_team")
  )

# Sanity checks
stopifnot(all(c("p_home","o_h","home_win","p_model") %in% names(df)))

# --- Compute EV and profit for a £1 stake home bet ---
df <- df %>%
  mutate(
    p_market = p_home,
    edge     = p_model - p_market,
    ev       = p_model * o_h - 1,                 # expected profit per £1 stake
    profit   = ifelse(home_win == 1, o_h - 1, -1) # realised profit per £1 stake
  )

# --- Threshold grid ---
tau_grid <- c(0, 0.01, 0.02, 0.05, 0.08, 0.10)

# Choose ONE main rule:
# rule = "ev"   -> bet if ev > tau
# rule = "edge" -> bet if edge > tau
rule <- "ev"

res <- lapply(tau_grid, function(tau){
  
  bet_flag <- if (rule == "ev") (df$ev > tau) else (df$edge > tau)
  
  tmp <- df[bet_flag, , drop = FALSE]
  
  total_bets  <- nrow(tmp)
  total_stake <- total_bets * 1
  
  total_profit <- if (total_bets == 0) 0 else sum(tmp$profit, na.rm = TRUE)
  roi          <- if (total_stake == 0) NA_real_ else total_profit / total_stake
  hit_rate     <- if (total_bets == 0) NA_real_ else mean(tmp$home_win == 1)
  
  mean_ev      <- if (total_bets == 0) NA_real_ else mean(tmp$ev)
  mean_edge    <- if (total_bets == 0) NA_real_ else mean(tmp$edge)
  
  tibble::tibble(
    rule = rule,
    tau = tau,
    total_bets = total_bets,
    total_profit = total_profit,
    total_stake = total_stake,
    roi = roi,
    hit_rate = hit_rate,
    mean_ev = mean_ev,
    mean_edge = mean_edge
  )
}) %>% bind_rows()

# Save results
dir.create(file.path("outputs","tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("outputs","figures"), recursive = TRUE, showWarnings = FALSE)

readr::write_csv(res, out_csv)

# Plot ROI vs threshold
p <- ggplot(res, aes(x = tau, y = roi)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Random Forest betting strategy: ROI vs threshold",
    subtitle = paste("Rule:", rule, "| Home-win bets | £1 flat stake"),
    x = "Threshold (tau)",
    y = "ROI (profit / stake)"
  ) +
  theme_minimal()

ggsave(out_fig, p, width = 7.5, height = 4.5, dpi = 300, bg = "white")

print(res)
cat("\nSaved table:", out_csv, "\nSaved plot:", out_fig, "\n")