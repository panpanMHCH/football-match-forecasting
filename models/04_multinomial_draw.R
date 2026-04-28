# ================================================================
# Multinomial Model: Home / Draw / Away
# Uses same merged CSV: data/processed/epl_matches_with_xg_odds.csv
# ================================================================

# ---- Packages ----
needed <- c(
  "dplyr", "readr", "tibble", "tidyr", "stringr", "lubridate",
  "ggplot2", "janitor", "slider", "nnet"
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

rolling_mean <- function(x, n = 5) {
  slider::slide_dbl(x, mean, .before = n - 1, .complete = TRUE)
}

multi_logloss <- function(P, y, class_levels = c("H", "D", "A")) {
  # P: matrix n x K, rows are probabilities
  # y: factor with levels matching class_levels
  if (!is.matrix(P)) P <- as.matrix(P)
  if (!all(class_levels %in% colnames(P)) && is.null(colnames(P))) {
    # assume columns are in order of class_levels
    colnames(P) <- class_levels
  }
  y <- factor(y, levels = class_levels)
  idx <- match(y, class_levels)
  p_true <- P[cbind(seq_len(nrow(P)), idx)]
  p_true <- pmin(pmax(p_true, 1e-15), 1 - 1e-15)
  -mean(log(p_true))
}

accuracy_3way <- function(P, y, class_levels = c("H","D","A")) {
  if (!is.matrix(P)) P <- as.matrix(P)
  if (is.null(colnames(P))) {
    colnames(P) <- class_levels
  }
  y <- factor(y, levels = class_levels)
  pred_class <- colnames(P)[max.col(P, ties.method = "first")]
  mean(pred_class == as.character(y))
}

# ================================================================
# 1) Load merged CSV (goals + xG + odds)
# ================================================================

processed_path <- file.path("data", "processed", "epl_matches_with_xg_odds.csv")
if (!file.exists(processed_path)) {
  stop(
    "File not found: ", processed_path,
    "\nPlease run your odds/xG merge script first."
  )
}

dat <- readr::read_csv(processed_path, show_col_types = FALSE) %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    date = as.Date(date),
    # 3-way outcome: Home / Draw / Away
    result = dplyr::case_when(
      home_goals > away_goals ~ "H",
      home_goals < away_goals ~ "A",
      TRUE                    ~ "D"
    ),
    result = factor(result, levels = c("H","D","A")),
    xg_diff = home_xg - away_xg
  ) %>%
  dplyr::arrange(date)

# If season_end_year not present or you want to enforce 22/23–24/25:
dat <- dat %>%
  dplyr::mutate(
    season_end_year = dplyr::if_else(
      lubridate::month(date) >= 7L,
      lubridate::year(date) + 1L,
      lubridate::year(date)
    )
  ) %>%
  dplyr::filter(season_end_year %in% c(2023L, 2024L, 2025L))

cat("Rows after season filter:", nrow(dat), "\n")

# Keep only rows with valid implied probabilities
dat <- dat %>%
  dplyr::filter(
    is.finite(xg_diff),
    !is.na(p_home), !is.na(p_draw), !is.na(p_away),
    p_home > 0, p_home < 1,
    p_draw > 0, p_draw < 1,
    p_away > 0, p_away < 1
  )

cat("Rows after odds filter:", nrow(dat), "\n")

# ================================================================
# 2) Recent form (last 5 matches) - same as binary model
# ================================================================

# Home form
home_form <- dat %>%
  dplyr::group_by(home_team) %>%
  dplyr::arrange(date, .by_group = TRUE) %>%
  dplyr::mutate(
    home_goal_diff_form = rolling_mean(home_goals - away_goals, 5),
    home_win_form       = rolling_mean(as.integer(home_goals > away_goals), 5)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(date, home_team, home_goal_diff_form, home_win_form)

# Away form
away_form <- dat %>%
  dplyr::group_by(away_team) %>%
  dplyr::arrange(date, .by_group = TRUE) %>%
  dplyr::mutate(
    away_goal_diff_form = rolling_mean(away_goals - home_goals, 5),
    away_win_form       = rolling_mean(as.integer(away_goals > home_goals), 5)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(date, away_team, away_goal_diff_form, away_win_form)

# Merge back
dat <- dat %>%
  dplyr::left_join(home_form, by = c("date","home_team")) %>%
  dplyr::left_join(away_form, by = c("date","away_team"))

cat("Rows after form merge:", nrow(dat), "\n")

# Drop rows where form is missing (early-season matches)
dat_model <- dat %>%
  tidyr::drop_na(
    xg_diff,
    p_home, p_draw, p_away,
    home_goal_diff_form, away_goal_diff_form,
    home_win_form, away_win_form
  )

cat("Rows used for multinomial model:", nrow(dat_model), "\n")

# ================================================================
# 3) Train/test split
# ================================================================

set.seed(123)
idx   <- sample(seq_len(nrow(dat_model)), floor(0.3 * nrow(dat_model)))
train <- dat_model[-idx, ]
test  <- dat_model[idx, ]

cat("Train rows:", nrow(train), "  Test rows:", nrow(test), "\n")

# ================================================================
# 4) Multinomial logistic regression (Home / Draw / Away)
# ================================================================

# Model formula: can later extend with Elo, quality_diff, etc.
form_multi <- result ~ xg_diff +
  p_home + p_draw + p_away +
  home_goal_diff_form + away_goal_diff_form +
  home_win_form       + away_win_form

m_multi <- nnet::multinom(
  form_multi,
  data  = train,
  trace = FALSE
)

cat("\n=== Multinomial model summary (coefficients per outcome) ===\n")
print(summary(m_multi))

# ================================================================
# 5) Evaluate vs bookmaker 3-way probabilities
# ================================================================

# ---- Model predictions ----
p_model_test <- predict(m_multi, newdata = test, type = "probs")
# Ensure it's a matrix with columns H,D,A in order:
if (is.vector(p_model_test)) {
  p_model_test <- cbind(H = p_model_test, D = 0, A = 0)  # shouldn't happen but safe
}
if (is.null(colnames(p_model_test))) {
  colnames(p_model_test) <- levels(train$result)
}
p_model_test <- p_model_test[, c("H","D","A")]

# ---- Bookmaker implied probabilities ----
p_book_test <- cbind(
  H = test$p_home,
  D = test$p_draw,
  A = test$p_away
)

# ---- True labels ----
y_test <- test$result

# ---- Metrics ----
logloss_model <- multi_logloss(p_model_test, y_test, class_levels = c("H","D","A"))
logloss_book  <- multi_logloss(p_book_test,  y_test, class_levels = c("H","D","A"))

acc_model <- accuracy_3way(p_model_test, y_test, class_levels = c("H","D","A"))
acc_book  <- accuracy_3way(p_book_test,  y_test, class_levels = c("H","D","A"))

res_eval <- tibble::tibble(
  source   = c("Model: xG+odds+form (multinomial)",
               "Bookmaker implied probs"),
  logloss  = c(logloss_model, logloss_book),
  accuracy = c(acc_model,     acc_book)
)

cat("\n=== 3-way (H/D/A) performance on test set ===\n")
print(res_eval)

# ================================================================
# 6) Simple plot: Predicted draw probability vs xG_diff
# ================================================================

test$pred_p_draw <- p_model_test[, "D"]

p_draw_plot <- ggplot(test, aes(x = xg_diff, y = pred_p_draw)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE, colour = "black") +
  labs(
    title    = "Predicted Draw Probability vs xG Difference",
    subtitle = "Multinomial model: xG + odds + recent form",
    x        = "xG Difference (Home - Away)",
    y        = "Predicted P(Draw)"
  ) +
  theme_minimal(base_size = 12)

print(p_draw_plot)

ggsave(
  file.path(fig_dir, "multinomial_pred_draw_vs_xgdiff.png"),
  p_draw_plot,
  width  = 8,
  height = 5,
  dpi    = 300,
  bg     = "white"
)

cat("\nSaved figure: outputs/figures/multinomial_pred_draw_vs_xgdiff.png\n")

# ================================================================
# 7) Per-class accuracy + confusion matrix
# ================================================================

pred_class <- colnames(p_model_test)[max.col(p_model_test, ties.method = "first")]
pred_class <- factor(pred_class, levels = c("H","D","A"))

conf <- table(Predicted = pred_class, Actual = y_test)
cat("\n=== Confusion matrix ===\n")
print(conf)

# Per-class recall (sensitivity)
per_class <- sapply(c("H","D","A"), function(cls) {
  actual_cls <- y_test == cls
  pred_cls   <- pred_class == cls
  recall    <- sum(pred_cls & actual_cls) / sum(actual_cls)
  precision <- if (sum(pred_cls) > 0) sum(pred_cls & actual_cls) / sum(pred_cls) else 0
  c(recall = recall, precision = precision, n = sum(actual_cls))
})

cat("\n=== Per-class recall and precision ===\n")
print(round(per_class, 3))

# Mean predicted probability by outcome class
mean_pred_by_class <- sapply(c("H","D","A"), function(cls) {
  colMeans(p_model_test[y_test == cls, , drop = FALSE])
})
cat("\n=== Mean predicted probabilities by actual outcome ===\n")
print(round(mean_pred_by_class, 3))

# ================================================================
# 8) Save tables for R Markdown
# ================================================================

tab_dir <- "outputs/tables"
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

# Confusion matrix
confusion_tab <- as.data.frame.matrix(conf)
confusion_tab <- tibble::rownames_to_column(confusion_tab, var = "Predicted")

write.csv(confusion_tab,
          file.path(tab_dir, "multinomial_confusion.csv"),
          row.names = FALSE)

# Per-class recall and precision
perclass_tab <- as.data.frame(t(per_class))
perclass_tab <- tibble::rownames_to_column(perclass_tab, var = "Outcome")

write.csv(perclass_tab,
          file.path(tab_dir, "multinomial_perclass.csv"),
          row.names = FALSE)

cat("Saved: multinomial_confusion.csv and multinomial_perclass.csv\n")
