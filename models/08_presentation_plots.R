# =========================================================
# Presentation Plots for Dissertation Viva / Slides
# =========================================================

library(ggplot2)
library(scales)

# -----------------------------
# Slide 10: Form window sensitivity
# -----------------------------
form_auc <- data.frame(
  k = c(5, 10, 15),
  auc = c(0.875, 0.841, 0.815)
)

p_form_auc <- ggplot(form_auc, aes(x = k, y = auc)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 3) +
  geom_text(
    aes(label = sprintf("%.3f", auc)),
    vjust = -0.8,
    size = 4
  ) +
  scale_x_continuous(breaks = c(5, 10, 15)) +
  scale_y_continuous(
    limits = c(0.80, 0.89),
    breaks = seq(0.80, 0.88, by = 0.02),
    labels = number_format(accuracy = 0.001)
  ) +
  labs(
    title = "AUC by Form Window Length",
    subtitle = "Random Forest walk-forward validation",
    x = "Form window length (k)",
    y = "Mean AUC"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 13),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p_form_auc)

ggsave(
  filename = "outputs/figures/slide10_form_window_auc.png",
  plot = p_form_auc,
  width = 9,
  height = 5.2,
  dpi = 300,
  bg = "white"
)

# -----------------------------
# Slide 13: ROI threshold sensitivity
# Using the fuller Table 5.4 values from the dissertation
# -----------------------------
roi_threshold_full <- data.frame(
  threshold = c(0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5,
                5.0, 5.5, 6.0, 6.5, 7.0, 8.0, 8.5, 9.0, 9.5, 10.0),
  roi = c(0.430, 0.439, 0.458, 0.479, 0.476, 0.485, 0.500, 0.514, 0.522, 0.537,
          0.551, 0.554, 0.560, 0.548, 0.555, 0.535, 0.547, 0.561, 0.569, 0.579)
)

p_roi_full <- ggplot(roi_threshold_full, aes(x = threshold, y = roi)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.4) +
  geom_vline(xintercept = 5, linetype = "dashed", linewidth = 0.7) +
  geom_vline(xintercept = 6, linetype = "dashed", linewidth = 0.7) +
  annotate(
    "text",
    x = 5.5, y = 0.592,
    label = "Selective-betting region",
    size = 4.1
  ) +
  scale_x_continuous(
    breaks = seq(0, 10, by = 1),
    labels = function(x) paste0(x, "%")
  ) +
  scale_y_continuous(
    limits = c(0.42, 0.60),
    breaks = seq(0.42, 0.60, by = 0.03),
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "ROI as a Function of the Edge Threshold",
    subtitle = "Walk-forward Random Forest betting strategy",
    x = expression(paste("Edge threshold ", tau)),
    y = "ROI"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 13),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p_roi_full)

ggsave(
  filename = "outputs/figures/slide13_roi_threshold_full.png",
  plot = p_roi_full,
  width = 9,
  height = 5.2,
  dpi = 300,
  bg = "white"
)

library(readr)
library(dplyr)
library(ggplot2)

fig_dir <- "outputs/figures"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

abilities_df <- read_csv(
  "outputs/tables/bt_home_adv_abilities.csv",
  show_col_types = FALSE
)

bt_plot_df <- abilities_df %>%
  arrange(desc(ability)) %>%
  slice(c(1:5, (n() - 4):n())) %>%
  mutate(team = factor(team, levels = rev(team)))

p_bt_bar <- ggplot(bt_plot_df, aes(x = ability, y = team)) +
  geom_col(fill = "#D39527", width = 0.85) +
  geom_vline(xintercept = 0, colour = "#00A7B5", linewidth = 0.8) +
  labs(
    title = "Estimated Team Strengths",
    x = "Ability estimate",
    y = NULL
  ) +
  theme_minimal(base_size = 20) +
  theme(
    plot.background = element_rect(fill = "#071C2C", colour = NA),
    panel.background = element_rect(fill = "#071C2C", colour = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(colour = "#2A3F4F", linewidth = 0.5),
    axis.text = element_text(colour = "white"),
    axis.title = element_text(colour = "white"),
    plot.title = element_text(colour = "white", face = "bold", size = 22),
    axis.ticks = element_blank()
  )

ggsave(
  file.path(fig_dir, "bt_team_strengths_bar_slide.png"),
  p_bt_bar,
  width = 11,
  height = 6.2,
  dpi = 300,
  bg = "#071C2C"
)

