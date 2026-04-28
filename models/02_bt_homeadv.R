# ================================================================
# BRADLEY-TERRY VIA LOGISTIC REGRESSION (NO PACKAGE NEEDED)
# Robust, Manual Matrix Method
# ================================================================

# ================================================================
# 1. Packages ----------------------------------------------------
# ================================================================
library(worldfootballR)
library(dplyr)
library(janitor)
library(stringi)
library(ggplot2)
library(viridis)
library(tibble)
library(tidyr)
library(scales)

# ================================================================
# 2. Settings ----------------------------------------------------
# ================================================================
fig_dir   <- "outputs/figures"
model_dir <- "outputs/models"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

# ================================================================
# 3. Helper: Standardise Team Names ------------------------------
# ================================================================
std_key <- function(x){
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
  x <- sub("^newcastle united$", "newcastle", x)
  x <- sub("^nottingham forest$", "nottm forest", x)
  x <- sub("^sheffield united$", "sheffield utd", x)
  x
}

# ================================================================
# 4. Load Data ---------------------------------------------------
# ================================================================
seasons <- c(2023, 2024, 2025)
cat("⏳ Downloading EPL match results...\n")

matches_raw <- tryCatch({
  load_match_results(country = "ENG", gender = "M", season_end_year = seasons, tier = "1st") %>% 
    clean_names()
}, error = function(e) { stop("❌ Error loading data.") })

# ================================================================
# 5. Build Dataset -----------------------------------------------
# ================================================================
bt_data_prep <- matches_raw
names(bt_data_prep)[grep("home_?score|home_?goals|home_?gls", names(bt_data_prep))] <- "home_goals"
names(bt_data_prep)[grep("away_?score|away_?goals|away_?gls", names(bt_data_prep))] <- "away_goals"
if(!"home_team" %in% names(bt_data_prep) && "home" %in% names(bt_data_prep)) names(bt_data_prep)[names(bt_data_prep)=="home"] <- "home_team"
if(!"away_team" %in% names(bt_data_prep) && "away" %in% names(bt_data_prep)) names(bt_data_prep)[names(bt_data_prep)=="away"] <- "away_team"

# Clean and Filter
bt_data <- bt_data_prep %>%
  mutate(
    home_goals = as.numeric(home_goals),
    away_goals = as.numeric(away_goals)
  ) %>%
  filter(!is.na(home_goals), !is.na(away_goals)) %>%
  mutate(
    home = std_key(home_team),
    away = std_key(away_team)
  ) %>%
  filter(home_goals != away_goals) %>% # Remove Draws
  mutate(
    home_win = ifelse(home_goals > away_goals, 1, 0)
  )

cat("✅ Data Ready. Rows:", nrow(bt_data), "\n")

# ================================================================
# 6. MANUAL MATRIX CONSTRUCTION (The Alternative Method)
# This creates the Bradley-Terry structure without the package
# ================================================================

# Get unique teams
teams <- sort(unique(c(bt_data$home, bt_data$away)))

# Create an empty matrix (Rows = Matches, Cols = Teams)
X_matrix <- matrix(0, nrow = nrow(bt_data), ncol = length(teams))
colnames(X_matrix) <- teams

# Fill the matrix: +1 for Home Team, -1 for Away Team
for(i in 1:nrow(bt_data)) {
  X_matrix[i, bt_data$home[i]] <- 1
  X_matrix[i, bt_data$away[i]] <- -1
}

# Convert to dataframe for GLM
glm_data <- as.data.frame(X_matrix)
glm_data$outcome_is_home_win <- bt_data$home_win

# ================================================================
# 7. Fit Standard Logistic Regression ----------------------------
# ================================================================
# We fit Y ~ Teams. The Intercept automatically becomes Home Advantage 
# because X is coded as (Home - Away)
bt_glm <- glm(outcome_is_home_win ~ ., data = glm_data, family = binomial(link = "logit"))

cat("\n=== GLM Model Summary (Alternative BT) ===\n")
# The Intercept is the Home Advantage
home_bonus <- coef(bt_glm)["(Intercept)"]
cat("🏠 Home Advantage Bonus:", round(home_bonus, 4), "\n")

# ================================================================
# 8. Extract Abilities (FIXED for Reference Team)
# ================================================================

# 1. Get all coefficients
raw_coefs <- coef(bt_glm)

# 2. Separate Home Advantage (Intercept) from Teams
# The Intercept is the Home Bonus
home_bonus <- raw_coefs["(Intercept)"]

# Everything else is a team
team_coefs <- raw_coefs[names(raw_coefs) != "(Intercept)"]

# Clean names (remove backticks if they exist)
names(team_coefs) <- gsub("`", "", names(team_coefs))

# --- THE FIX IS HERE ---
# R sets the reference team to NA to solve the linear equation.
# We must convert that NA to 0 (Baseline Ability).
team_coefs[is.na(team_coefs)] <- 0

# 3. Create the clean dataframe
abilities_df <- data.frame(
  team = names(team_coefs),
  ability = as.numeric(team_coefs)
) %>%
  arrange(desc(ability))

cat("🏠 Home Advantage Bonus:", round(home_bonus, 4), "\n")
cat("🏆 Top 5 Teams:\n")
print(head(abilities_df, 5))

# Check if Wolves (or any team) is now 0 instead of missing
print(tail(abilities_df, 5))

# ================================================================
# 8B. Save home-advantage BT outputs for dissertation ------------
# ================================================================

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# Clean names for dissertation tables
pretty_team <- function(x) {
  dplyr::recode(
    x,
    "arsenal" = "Arsenal",
    "aston villa" = "Aston Villa",
    "bournemouth" = "Bournemouth",
    "brentford" = "Brentford",
    "brighton" = "Brighton",
    "burnley" = "Burnley",
    "chelsea" = "Chelsea",
    "crystal palace" = "Crystal Palace",
    "everton" = "Everton",
    "fulham" = "Fulham",
    "ipswich town" = "Ipswich Town",
    "leeds united" = "Leeds United",
    "leicester city" = "Leicester City",
    "liverpool" = "Liverpool",
    "luton town" = "Luton Town",
    "man city" = "Manchester City",
    "man united" = "Manchester United",
    "newcastle utd" = "Newcastle United",
    "newcastle" = "Newcastle United",
    "nottm forest" = "Nottingham Forest",
    "sheffield utd" = "Sheffield United",
    "southampton" = "Southampton",
    "tottenham" = "Tottenham Hotspur",
    "west ham" = "West Ham United",
    "wolves" = "Wolverhampton Wanderers",
    .default = tools::toTitleCase(x)
  )
}

# Save full ability table
abilities_df_clean <- abilities_df %>%
  mutate(team = pretty_team(team))

write.csv(
  abilities_df_clean,
  "outputs/tables/bt_home_adv_abilities.csv",
  row.names = FALSE
)

# Save compact top/bottom table
bt_home_adv_table <- dplyr::bind_rows(
  abilities_df_clean %>%
    dplyr::slice_max(order_by = ability, n = 5) %>%
    dplyr::mutate(group = "Top 5"),
  abilities_df_clean %>%
    dplyr::slice_min(order_by = ability, n = 5) %>%
    dplyr::mutate(group = "Bottom 5")
) %>%
  dplyr::mutate(
    Rank = c(1:5, 1:5),
    Team = team,
    `Estimated ability` = round(ability, 3)
  ) %>%
  dplyr::select(group, Rank, Team, `Estimated ability`)

write.csv(
  bt_home_adv_table,
  "outputs/tables/bt_home_adv_table.csv",
  row.names = FALSE
)

# Save home advantage parameter
home_bonus_df <- data.frame(
  parameter = "home_advantage",
  estimate = round(as.numeric(home_bonus), 3)
)

write.csv(
  home_bonus_df,
  "outputs/tables/bt_home_bonus.csv",
  row.names = FALSE
)

cat("\n✅ Saved:\n")
cat("- outputs/tables/bt_home_adv_abilities.csv\n")
cat("- outputs/tables/bt_home_adv_table.csv\n")
cat("- outputs/tables/bt_home_bonus.csv\n")
# ================================================================
# 9. Visualization -----------------------------------------------
# ================================================================

# Short display labels for the heatmap only
short_team <- function(x) {
  dplyr::recode(
    x,
    "arsenal" = "Arsenal",
    "aston villa" = "A Villa",
    "bournemouth" = "B'mouth",
    "brentford" = "Brentford",
    "brighton" = "Brighton",
    "burnley" = "Burnley",
    "chelsea" = "Chelsea",
    "crystal palace" = "C Palace",
    "everton" = "Everton",
    "fulham" = "Fulham",
    "ipswich town" = "Ipswich",
    "leeds united" = "Leeds",
    "leicester city" = "Leicester",
    "liverpool" = "Liverpool",
    "luton town" = "Luton",
    "man city" = "Man City",
    "man united" = "Man Utd",
    "newcastle utd" = "Newcastle",
    "newcastle" = "Newcastle",
    "nottm forest" = "Nottm",
    "sheffield utd" = "Sheff Utd",
    "southampton" = "Southampton",
    "tottenham" = "Spurs",
    "west ham" = "West Ham",
    "wolves" = "Wolves",
    .default = tools::toTitleCase(x)
  )
}

# Function to calculate home win probability
prob_glm <- function(home_team, away_team, abilities_df, bonus) {
  ab_home <- if (home_team %in% abilities_df$team) {
    abilities_df$ability[abilities_df$team == home_team]
  } else 0
  
  ab_away <- if (away_team %in% abilities_df$team) {
    abilities_df$ability[abilities_df$team == away_team]
  } else 0
  
  plogis(ab_home - ab_away + bonus)
}

# probability matrix
prob_matrix <- outer(
  teams, teams,
  Vectorize(function(h, a) prob_glm(h, a, abilities_df, home_bonus))
)

rownames(prob_matrix) <- teams
colnames(prob_matrix) <- teams
diag(prob_matrix) <- NA

# team order by estimated ability
team_order <- abilities_df$team
team_labels <- short_team(team_order)

# long format
# Plot
prob_long <- as.data.frame(prob_matrix) %>%
  rownames_to_column("home_team") %>%
  pivot_longer(-home_team, names_to = "away_team", values_to = "prob")

# short labels just for plotting
short_labels <- c(
  "arsenal" = "Arsenal",
  "man city" = "Man City",
  "liverpool" = "Liverpool",
  "newcastle utd" = "Newcastle",
  "newcastle" = "Newcastle",
  "aston villa" = "A Villa",
  "brighton" = "Brighton",
  "chelsea" = "Chelsea",
  "man united" = "Man Utd",
  "tottenham" = "Spurs",
  "brentford" = "Brentford",
  "fulham" = "Fulham",
  "crystal palace" = "C Palace",
  "bournemouth" = "B'mouth",
  "nottm forest" = "Nottm",
  "west ham" = "West Ham",
  "everton" = "Everton",
  "wolves" = "Wolves",
  "leeds united" = "Leeds",
  "leicester city" = "Leicester",
  "luton town" = "Luton",
  "burnley" = "Burnley",
  "ipswich town" = "Ipswich",
  "southampton" = "Southampton",
  "sheffield utd" = "Sheff Utd"
)

team_order <- abilities_df$team

prob_long <- prob_long %>%
  mutate(
    home_team = factor(home_team, levels = team_order),
    away_team = factor(away_team, levels = team_order)
  )

p_glm <- ggplot(prob_long, aes(x = away_team, y = home_team, fill = prob)) +
  geom_tile(color = "white", linewidth = 0.12) +
  scale_fill_viridis(
    name = "P(Home Wins)",
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    na.value = "grey80"
  ) +
  scale_x_discrete(
    position = "top",
    labels = short_labels[team_order]
  ) +
  scale_y_discrete(
    labels = short_labels[team_order]
  ) +
  coord_fixed() +
  labs(
    x = "Away Team",
    y = "Home Team"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(
      size = 10.5,
      angle = 50,
      hjust = 0,
      vjust = 0
    ),
    axis.text.y = element_text(size = 10.5),
    axis.title.x = element_text(size = 12, margin = margin(b = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(t = 25, r = 20, b = 20, l = 25)
  )

ggsave(
  file.path(fig_dir, "bt_glm_heatmap_v2.png"),
  p_glm,
  width = 18,
  height = 15,
  dpi = 450,
  bg = "white"
)

cat("\n✅ Saved: outputs/figures/bt_glm_heatmap_v2.png\n")