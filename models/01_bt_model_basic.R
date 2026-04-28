# ================================================================
# BASIC BRADLEY-TERRY MODEL (EPL 2022/23 - 2024/25)
# Neutral Venue (No Home Advantage)
# ================================================================

# ================================================================
# 1. Packages ----------------------------------------------------
# ================================================================

library(worldfootballR)
library(BradleyTerry2)
library(dplyr)
library(janitor)
library(stringi)
library(ggplot2)
library(viridis)
library(tibble)
library(tidyr)

# ================================================================
# 2. Team name normaliser (Matches your LR model) ----------------
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
# 3. Load EPL match results from GitHub (No Blocking) ------------
# 2023 = 22/23, 2024 = 23/24, 2025 = 24/25
# ================================================================

seasons <- c(2023, 2024, 2025)

cat("⏳ Downloading EPL match results for seasons ending:", paste(seasons, collapse = ", "), "...\n")

# Using load_match_results prevents HTTP 403 errors
matches_raw <- load_match_results(
  country = "ENG", gender = "M", 
  season_end_year = seasons, tier = "1st"
) %>% clean_names()

cat("✅ Loaded", nrow(matches_raw), "rows.\n")

# ================================================================
# 4. Build Basic BT dataset (Neutral) ----------------------------
# ================================================================

bt_basic <- matches_raw %>%
  filter(!is.na(home_goals), !is.na(away_goals)) %>%
  mutate(
    home_key = std_key(home),
    away_key = std_key(away)
  ) %>%
  # Filter out draws
  filter(home_goals != away_goals) %>%
  mutate(
    # Define Winner (player1) and Loser (player2)
    player1 = ifelse(home_goals > away_goals, home_key, away_key),
    player2 = ifelse(home_goals > away_goals, away_key, home_key),
    outcome = 1 # Player 1 always wins in this structure
  ) %>%
  transmute(
    player1 = factor(player1),
    player2 = factor(player2),
    outcome
  )

cat("BT dataset rows (Draws removed):", nrow(bt_basic), "\n")

# ================================================================
# 5. Fit Basic Bradley Terry Model -------------------------------
# No 'formula' argument needed for basic model (outome ~ 1 is implied)
# ================================================================

bt_basic_model <- BTm(outcome, player1, player2, data = bt_basic)

cat("\n=== Basic Bradley Terry Model (Neutral Venue) ===\n")
summary(bt_basic_model)

# ================================================================
# 6. Extract Abilities -------------------------------------------
# ================================================================

bt_basic_abilities <- BTabilities(bt_basic_model) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("team") %>%
  arrange(desc(ability))

cat("\nTop 10 Teams (Basic Ability):\n")
print(head(bt_basic_abilities, 10), row.names = FALSE)

bt_basic_table <- bind_rows(
  bt_basic_abilities %>%
    slice_max(order_by = ability, n = 5) %>%
    mutate(group = "Top 5"),
  bt_basic_abilities %>%
    slice_min(order_by = ability, n = 5) %>%
    mutate(group = "Bottom 5")
) %>%
  mutate(
    Rank = c(1:5, 1:5),
    Team = team,
    `Estimated ability` = round(ability, 3)
  ) %>%
  select(group, Rank, Team, `Estimated ability`)

write.csv(
  bt_basic_table,
  "outputs/tables/bt_basic_table.csv",
  row.names = FALSE
)

# ================================================================
# 7. Win Probability Matrix (Neutral) ----------------------------
# ================================================================

prob_bt_neutral <- function(team_i, team_j, abil_df){
  if (!team_i %in% abil_df$team || !team_j %in% abil_df$team) return(NA_real_)
  a_i <- abil_df$ability[abil_df$team == team_i]
  a_j <- abil_df$ability[abil_df$team == team_j]
  
  # Basic Formula: exp(A) / (exp(A) + exp(B))
  exp(a_i) / (exp(a_i) + exp(a_j))
}

teams <- bt_basic_abilities$team

prob_matrix_basic <- outer(
  teams, teams,
  Vectorize(function(i, j) prob_bt_neutral(i, j, bt_basic_abilities))
)

rownames(prob_matrix_basic) <- teams
colnames(prob_matrix_basic) <- teams

# Neutral model: Team vs Itself is 50/50
diag(prob_matrix_basic) <- NA

# ================================================================
# 8. Visualization -----------------------------------------------
# ================================================================

prob_long_basic <- as.data.frame(prob_matrix_basic) %>%
  rownames_to_column("row_team") %>%
  pivot_longer(-row_team,
               names_to  = "col_team",
               values_to = "prob")

prob_long_basic <- prob_long_basic %>%
  mutate(
    row_team = factor(row_team, levels = teams),
    col_team = factor(col_team, levels = teams)
  )

p_basic <- ggplot(prob_long_basic, aes(x = col_team, y = row_team, fill = prob)) +
  geom_tile(color = "white", linewidth = 0.1) +
  scale_fill_viridis(
    name   = "P(row beats col)",
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    na.value = "grey"
  ) +
  scale_x_discrete(position = "top") +
  labs(
    title    = "Basic Bradley-Terry Probabilities (Neutral Venue)",
    subtitle = "Based on EPL seasons 2022/23 - 2024/25",
    x        = "Opponent (column team)",
    y        = "Team (row team)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0),
    panel.grid  = element_blank()
  )

print(p_basic)

# Save
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
ggsave("outputs/figures/bt_basic_heatmap.png",
       p_basic, width = 12, height = 10, dpi = 300, bg = "white")

cat("\n✅ Saved: outputs/figures/bt_basic_heatmap.png\n")
