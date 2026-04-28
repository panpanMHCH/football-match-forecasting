library(dplyr)
library(readr)
library(janitor)
library(lubridate)
library(stringr)

# ---- SETTINGS ----
raw_dir  <- "data/football-data"
out_file <- "data/processed/epl_fbref_with_odds.csv"

# ---- Helper to pick first non-NA odds ----
pick_first <- function(...) {
  Reduce(function(a, b) ifelse(is.na(a), b, a), list(...))
}

# ---- 1) Load ALL E0_* raw files ----
odds_files <- list.files(raw_dir, pattern = "^E0_.*\\.csv$", full.names = TRUE)
cat("Files detected:\n")
print(odds_files)

odds_raw <- lapply(odds_files, function(f) {
  df <- readr::read_csv(f, show_col_types = FALSE)
  df$file_source <- basename(f)    # Keep season info
  df
}) |> bind_rows()

cat("\nTotal rows loaded:", nrow(odds_raw), "\n")

# ---- 2) Clean column names ----
odds_clean <- odds_raw |> 
  janitor::clean_names()

# ---- 3) Extract date + season ----
# Football-Data date formats vary across seasons
odds_clean <- odds_clean %>%
  mutate(
    date = suppressWarnings(
      dmy(date) %>% coalesce(mdy(date)) %>% coalesce(ymd(date))
    ),
    season_end_year = if_else(
      month(date) >= 7,
      year(date) + 1L,
      year(date)
    )
  )

# ---- 3b) Ensure all odds columns exist (create as NA if missing) ----
expected_cols <- c(
  "b365h","b365d","b365a",
  "bbavh","bbavd","bbava",
  "psh","psd","psa"
)

missing_cols <- setdiff(expected_cols, names(odds_clean))
if (length(missing_cols) > 0) {
  for (nm in missing_cols) {
    odds_clean[[nm]] <- NA_real_
  }
  message("Created missing odds columns as NA: ",
          paste(missing_cols, collapse = ", "))
}


# ---- 4) Basic field selection ----
# Adjust these names if needed (football-data columns)
odds_tidy <- odds_clean %>%
  transmute(
    date,
    season_end_year,
    home_team = home_team,   # sometimes Div/Eq differ; adjust if needed
    away_team = away_team,
    
    # Odds providers:
    b365h, b365d, b365a,
    bbavh, bbavd, bbava,
    psh, psd, psa
  )

# ---- 5) Compute implied probabilities ----
odds_tidy <- odds_tidy %>%
  mutate(
    o_h = pick_first(b365h, bbavh, psh),
    o_d = pick_first(b365d, bbavd, psd),
    o_a = pick_first(b365a, bbava, psa),
    
    imp_h_raw = 1 / o_h,
    imp_d_raw = 1 / o_d,
    imp_a_raw = 1 / o_a,
    
    overround = imp_h_raw + imp_d_raw + imp_a_raw,
    
    p_home = imp_h_raw / overround,
    p_draw = imp_d_raw / overround,
    p_away = imp_a_raw / overround
  )

# ---- 6) Save updated master file ----
dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
write_csv(odds_tidy, out_file)

cat("\nSUCCESS! Updated master odds file saved to:\n", out_file, "\n")

# ============================================================
# STEP 7: MERGE ODDS WITH MATCH RESULTS + xG FROM FBref (GitHub)
# ============================================================

library(worldfootballR)
library(stringi)

# ---- Team name standardiser (same as in your main project) ----
std_key <- function(x){
  x <- tolower(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- gsub("\\(.*?\\)|\\[.*?\\]", "", x)
  x <- gsub("[^a-z0-9 ]", " ", x)
  x <- gsub("\\b(fc|afc|cf|sc|club|u\\d{2})\\b", " ", x)
  x <- gsub("\\s+", " ", x); x <- trimws(x)
  
  # ---- EPL common mappings ----
  x <- sub("^manchester united$", "man united", x)
  x <- sub("^manchester utd$",   "man united", x)
  x <- sub("^manchester city$",  "man city",   x)
  
  # Wolves
  x <- sub("^wolverhampton wanderers$", "wolves", x)
  x <- sub("^wolverhampton$",           "wolves", x)
  
  # Brighton
  x <- sub("^brighton and hove albion$", "brighton", x)
  x <- sub("^brighton hove albion$",     "brighton", x)
  
  # West Ham
  x <- sub("^west ham united$", "west ham", x)
  
  # Newcastle
  x <- sub("^newcastle united$", "newcastle", x)
  
  # Forest
  x <- sub("^nottingham forest$", "nottm forest", x)
  x <- sub("^nottm forest$",      "nottm forest", x)
  
  # Sheffield United
  x <- sub("^sheffield united$", "sheffield utd", x)
  
  # West Brom
  x <- sub("^west bromwich albion$", "west brom", x)
  x <- sub("^west bromwich$",        "west brom", x)
  x <- sub("^west brom$",            "west brom", x)
  
  # Leicester
  x <- sub("^leicester city$", "leicester", x)
  
  # Swansea
  x <- sub("^swansea city$", "swansea", x)
  
  # Stoke
  x <- sub("^stoke city$", "stoke", x)
  
  # Huddersfield
  x <- sub("^huddersfield town$", "huddersfield", x)
  
  # Ipswich
  x <- sub("^ipswich town$", "ipswich", x)
  
  # Cardiff  ---- "cardiff city" vs "cardiff"
  x <- sub("^cardiff city$", "cardiff", x)
  x <- sub("^cardiff$",      "cardiff", x)
  
  # Luton  ---- "luton town" vs "luton"
  x <- sub("^luton town$", "luton", x)
  x <- sub("^luton$",      "luton", x)
  
  # Extra
  x <- sub("^tottenham hotspur$", "tottenham", x)
  x <- sub("^leeds united$", "leeds", x)
  x <- sub("^norwich city$", "norwich", x)
  x <- sub("^ipswich town$", "ipswich", x)
  x <- sub("^nottm forest$", "nottm forest", x)
  x <- sub("^brighton and hove albion$", "brighton", x)
  x <- sub("^wolverhampton wanderers$", "wolves", x)
  
  x <- ifelse(grepl("forest", x), "nottm forest", x)
  
  x
}



# ---- Safety: check that the load function exists ----
if (!"load_fb_advanced_match_stats" %in% ls("package:worldfootballR")) {
  stop(
    "Your worldfootballR version does not have load_fb_advanced_match_stats().\n",
    "Install the latest version, e.g.\n",
    "install.packages('worldfootballR', repos = c('https://jaseziv.r-universe.dev','https://cloud.r-project.org'))"
  )
}

cat("\n=== EXTRACTING EPL MATCH STATS (goals + xG) FROM FBref (GitHub) ===\n")

# ---- 1) Load TEAM-level advanced match stats for EPL ----
# We take summary stats for teams; there will be 2 rows per match (home/away)
matches_team_raw <- worldfootballR::load_fb_advanced_match_stats(
  country        = "ENG",
  gender         = "M",
  tier           = "1st",
  stat_type      = "summary",   # contains Gls + xG
  team_or_player = "team",
  season_end_year = 2018:2025
)

cat("Raw rows from FBref advanced match stats:", nrow(matches_team_raw), "\n")

# =====================================================
# 1) Build match-level data (goals + xG) from FBref
# =====================================================

matches <- matches_team_raw %>%
  janitor::clean_names() %>%
  filter(tolower(league) == "premier league") %>%
  # one row per match (in case of duplicates)
  distinct(match_url, .keep_all = TRUE) %>%
  transmute(
    season_end_year = season_end_year,
    date            = lubridate::ymd(match_date),
    
    home_team = std_key(home_team),
    away_team = std_key(away_team),
    
    # numeric scores
    home_goals = as.integer(home_score),
    away_goals = as.integer(away_score),
    
    # numeric xG  ---- NOTE: use home_x_g / away_x_g
    home_xg = as.numeric(home_x_g),
    away_xg = as.numeric(away_x_g)
  ) %>%
  arrange(date)

cat("Match rows (1 per game):", nrow(matches), "\n")

# =====================================================
# 2) Prepare odds using odds_tidy you already built
# =====================================================

odds <- odds_tidy %>%
  mutate(
    home_team = std_key(home_team),
    away_team = std_key(away_team)
  )

cat("Odds rows:", nrow(odds), "\n")

# =====================================================
# 3) Merge matches + odds and save in odds/processed
# =====================================================

dat_merged <- matches %>%
  left_join(
    odds,
    by = c("date", "home_team", "away_team")
  ) %>%
  arrange(date) %>%
  select(-season_end_year.y) %>%              # drop odds season
  rename(season_end_year = season_end_year.x) # keep FBref one

cat("Merged rows:", nrow(dat_merged), "\n")
cat("Missing p_home:", sum(is.na(dat_merged$p_home)), "\n")

# === DEBUG MISSING ODDS ===

# Rows missing odds
missing_odds <- dat_merged %>%
  filter(is.na(p_home)) %>%
  select(date, season_end_year, home_team, away_team)

cat("\nExamples of matches missing odds:\n")
print(head(missing_odds, 20))

# Check if odds exist in odds_tidy but under different team names
cat("\nChecking if date matches exist in odds file:\n")
print(
  odds %>% filter(date %in% missing_odds$date) %>%
    head(20)
)

# Check for mismatched name keys
cat("\nUnique odds teams around problematic dates:\n")
print(
  odds %>%
    filter(date %in% missing_odds$date) %>%
    distinct(home_team, away_team)
)

# save to data/processed/
out_file_final <- "data/processed/epl_matches_with_xg_odds.csv"
readr::write_csv(dat_merged, out_file_final)

cat("\n✅ Saved merged dataset to:", out_file_final, "\n")

