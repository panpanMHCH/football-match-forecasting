# Forecasting Premier League Match Outcomes

**Final-year dissertation, BSc Data Science, University College London (2026).** Supervised by Prof. Jim Griffin.

This repository contains the code and supporting materials for my dissertation, *Forecasting the Results of Football Matches: Integrating On-Pitch Performance, Market Expectations, and Structural Data*. The project investigates whether combining performance-based metrics, betting market information, and structural indicators of team strength can improve forecasts of English Premier League match outcomes.

---

## TL;DR

A walk-forward Random Forest using xG difference, bookmaker-implied probabilities, ClubElo ratings, recent form, and player-quality features achieves a **mean log-loss of 0.435** and **AUC of 0.875** for home-win prediction with a five-match form window. Logistic regression is retained as the main inferential model because it makes the contribution of each information source interpretable. A walk-forward paper-trading simulation tests whether predictive signal translates into economic value under a positive-edge betting rule.

---

## Project aim

The dissertation examines how different sources of football information contribute to predictive performance. In particular, it studies whether bookmaker-implied probabilities, recent form, Elo ratings, and player-based quality measures add value beyond expected-goals differences alone.

## Data sources

The analysis combines several sources of pre-match and historical football data:

- **FBref**: match-level data for English Premier League fixtures, including goals and expected goals (xG).
- **Football-Data.co.uk**: bookmaker odds, converted into de-vigged implied probabilities.
- **ClubElo**: Elo ratings, capturing longer-run team strength.
- **Player-level statistics** aggregated to team level, representing differences in squad quality.

All features are constructed using information available **prior to each match** to avoid look-ahead bias.

## Main features

The modelling pipeline includes the following predictors:

- Expected-goals difference (rolling windows)
- Bookmaker-implied home-win probability
- Recent-form measures (rolling-window points / goals)
- Elo rating difference
- Player-based team quality measures

These variables combine short-run performance, market expectations, and longer-run indicators of team strength within a single forecasting framework.

## Modelling approach

The project uses an incremental modelling strategy.

**1. Bradley-Terry model.** A Bradley-Terry framework first estimates latent team strength from match results, with a home-advantage extension. The home-win probability takes the form

$$P(\text{home win}) = \sigma(\alpha_h - \alpha_a + \lambda)$$

where `α_h` and `α_a` are home and away latent strengths, `λ` is a fitted home-advantage term, and `σ` is the logistic function. As a worked example, an uplift of `α_h − α_a + λ = 0.45` corresponds to `σ(0.45) ≈ 0.61`, i.e. a ~61% home-win probability. This provides a structural benchmark for later models.

**2. Binary logistic regression.** The main inferential model in the dissertation. Estimates the probability that the home team wins, conditional on observed pre-match features. Coefficients can be read directly as log-odds contributions of each feature.

**3. Regularised logistic regression.** LASSO regularisation is used to assess variable stability and control complexity in the presence of correlated predictors.

**4. Multinomial logistic regression.** Extends to the full home-win, draw, and away-win outcome space. The comparison highlights that the available features are substantially more informative for binary home-win prediction than for draws.

**5. Random Forest.** A flexible non-linear benchmark that captures interactions and non-linearities not accessible to standard regression.

## Evaluation

Models are evaluated using temporally ordered train-test splits and **walk-forward validation**, so that predictions are generated only from information that would have been available at the time of the match. This is closer to realistic forecasting conditions than a single random hold-out.

Performance is assessed using:

- **Log-loss** (primary; proper scoring rule)
- **AUC** for the binary home-win problem
- **Pseudo-R² and AIC** for regression specifications
- **Walk-forward paper-trading returns** for economic evaluation

## Main findings

Expected-goals difference provides a strong baseline, but bookmaker-implied probabilities produce the largest single improvement in predictive performance. Elo ratings and player-based quality measures add further incremental value, indicating that the three information sources carry complementary signal.

Within the regression framework, the strongest models are obtained using moderate recent-form windows and a limited number of historical seasons. The **Random Forest achieves the best overall out-of-sample performance under walk-forward validation, with a mean log-loss of approximately 0.435 and an AUC of 0.875** using a five-match form window.

Although the Random Forest is the best predictor, **logistic regression is retained as the main inferential model** because its coefficients allow each information source's contribution to forecasted probabilities to be read off directly. This property matters for the dissertation's explanatory aim, even where it costs a small amount of predictive performance.

## Profitability analysis

In addition to statistical evaluation, the project includes a walk-forward paper-trading simulation. For each match, a bet is placed only when the model-implied probability of a home win exceeds the bookmaker-implied probability by a chosen edge threshold. This tests whether predictive signal translates into economic value under realistic out-of-sample conditions, and includes a sensitivity analysis over the edge threshold.

The profitability results are interpreted cautiously, since closing odds are an efficient probabilistic forecast, sample sizes are modest, and the simulation does not account for slippage or limits. They provide an applied perspective on whether forecasting accuracy can be converted into a useful decision rule.

## Repository structure

```
football-match-forecasting/
├── README.md
├── .gitignore
├── football-match-forecasting.Rproj
│
├── data/
│   └── README.md                             (sources, licensing, schema)
│
├── models/
│   ├── 01_bt_model_basic.R                   (Bradley-Terry baseline)
│   ├── 02_bt_homeadv.R                       (BT with home-advantage extension)
│   ├── 03_logistic_binary.R                  (binary logistic, incl. LASSO)
│   ├── 04_multinomial_draw.R                 (multinomial extension)
│   ├── 05_random_forest_binary.R             (RF, single-split benchmark)
│   ├── 05_random_forest_binary_walkforward.R (RF under walk-forward validation)
│   ├── 06_profit_simulation_rf.R             (paper-trading sim on RF outputs)
│   ├── 07_profit_simulation_threshold.R      (edge-threshold sensitivity)
│   ├── 08_presentation_plots.R               (figures for dissertation defence)
│   └── experiments_seasons_form.R            (form window and season-count experiments)
│
└── reports/
    └── figures/                              (calibration, equity curve, ROC)
```

The numbered scripts in `models/` are a guided tour of the project: each one builds on the previous and corresponds to a section of the dissertation.

## Reproducibility

The code is written in R.

```bash
git clone https://github.com/panpanMHCH/football-match-forecasting.git
cd football-match-forecasting
```

Open `football-match-forecasting.Rproj` in RStudio, install the packages used in the scripts:

```r
install.packages(c("tidyverse", "glmnet", "randomForest", "BradleyTerry2", "nnet", "pROC"))
```

Then run the numbered scripts in `models/` in order:

```r
source("models/01_bt_model_basic.R")
source("models/03_logistic_binary.R")
source("models/05_random_forest_binary_walkforward.R")
source("models/06_profit_simulation_rf.R")
```

Exact reproduction of the headline numbers requires access to the underlying FBref, Football-Data, and ClubElo extracts; see `data/README.md` for how to regenerate these from public sources.

## Limitations

- **Sample size.** Even across multiple Premier League seasons, draw frequency is ~25% and confidence intervals on the draw bin are wide. The multinomial model's relative weakness on draws partly reflects this.
- **Market efficiency.** Closing odds incorporate information the model does not, including lineups, weather, late injuries, and sharp money. Beating closing odds consistently is hard, and any positive paper-trading ROI should be treated as exploratory rather than a deployable edge.
- **Single league, single market.** Generalisation to other leagues, in-play markets, or non-1X2 markets is not tested here.
- **No explicit team-level random effects.** A hierarchical Bayesian extension (e.g. Dixon-Coles with `rstan`) is a natural next step.

## Author

**Panapat Mahatchavaroj**  
BSc Data Science, University College London  
Open to data scientist roles in sports analytics, betting markets, and applied probabilistic modelling.

- GitHub: [@panpanMHCH](https://github.com/panpanMHCH)
- LinkedIn: [panapat-mahatchavaroj](https://www.linkedin.com/in/panapat-mahatchavaroj-39a3172b4)
- Email: [panpan232004@gmail.com](mailto:panpan232004@gmail.com)
