# Football Match Forecasting

This repository contains the code and supporting materials for my final-year dissertation in Data Science at University College London, titled *Forecasting the Results of Football Matches*.

The project investigates whether combining performance-based metrics, betting market information, and structural indicators of team strength can improve forecasts of English Premier League match outcomes. The main focus is on predicting the probability of a home win using a transparent statistical framework, while also comparing this approach with more flexible machine learning methods. :contentReference[oaicite:0]{index=0} :contentReference[oaicite:1]{index=1}

## Project Aim

The dissertation examines how different sources of football information contribute to predictive performance. In particular, it studies whether bookmaker-implied probabilities, recent form, Elo ratings, and player-based quality measures add value beyond expected-goals differences alone. :contentReference[oaicite:2]{index=2}

## Data Sources

The analysis combines several sources of pre-match and historical football data:

- **FBref** match-level data for English Premier League fixtures, including goals and expected goals (xG). :contentReference[oaicite:3]{index=3}
- **Football-Data** bookmaker odds, converted into implied probabilities.
- **ClubElo** ratings, used to capture longer-run team strength.
- **Player-level statistics** aggregated to the team level to represent differences in squad quality. :contentReference[oaicite:4]{index=4}

All features are constructed using information available prior to each match in order to avoid look-ahead bias. 

## Main Features

The modelling pipeline includes the following predictors:

- expected-goals difference
- bookmaker-implied home win probability
- recent-form measures based on rolling windows
- Elo rating difference
- player-based team quality measures

These variables are designed to combine short-run performance, market expectations, and longer-run indicators of team strength within a single forecasting framework. 

## Modelling Approach

The project uses an incremental modelling strategy.

### 1. Bradley-Terry model
A Bradley-Terry framework is used first to estimate latent team strength from match results, including a home advantage extension. This provides a structural benchmark for later models. :contentReference[oaicite:7]{index=7}

### 2. Binary logistic regression
Binary logistic regression is the main inferential model in the dissertation. It is used to estimate the probability that the home team wins a match based on observed pre-match features. 

### 3. Regularised logistic regression
LASSO regularisation is used to assess variable stability and control model complexity when multiple correlated predictors are present. 

### 4. Multinomial logistic regression
A multinomial extension is included to model home wins, draws, and away wins explicitly. This is used as a three-outcome benchmark, although the results suggest that the available features are more effective for binary home-win prediction than for draw prediction. 

### 5. Random Forest
A Random Forest classifier is used as a flexible non-linear benchmark. It captures interactions and non-linearities that are less accessible in standard regression models. 

## Evaluation

Models are evaluated using temporally ordered train-test splits and walk-forward validation so that predictions are generated only from information that would have been available at the time. This makes the evaluation closer to realistic forecasting conditions. 

Performance is assessed using:

- log-loss
- AUC for binary home-win models
- pseudo-\(R^2\) and AIC for regression-based specifications
- walk-forward paper-trading results for the betting simulation framework 

## Main Findings

The results show that expected-goals difference provides a strong baseline, but bookmaker-implied probabilities produce the largest single improvement in predictive performance. Elo ratings and player-based quality measures add further value, suggesting that different information sources contain complementary signal. :contentReference[oaicite:14]{index=14}

Within the regression framework, the strongest models are obtained using moderate recent-form windows and a limited number of historical seasons. However, the Random Forest model achieves the best overall predictive performance under walk-forward validation, with a mean log-loss of about 0.435 and an AUC of 0.875 when using a five-match form window. :contentReference[oaicite:15]{index=15}

Although the Random Forest performs best predictively, logistic regression remains the main inferential model because it is easier to interpret and better suited to explaining how each source of information contributes to forecasted probabilities. 

## Profitability Analysis

In addition to statistical evaluation, the project includes a walk-forward paper-trading simulation. For each match, a bet is placed only when the model-implied probability of a home win exceeds the bookmaker-implied probability by a chosen threshold. This is used to test whether predictive signals translate into potential economic value under realistic out-of-sample conditions. 

The profitability results should be interpreted cautiously, but they provide an applied perspective on whether forecasting accuracy can be converted into a decision rule. :contentReference[oaicite:18]{index=18}

## Repository Structure

A typical workflow in this project is:

1. collect and clean match and odds data
2. construct pre-match features
3. fit Bradley-Terry and logistic regression models
4. compare regularised and multinomial extensions
5. run walk-forward Random Forest evaluation
6. perform profitability simulation and threshold sensitivity analysis
7. generate tables and figures for the dissertation

## Reproducibility

The code is written in R and uses packages for data collection, cleaning, modelling, and visualisation. Exact reproduction may require access to the original data files or regeneration of intermediate outputs depending on the scripts included in the repository.

## Author

Panapat Mahatchavaroj  
BSc Data Science  
University College London
