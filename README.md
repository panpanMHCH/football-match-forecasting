# football-match-forecasting
Predicting football match outcomes using Statistical Modelling in R.

This project explores how football match outcomes can be modelled and predicted using statistical techniques and expected goals (xG) data.  
It combines the **Bradley–Terry model** for estimating team strengths with **logistic regression** models incorporating xG and other match features.

## 🎯 Objectives
- Build predictive models of football results (win/draw/loss) using match-level features.  
- Compare performance of the **Bradley–Terry model** vs **logistic regression with xG and additional predictors**.  
- Analyse the influence of home advantage and recent form on outcome probabilities.  
- Evaluate predictive accuracy using cross-validation and performance metrics.

## ⚙️ Tools & Libraries
- R, dplyr, tidyr, ggplot2, worldfootballR, bradleyterry2, glm(), caret  
- Data visualisation: ggplot2  
- Model evaluation: Accuracy, Log Loss, ROC-AUC

## 📊 Key Features Used
- Expected Goals (xG)  
- Home/Away indicator  
- Recent form (past 5 matches)  
- Team and opponent strengths (from BT model)  
- Match location and rest days  

## 📈 Results & Insights
- Logistic regression using xG achieved higher predictive accuracy than the base Bradley–Terry model.  
- Incorporating form and home advantage improved calibration.  
- Visualisations show meaningful patterns between xG differences and win probabilities.

## 🧠 Future Work
- Integrate player-level metrics (xGOT, possession, pass completion).  
- Explore Bayesian updates for dynamic strength estimation.  
- Extend to multiple leagues with automated data collection from `worldfootballR`.

## 👨‍💻 About Me
I’m **Panapat Mahatchavaroj**, a **Data Science undergraduate at University College London (UCL)** and lifelong football enthusiast.  
This project reflects my passion for applying **machine learning and statistical modelling** to understand and predict football performance.

