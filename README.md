## Modeling Summary — Depression Score Prediction (PHQ-9)

This project examines linear modeling approaches to predict depression severity (PHQ-9 scores) using 
behavioral, demographic, and psychosocial predictors. The baseline ordinary least squares (OLS) 
model exhibited limited explanatory power (R² ≈ 0.07) and modest fit (RMSE ≈ 4.4), suggesting that 
available predictors capture little of the variance in depression outcomes.

To improve model assumptions and stability, Box–Cox, Yeo–Johnson, and log-transformations of the 
outcome were explored. The Box–Cox transformation failed due to zero values, whereas Yeo–Johnson 
and log transformations improved residual symmetry and homoscedasticity on their respective 
transformed scales. However, after back-transforming predictions to the original PHQ-9 metric, 
performance remained comparable to the raw-outcome model (RMSE ≈ 4.4, MAE ≈ 2.8), indicating no 
meaningful gain in predictive accuracy.

A regularized Elastic Net model was also evaluated to mitigate potential overfitting and 
multicollinearity. While coefficients were more stable, predictive performance was similar to OLS.

**Takeaway**: Transformations and regularization improved statistical robustness but not 
out-of-sample accuracy. Predictive limits likely reflect unmeasured factors, measurement error, or 
nonlinear relationships. Future steps include multiple imputation, nonlinear learners (e.g., 
XGBoost, Random), and model-agnostic interpretability tools (PDP, SHAP) for transparent insights.

---

### Methods
All analyses were conducted in R (version 4.4 +) using the `tidymodels` framework for data 
preprocessing, model training, and validation. Continuous variables were normalized using the 
`bestNormalize` package, and penalized regression models were implemented via `glmnet`. Data 
wrangling and visualization relied on `dplyr`, `ggplot2`, and related `tidyverse` tools. All 
scripts were version-controlled and documented to ensure reproducibility of the analytic workflow 
and results.

---
## Repository Structure
├── Predicting-Depression-Severity_files/figure-gfm/ # Auto-generated plots  
├── data/ # Source dataset  
├── .gitignore  
├── Predicting Depression Severity.Rmd # Main notebook (.Rmd version)  
├── Predicting-Depression-Severity.md # Main notebook (for GitHub rendering)  
├── R-tidyverse-Linear-Regression.Rproj # manages the project in RStudio  
└── README.md # Overview + documentation
