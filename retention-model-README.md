# Policyholder Retention Model

A GLM-based predictive model built during an actuarial internship at a regional P&C insurance company (Summer 2025) to predict homeowner policy renewal probability and identify the key drivers of customer churn.

## Overview

Policyholder retention is a critical lever for insurance profitability. This project analyzes **376,458 homeowner insurance policy transactions** across three product lines covering multiple states, and builds a logistic regression model that assigns each policy a probability of renewal at the next term — enabling targeted intervention before lapse.

The model and its findings were presented to senior leadership and used to support pricing strategy and retention campaign decisions.

## Key Findings

| Driver | Effect |
|---|---|
| Premium change | Every 10% increase reduces renewal odds by ~23% |
| Renewal term | Each additional term reduces renewal odds by ~25% (counter-intuitive) |
| Insurance score | High-credit customers lapse at higher rates due to price sensitivity |
| Claims history | Zero-claim and 3-claim policies show highest cancellation rates |
| Construction type | Masonry homes renew at higher rates than frame construction |

## Methodology

**Model type:** Binomial GLM (logistic regression)  
**Target variable:** Renewal status (1 = Renewed, 0 = Canceled)  
**Train/test split:** 70% / 30%  
**Predictors:** 18 variables / 165 total coefficients (after categorical expansion)

### Feature Engineering
- Premium change expressed as % change (not raw dollars) for cross-policy comparability
- Coverage A limits log-transformed to normalize the skewed property value distribution
- Peril discounts converted to discount-to-premium ratios (AOP, HUR, OWH)
- Insurance score ranges consolidated from 15 overlapping groups to 9 standardized classes
- Roof age binned into 5 ordinal groups (Very New → Very Old)
- Personal status change flag derived from year-over-year policy history

### Class Imbalance
The data had a severe 9:1 renewal-to-cancellation imbalance. Addressed using:
- **Undersampling:** Renewals reduced to 6.5× the cancellation count (preserves more majority-class information than 1:1 sampling)
- **Differential weighting:** Cancellations weighted 5×, renewals 2× during training

### Model Selection
- Bidirectional stepwise selection (AIC and BIC) — no variables dropped
- VIF check: TierGroup showed VIF = 13; retained after likelihood ratio test confirmed significant contribution
- Interaction terms evaluated but none were significant
- Optimal classification threshold selected via Youden's Index on ROC curve

## Repository Structure

```
├── DataPrep.R        # Data cleaning and feature engineering
├── EDA.R             # Exploratory data analysis and visualizations
├── DataModel.R       # GLM model development and selection
├── TestnEval.R       # Out-of-sample evaluation, ROC analysis, predictions export
└── README.md
```

> **Note:** The underlying policy data is proprietary and is not included in this repository. Scripts are provided for transparency on methodology and reproducibility of approach.

## Requirements

```r
library(dplyr)
library(car)    # vif()
library(caret)  # confusionMatrix()
library(pROC)   # roc(), auc(), coords()
```

## Author

**Miriam Tenkorang**  
MSc Statistics, Oklahoma State University  
Actuarial Candidate — SOA Exam P (Passed May 2025)  
[LinkedIn](https://www.linkedin.com/in/miriam-tenkorang-a15695263)
