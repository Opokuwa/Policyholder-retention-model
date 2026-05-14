# =============================================================================
# Policyholder Retention Model — Model Development
# Actuarial Internship Project | Summer 2025
# Author: Miriam Tenkorang
# =============================================================================
# PURPOSE:
#   Builds a binomial GLM to predict the probability of policy renewal.
#   Handles class imbalance via hybrid undersampling + differential weighting,
#   and selects the final model specification via stepwise AIC/BIC and
#   likelihood ratio testing.
#
# PREREQUISITE: Run DataPrep.R first to generate inputdata.RData
# =============================================================================

library(dplyr)
library(car)   # for vif()

load("inputdata.RData")


# =============================================================================
# 1. TRAIN / TEST SPLIT  (70 / 30)
# =============================================================================

set.seed(100)
train_idx  <- sample(seq_len(nrow(inputdata)), size = 0.7 * nrow(inputdata))
traindata  <- inputdata[train_idx, ]
testdata   <- inputdata[-train_idx, ]


# =============================================================================
# 2. CLASS IMBALANCE RESOLUTION
# =============================================================================
# Original split: ~89.7% Renewed / 10.3% Canceled (≈ 9:1 ratio)
# A naive model predicting "Renewed" for all cases would achieve 89.7% accuracy
# but provide zero business value. Strategy: hybrid undersampling + class weights.

renewed <- traindata[traindata$Status == 1, ]
canceled <- traindata[traindata$Status == 0, ]

# Undersample renewals to 6.5× the number of cancellations.
# This retains more majority-class information than 1:1 sampling,
# while still giving the model sufficient exposure to canceled cases.
set.seed(100)
renewed_sample <- renewed[sample(nrow(renewed), size = round(6.5 * nrow(canceled))), ]
balanced_train <- rbind(renewed_sample, canceled)

# Shuffle to remove any ordering effect
set.seed(100)
balanced_train <- balanced_train[sample(nrow(balanced_train)), ]

# Differential class weights: upweight cancellations further to improve
# minority-class detection without discarding majority-class data
weights <- ifelse(balanced_train$Status == 0, 5, 2)


# =============================================================================
# 3. MODEL SPECIFICATION
# =============================================================================
# 18 predictor variables / 165 total coefficients (due to categorical expansion)
#
# Continuous:  Calendar Year, Renewal Term, Homeowner Age, No. of Claims,
#              PremiumChange, CovA (log), AOP/HUR/OWH Discount-Premium Ratios
# Categorical: Product Code, Construction Type, LLRS at Renewal, County Code,
#              TierGroup, RoofAgeGroup, InsuranceScoreClass, PStatusChange

model <- glm(
  Status ~
    `Calendar Year`          +
    `Renewal Term`           +
    factor(`Product Code`)   +
    `Homeowner Age`          +
    factor(`Construction Type`) +
    factor(`LLRS at Renewal`) +
    factor(`Property County Code`) +
    factor(TierGroup)        +
    `No. of Claims`          +
    PremiumChange            +
    factor(RoofAgeGroup)     +
    AOP_DiscPrem_Ratio       +
    HUR_DiscPrem_Ratio       +
    OWH_DiscPrem_Ratio       +
    factor(InsuranceScoreClass) +
    factor(PStatusChange)    +
    CovA,
  data    = balanced_train,
  weights = weights,
  family  = binomial
)

summary(model)


# =============================================================================
# 4. MODEL SELECTION
# =============================================================================

# --- 4a. Stepwise Selection (AIC) -----------------------------------------
# Bidirectional stepwise — no variables were dropped, confirming all 18
# predictors contribute meaningfully to model fit.
mainsel <- step(model, direction = "both")
summary(mainsel)

# --- 4b. Stepwise Selection (BIC) -----------------------------------------
# BIC applies a heavier penalty for complexity; used as a robustness check.
n <- nrow(balanced_train)
mainsel_bic <- step(model, direction = "both", k = log(n))
summary(mainsel_bic)


# =============================================================================
# 5. MULTICOLLINEARITY CHECK
# =============================================================================

vif_results <- vif(model)
print(vif_results)

# TierGroup VIF = 13 (exceeds the standard threshold of 5).
# Test whether Tier Group adds significant explanatory power despite collinearity.

model_reduced <- glm(
  Status ~
    `Calendar Year`          +
    `Renewal Term`           +
    factor(`Product Code`)   +
    `Homeowner Age`          +
    factor(`Construction Type`) +
    factor(`LLRS at Renewal`) +
    factor(`Property County Code`) +
    `No. of Claims`          +
    PremiumChange            +
    factor(RoofAgeGroup)     +
    AOP_DiscPrem_Ratio       +
    HUR_DiscPrem_Ratio       +
    OWH_DiscPrem_Ratio       +
    factor(InsuranceScoreClass) +
    factor(PStatusChange)    +
    CovA,
  family = binomial,
  data   = balanced_train
)

# Likelihood Ratio Test: Tier Group addition is highly significant
# -> retained in final model despite elevated VIF
anova(model_reduced, model, test = "Chisq")
