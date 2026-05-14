# =============================================================================
# Policyholder Retention Model — Testing & Evaluation
# Actuarial Internship Project | Summer 2025
# Author: Miriam Tenkorang
# =============================================================================
# PURPOSE:
#   Generates out-of-sample predictions on the test set, evaluates model
#   performance via confusion matrix and ROC analysis, and exports final
#   renewal probability scores for business use.
#
# PREREQUISITES:
#   - Run DataPrep.R  → inputdata.RData
#   - Run DataModel.R → model object in environment
# =============================================================================

library(dplyr)
library(caret)
library(pROC)


# =============================================================================
# 1. ALIGN TEST DATA FACTOR LEVELS WITH TRAINING DATA
# =============================================================================
# County codes in test set must match those seen during training.
# Rows with unseen county codes are dropped to avoid prediction errors.

balanced_train$`Property County Code` <- factor(balanced_train$`Property County Code`)
testdata$`Property County Code`        <- factor(testdata$`Property County Code`)

valid_levels <- levels(balanced_train$`Property County Code`)
testdata      <- testdata[testdata$`Property County Code` %in% valid_levels, ]
testdata$`Property County Code` <- factor(testdata$`Property County Code`,
                                           levels = valid_levels)


# =============================================================================
# 2. GENERATE PREDICTIONS
# =============================================================================

# Predicted renewal probabilities (continuous, 0–1)
test_probs <- predict(model, newdata = testdata, type = "response")

# Binary classification using default 0.5 threshold
test_preds <- ifelse(test_probs > 0.5, "Renewed", "Canceled")

# Append predictions to test data
testdata$RenewalProb  <- test_probs
testdata$RenewalClass <- test_preds


# =============================================================================
# 3. CONFUSION MATRIX
# =============================================================================

testdata$Status <- ifelse(testdata$Status == 1, "Renewed", "Canceled")
evaldata        <- testdata[!is.na(testdata$RenewalClass), ]

confusionMatrix(
  factor(evaldata$RenewalClass, levels = c("Canceled", "Renewed")),
  factor(evaldata$Status,       levels = c("Canceled", "Renewed")),
  positive = "Renewed"
)


# =============================================================================
# 4. ROC CURVE & OPTIMAL THRESHOLD
# =============================================================================

roc_obj <- roc(testdata$Status, test_probs)

# Plot ROC curve
plot(roc_obj,
     main = "ROC Curve — Retention Model",
     col  = "steelblue",
     lwd  = 2)
cat("AUC:", auc(roc_obj), "\n")

# Youden's Index: maximizes sensitivity + specificity simultaneously
# Used to select the operating threshold for business deployment
optimal_threshold <- coords(roc_obj, "best", ret = "threshold", best.method = "youden")
cat("Optimal probability threshold (Youden's Index):", optimal_threshold$threshold, "\n")


# =============================================================================
# 5. EXPORT FINAL PREDICTIONS
# =============================================================================

testdata$RenewalProb <- round(testdata$RenewalProb, 2)

final_predictions <- testdata %>%
  select(TranId, Status, RenewalProb, RenewalClass)

write.csv(final_predictions, "final_renewal_predictions.csv", row.names = FALSE)
cat("Predictions exported to final_renewal_predictions.csv\n")
