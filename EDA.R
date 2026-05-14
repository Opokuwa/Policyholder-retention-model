# =============================================================================
# Policyholder Retention Model — Exploratory Data Analysis
# Actuarial Internship Project | Summer 2025
# Author: Miriam Tenkorang
# =============================================================================
# PURPOSE:
#   Exploratory visualizations to understand retention patterns across key
#   policy features before modeling. Informs feature selection and
#   transformation decisions.
#
# PREREQUISITE: Run DataPrep.R first to generate inputdata.RData
# =============================================================================

library(dplyr)

load("inputdata.RData")


# =============================================================================
# 1. OVERALL STATUS DISTRIBUTION
# =============================================================================

barplot(
  table(inputdata$Status),
  col        = c("red", "darkgreen"),
  names.arg  = c("Canceled", "Renewed"),
  main       = "Policy Status Distribution",
  ylab       = "Count"
)
# ~89.7% renewed / 10.3% canceled — severe class imbalance addressed in modeling


# =============================================================================
# 2. PREMIUM CHANGE vs. RETENTION
# =============================================================================

inputdata$PremiumChangeBin <- cut(
  inputdata$PremiumChange,
  breaks = c(-Inf, -10, 0, 10, 20, 40, 60, Inf),
  labels = c("<-10%", "-10–0%", "0–10%", "10–20%", "20–40%", "40–60%", "60%+"),
  right  = FALSE
)

premtab <- table(inputdata$PremiumChangeBin, inputdata$Status)

# Raw counts
barplot(
  t(premtab),
  beside    = TRUE,
  col       = c("red", "darkgreen"),
  legend    = TRUE,
  main      = "Raw Counts: Policy Status by Premium Change",
  xlab      = "Premium Change Bin",
  ylab      = "Number of Policies"
)

# Proportions — more informative for imbalanced data
par(mar = c(5, 4, 4, 8))
barplot(
  t(prop.table(premtab, 1)),
  beside    = TRUE,
  col       = c("red", "darkgreen"),
  main      = "Proportions: Policy Status by Premium Change",
  xlab      = "Premium Change (%)",
  ylab      = "Proportion"
)
legend(
  "topright", inset = c(-0.2, 0), xpd = TRUE,
  legend = c("Canceled", "Renewed"),
  fill   = c("red", "darkgreen"),
  title  = "Policy Status"
)

# Key finding: Cancellation rate roughly doubles for each premium band above 20%
print(round(prop.table(premtab, 1), 2))


# --- Premium Change by Year -----------------------------------------------
# Observe how the premium-retention relationship evolved over time

tab3d <- table(inputdata$PremiumChangeBin, inputdata$Status, inputdata$`Calendar Year`)
years <- dimnames(tab3d)[[3]]

devAskNewPage(TRUE)
for (i in seq(1, length(years), by = 3)) {
  par(mfrow = c(1, 3), mar = c(5, 4, 4, 2))
  for (j in i:min(i + 2, length(years))) {
    yr   <- years[j]
    ptab <- prop.table(tab3d[, , yr], 1)
    barplot(
      t(ptab),
      beside = TRUE,
      col    = c("red", "darkgreen"),
      main   = paste("Premium Change –", yr),
      ylim   = c(0, 1),
      las    = 2,
      xlab   = "Premium Change Bin",
      ylab   = "Proportion"
    )
  }
}
devAskNewPage(FALSE)


# =============================================================================
# 3. RENEWAL TERM vs. RETENTION
# =============================================================================

par(mfrow = c(1, 1))
tab_term <- table(inputdata$`Renewal Term`, inputdata$Status)

barplot(
  t(prop.table(tab_term, 1)),
  beside    = TRUE,
  col       = c("red", "darkgreen"),
  main      = "Retention by Renewal Term",
  xlab      = "Renewal Term",
  ylab      = "Proportion",
  las       = 2
)
legend(
  "topright", inset = c(-0.2, 0), xpd = TRUE,
  legend = c("Canceled", "Renewed"),
  fill   = c("red", "darkgreen"),
  title  = "Policy Status"
)
# Counter-intuitive: longer-tenured customers show lower retention


# =============================================================================
# 4. INSURANCE SCORE vs. RETENTION
# =============================================================================

tab_score <- table(inputdata$InsuranceScoreClass, inputdata$Status)

barplot(
  t(prop.table(tab_score, 1)),
  beside = TRUE,
  col    = c("red", "darkgreen"),
  main   = "Renewal by Insurance Score Class",
  las    = 2
)
# Key finding: Higher-credit customers (A, A+, S) cancel at higher rates —
# likely more price-sensitive and able to shop alternatives


# =============================================================================
# 5. NUMBER OF CLAIMS vs. RETENTION
# =============================================================================

tab_claims <- table(inputdata$`No. of Claims`, inputdata$Status)

barplot(
  t(prop.table(tab_claims, 1)),
  beside  = TRUE,
  col     = c("red", "darkgreen"),
  main    = "Renewal by Number of Claims",
  xlab    = "No. of Claims",
  ylab    = "Proportion",
  legend  = TRUE,
  las     = 2
)

boxplot(
  PremiumChange ~ `No. of Claims`,
  data  = inputdata,
  col   = "steelblue",
  main  = "Premium Change by Number of Claims",
  xlab  = "No. of Claims",
  ylab  = "Premium Change (%)",
  ylim  = c(-100, 350)
)
# Non-linear pattern: 0-claim and 3-claim policies show highest cancellation rates


# =============================================================================
# 6. DISCOUNT ANALYSIS
# =============================================================================

# AOP (All Other Perils) discount
tab_aopflag <- table(inputdata$AOP_DiscountFlag, inputdata$Status)
barplot(
  t(prop.table(tab_aopflag, 1)),
  beside     = TRUE,
  col        = c("red", "darkgreen"),
  main       = "Renewal by AOP Discount",
  names.arg  = c("No Discount", "Discounted"),
  xlab       = "AOP Discount Applied",
  ylab       = "Proportion",
  legend     = TRUE
)

# HUR (Hurricane) discount
tab_hurflag <- table(inputdata$HUR_DiscountFlag, inputdata$Status)
barplot(
  t(prop.table(tab_hurflag, 1)),
  beside     = TRUE,
  col        = c("red", "darkgreen"),
  main       = "Renewal by Hurricane Discount",
  names.arg  = c("No Discount", "Discounted"),
  xlab       = "HUR Discount Applied",
  ylab       = "Proportion",
  legend     = TRUE
)

# OWH (Other Wind/Hail) discount
tab_owhflag <- table(inputdata$OWH_DiscountFlag, inputdata$Status)
barplot(
  t(prop.table(tab_owhflag, 1)),
  beside     = TRUE,
  col        = c("red", "darkgreen"),
  main       = "Renewal by OWH Discount",
  names.arg  = c("No Discount", "Discounted"),
  xlab       = "OWH Discount Applied",
  ylab       = "Proportion",
  legend     = TRUE
)


# =============================================================================
# 7. PRODUCT LINE vs. RETENTION
# =============================================================================

tab_product <- table(inputdata$`Product Code`, inputdata$Status)

barplot(
  t(prop.table(tab_product, 1)),
  beside    = TRUE,
  col       = c("red", "darkgreen"),
  main      = "Renewal by Product Line",
  xlab      = "Product Code",
  ylab      = "Proportion",
  las       = 2
)
print(round(prop.table(tab_product, 1), 2))
# HO3LA equivalent: highest/most stable retention
# TX product: steepest decline and greatest volatility (2018–2021)
# SC product: more recent market, performance between the other two


# =============================================================================
# 8. ROOF AGE vs. RETENTION
# =============================================================================

par(mfrow = c(1, 2))

plot(
  table(inputdata$`Roof Age`, inputdata$Status),
  main = "Policy Status by Roof Age (Raw)",
  xlab = "Roof Age (Years)",
  ylab = "Status",
  col  = c("lightblue", "red")
)

plot(
  table(inputdata$RoofAgeGroup, inputdata$Status),
  main = "Policy Status by Roof Age Group",
  xlab = "Roof Age Group",
  ylab = "Status",
  col  = c("lightblue", "red")
)

par(mfrow = c(1, 1))
