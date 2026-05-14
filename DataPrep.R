# =============================================================================
# Policyholder Retention Model — Data Preparation
# Actuarial Internship Project | Summer 2025
# Author: Miriam Tenkorang
# =============================================================================
# PURPOSE:
#   Cleans and engineers features from raw policy transaction data in
#   preparation for exploratory analysis and GLM modeling.
#
# INPUT:  Raw policy export from internal underwriting databases
# OUTPUT: inputdata — a clean, feature-engineered data frame (saved as .RData)
#
# PRODUCT LINES COVERED: Three homeowner product lines across multiple states
# INITIAL ROWS: 397,728  |  AFTER CLEANING: 376,458
# =============================================================================

library(dplyr)

# NOTE: Raw data is proprietary and not included in this repository.
# Load your own data extract and assign it to `inputdata` to run this script.
# load("inputdata.RData")  # Uncomment if using the saved clean version


# =============================================================================
# 1. TARGET VARIABLE
# =============================================================================

# Encode renewal status as binary: Renewed = 1, Canceled = 0
inputdata$Status <- ifelse(inputdata$Status == "Renewed", 1, 0)


# =============================================================================
# 2. FEATURE ENGINEERING
# =============================================================================

# --- 2a. Premium Change (%) -----------------------------------------------
# Use relative premium change instead of raw dollar amounts.
# This makes changes comparable across all policy sizes and avoids
# skewing results toward high-value properties.

inputdata <- inputdata %>%
  mutate(
    PremiumChange = round(((OffersPremium - PriorPremium) / PriorPremium) * 100, 2)
  )


# --- 2b. Roof Age Classification ------------------------------------------
# Bin continuous roof age into ordinal groups for interpretability.

inputdata <- inputdata %>%
  mutate(
    RoofAgeGroup = case_when(
      `Roof Age` <= 5  ~ "Very New",
      `Roof Age` <= 10 ~ "New",
      `Roof Age` <= 15 ~ "Moderate",
      `Roof Age` <= 20 ~ "Old",
      `Roof Age` >  20 ~ "Very Old"
    )
  )


# --- 2c. Insurance Score Consolidation ------------------------------------
# Original data had 15 overlapping score ranges.
# Consolidated into 9 standardized letter-grade classes for cleaner modeling.
# Overlapping intervals resolved by assigning to the broader category.

inputdata <- inputdata %>%
  mutate(
    InsuranceScoreClass = case_when(
      `Insurance Score Range` == "1-550"             ~ "F",
      `Insurance Score Range` %in% c("551-575", "576-600") ~ "D",
      `Insurance Score Range` %in% c("601-625", "626-650") ~ "C",
      `Insurance Score Range` %in% c("651-675", "676-700") ~ "B",
      `Insurance Score Range` %in% c("701-725", "726-750") ~ "A",
      `Insurance Score Range` %in% c("751-775", "776-800", "801-825") ~ "A+",
      `Insurance Score Range` == "826-875"           ~ "S",
      `Insurance Score Range` == "876+"              ~ "S+",
      `Insurance Score Range` == "No Hit / No Score" ~ "Unscored",
      TRUE ~ "Unknown"
    )
  )


# --- 2d. Tier Grouping ----------------------------------------------------
# Map 9 original tier codes to 3 business-meaningful groups.
# Tiers 3 and 4 merged due to similar coastal proximity and risk profiles.

tier_map <- c(
  "0" = "Tier 1", "5" = "Tier 1",
  "1" = "Tier 2", "4" = "Tier 2", "8" = "Tier 2",
  "2" = "Tier 3", "3" = "Tier 3", "6" = "Tier 3", "7" = "Tier 3"
)
inputdata$`Tier Code` <- as.character(inputdata$`Tier Code`)
inputdata$TierGroup   <- factor(tier_map[inputdata$`Tier Code`])


# --- 2e. Discount-to-Premium Ratios ---------------------------------------
# Convert raw discount amounts to ratios relative to peril-level premium.
# Ratios are more stable predictors than raw discount dollar amounts.

names(inputdata)[names(inputdata) == "HUR Dsicount"] <- "HUR Discount"  # fix typo in source data

inputdata <- inputdata %>%
  mutate(
    AOP_DiscPrem_Ratio = round(`AOP Discount` / `AOP Premium`, 4),
    HUR_DiscPrem_Ratio = round(`HUR Discount` / `HUR Premium`, 4),
    OWH_DiscPrem_Ratio = round(`OWH Discount` / `OWH Premium`, 4)
  )


# --- 2f. Coverage A Limit (Log Scale) ------------------------------------
# Log-transform to normalize the wide distribution of property values.

inputdata$CovA <- log(inputdata$`Coverage A Limit`)


# --- 2g. Personal Status Change Flag -------------------------------------
# Flag policies where the insured's personal status changed year-over-year.
# Life events (marriage, divorce, etc.) may signal elevated lapse risk.

inputdata$`Personal Status` <- as.integer(inputdata$`Personal Status`)
inputdata <- inputdata %>% arrange(PolicyId, `Calendar Year`)

inputdata$PStatusChange <- ave(
  inputdata$`Personal Status`,
  inputdata$PolicyId,
  FUN = function(x) c(0, as.integer(diff(x) != 0))
)


# =============================================================================
# 3. DATA CLEANING
# =============================================================================

# Replace NAs in discount ratios with 0 (South Carolina: 1,219 observations
# had no peril-level discount data — absence treated as no discount applied)
inputdata$HUR_DiscPrem_Ratio[is.na(inputdata$HUR_DiscPrem_Ratio)] <- 0
inputdata$OWH_DiscPrem_Ratio[is.na(inputdata$OWH_DiscPrem_Ratio)] <- 0

# Replace infinite PremiumChange values (caused by zero prior premium)
# with the median to avoid distortion during modeling
inputdata$PremiumChange[is.infinite(inputdata$PremiumChange)] <-
  median(inputdata$PremiumChange[is.finite(inputdata$PremiumChange)], na.rm = TRUE)

# Impute missing Bind Year values with the median year
inputdata$`Bind Year`[is.na(inputdata$`Bind Year`)] <-
  median(inputdata$`Bind Year`, na.rm = TRUE)


# =============================================================================
# 4. VALIDATION CHECKS
# =============================================================================

# Confirm no remaining NAs or infinite values
cat("--- Missing Values ---\n")
print(sapply(inputdata, function(x) sum(is.na(x))))

cat("\n--- Infinite Values (numeric columns only) ---\n")
print(sapply(inputdata[sapply(inputdata, is.numeric)], function(x) sum(is.infinite(x))))


# =============================================================================
# 5. SAVE CLEAN DATA
# =============================================================================

save(inputdata, file = "inputdata.RData")
cat("\nData saved to inputdata.RData\n")
