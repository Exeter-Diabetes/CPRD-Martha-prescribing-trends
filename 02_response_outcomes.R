# ---------------------------------------------------------
# 02_response_outcomes.R
#
# This script:
#   1. Loads CPRD data & builds the second-line cohort
#   2. Prepares response variables  
#        - If 12m response is missing, uses 6m response  
#        - Performs multiple imputation (mice)
#   3. For each response:
#        - Fits regression model
#        - Computes marginal predictions (year × frailty)
#        - Computes contrasts (e.g., 2023 vs 2019)
#        - Produces plots of adjusted trends
#      Outcomes:
#        (a) 12-month HbA1c response
#        (b) 12-month weight response
#        (c) 12-month treatment discontinuation
# ---------------------------------------------------------


# Load shared setup
source("00_setup.R")


# ---------------------------------------------------------
# 1. Set up CPRD environment and build second-line cohort
# ---------------------------------------------------------

cprd <- CPRDData$new(
  cprdEnv  = "diabetes-2024",
  cprdConf = "~/.aurum.yaml"
)

analysis <- cprd$analysis("mm")

# Load drug initiation data
drug_classes <- analysis$cached(name = "martha_20250327_t2d_1stinstance") %>% collect()

# Define second-line therapy initiation cohort
cohort <- define_cohort(
  data           = drug_classes,
  min_start_date = "2019-01-01"
)



# ---------------------------------------------------------
# 2. Prepare data for regression modelling
# ---------------------------------------------------------

# Creates hba1cresp12m_all and weightresp12m_all using 6-month values
# where the 12-month value is missing
cohort <- prepare_response_dataset(cohort)

# Imputes missing IMD and ethnicity using mice
cohort <- mice_imputation(cohort)



# ---------------------------------------------------------
# 3a. HbA1c response
# ---------------------------------------------------------

# Fit linear regression model for 12-month HbA1c response
# adjusting for the standard covariate set
model_hba1c <- fit_regression_model(
  data          = cohort,
  outcome       = "hba1c",
  response_time = "12m"
)

# Counterfactual marginal predictions by year × frailty group:
# For each patient, predict HbA1c response as if they had started
# therapy in each calendar year, holding all other covariates fixed.
# Each patient contributes one predicted value per year.
pred_hba1c <- average_predictions_by_year_frailty(
  model   = model_hba1c,
  data    = cohort,
  outcome = "hba1c"
)

# Compare predicted HbA1c response: 2023 vs 2019
#  1. Predict each person's response under 2019 initiation
#  2. Predict it again under 2023 initiation
#  3. Compute individual-level differences
#  4. Average differences within each frailty group
hba1c_comparison_2019_2023 <- year_contrast_by_frailty(
  model     = model_hba1c,
  data      = cohort,
  outcome   = "hba1c",
  from_year = 2019,
  to_year   = 2023
)
hba1c_comparison_2019_2023

# Plot marginal HbA1c response trends over time by frailty group
hba1c_response_plot <- plot_response_outcome(
  preds    = pred_hba1c,
  y_label  = "HbA1c response (mmol/mol)",
  y_limits = c(-15, 0),
  y_breaks = seq(-15, 0, by = 3)
)
hba1c_response_plot



# ---------------------------------------------------------
# 3b. Weight response
# ---------------------------------------------------------

# Fit regression model for 12-month weight change
# adjusting for the standard covariate set + baseline weight
model_weight <- fit_regression_model(
  data          = cohort,
  outcome       = "weight",
  response_time = "12m"
)

# Counterfactual predictions by year × frailty
pred_weight <- average_predictions_by_year_frailty(
  model   = model_weight,
  data    = cohort,
  outcome = "weight"
)

# Compare predicted weight change: 2023 vs 2019
weight_comparison_2019_2023 <- year_contrast_by_frailty(
  model     = model_weight,
  data      = cohort,
  outcome   = "weight",
  from_year = 2019,
  to_year   = 2023
)
weight_comparison_2019_2023

# Plot weight change trends
weight_response_plot <- plot_response_outcome(
  preds    = pred_weight,
  y_label  = "Weight change (kg)",
  y_limits = c(-5, 0),
  y_breaks = seq(-5, 0, by = 1)
)
weight_response_plot



# ---------------------------------------------------------
# 3c. Treatment discontinuation
# ---------------------------------------------------------

# Fit logistic regression model for 12-month discontinuation
# adjusting for the standard covariate set.
# Note: only years 2019–2022 are included (requires ≥12 months + 3m FU).
model_discontinuation <- fit_regression_model(
  data          = cohort,
  outcome       = "discontinuation",
  response_time = "12m"
)

# Counterfactual probabilities by year × frailty group
pred_disc <- average_predictions_by_year_frailty(
  model   = model_discontinuation,
  data    = cohort,
  outcome = "discontinuation"
)

# Compare discontinuation probability: 2022 vs 2019
disc_comparison_2019_2022 <- year_contrast_by_frailty(
  model     = model_discontinuation,
  data      = cohort,
  outcome   = "discontinuation",
  from_year = 2019,
  to_year   = 2022
)
disc_comparison_2019_2022

# Plot discontinuation trends
discontinuation_plot <- plot_response_outcome(
  preds    = pred_disc,
  y_label  = "Treatment discontinuation (%)",
  y_limits = c(0, 0.5),
  y_breaks = seq(0, 0.5, by = 0.1)
)
discontinuation_plot

