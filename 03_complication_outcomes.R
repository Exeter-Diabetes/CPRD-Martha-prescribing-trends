# ---------------------------------------------------------
# 03_complication_outcomes.R
#
# This script:
#   1. Loads CPRD data & builds the second-line cohort
#   2. Prepares data for complication modelling
#   3. For each complication outcome:
#        - fits a Poisson rate model
#        - obtains marginal predictions by year × frailty
#        - computes adjusted 2022 vs 2019 rate differences
#        - plots adjusted rates over time
#      Outcomes:
#        (a) Composite UKPDS severe diabetes-related complications
#        (b) Heart failure composite
#        (c) Kidney failure composite
# ---------------------------------------------------------

# Load shared setup
source("00_setup.R")


# ---------------------------------------------------------
# 1. Set up CPRD environment and second-line cohort
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
# 2. Prepare data for complication modelling
# ---------------------------------------------------------

# To be consistent with response data pipeline
cohort <- prepare_response_dataset(cohort)
cohort <- mice_imputation(cohort)

# Restrict complication analysis to patients with linked HES data (with_hes == 1)
data_hes <- prepare_complication_data(cohort)

# Add composite outcome definitions and follow-up time:
# For each individual:
#   - UKPDS composite outcome date:
#       * first severe diabetes-related event defined by UKPDS criteria
#   - Heart failure composite:
#       * HHF admission OR HF primary-cause death
#   - Kidney failure composite:
#       * CKD5 diagnosis OR KF primary-cause death
#
# Censoring date is the earliest of:
#   * 1 year after second-line therapy initiation
#   * GP registration end
#   * therapy discontinuation
#   * death
#   * end of HES follow-up
#
# For each outcome, creates:
#   * event flag (1 if outcome before censoring)
#   * follow-up end date (earliest of event or censor)
#   * person-years contributed to the analysis
data_hes <- create_composite_outcomes(data_hes)

# Create outcome-specific datasets
# Remove individuals with negative follow-up time (e.g. therapy initiation after hes_end_date)
#   ds$ukpds : valid follow-up for UKPDS outcome
#   ds$hf    : valid follow-up for heart failure outcome 
#   ds$renal : valid follow-up for kidney failure outcome
ds <- make_outcome_datasets(data_hes)



# ---------------------------------------------------------
# 3a. Severe diabetes-related complications (UKPDS composite)
# ---------------------------------------------------------

# Fit Poisson model for rate of severe diabetes-related complications
# adjusting for the standard covariate set
model_ukpds <- fit_poisson_rate_model(
  data        = ds$ukpds,
  outcome_var = "event_ukpds",
  offset_var  = "person_years_ukpds"
)

# Marginal predictions by year × frailty (per 1,000 PY)
# For each individual:
#   - predict rate as if therapy began in each calendar year
#   - hold all other covariates fixed
#   - produce one prediction per year per individual
# Then average predictions within year × frailty groups.
pred_ukpds <- poisson_predictions_by_year_frailty(
  model = model_ukpds,
  data  = ds$ukpds
)

# Adjusted rate differences: 2022 vs 2019 (per 1,000 PY)
#   1. Predict each individual under 2019 initiation
#   2. Predict again under 2022 initiation
#   3. Compute individual differences
#   4. Average within frailty groups
ukpds_contrast_2019_2022 <- year_contrast_by_frailty_poisson(
  model     = model_ukpds,
  data      = ds$ukpds,
  from_year = 2019,
  to_year   = 2022
)
ukpds_contrast_2019_2022

# Plot UKPDS composite rates over time
ukpds_complication_plot <- plot_complication_outcome(
  preds   = pred_ukpds,
  y_label = "Rate of composite diabetes-related\ncomplications (per 1,000 person-years)"
)



# ---------------------------------------------------------
# 3b. Heart failure 
# ---------------------------------------------------------

# Fit Poisson model for heart failure 
model_hf <- fit_poisson_rate_model(
  data        = ds$hf,
  outcome_var = "event_hf",
  offset_var  = "person_years_hf"
)

# Marginal predictions by year × frailty
pred_hf <- poisson_predictions_by_year_frailty(
  model = model_hf,
  data  = ds$hf
)

# Adjusted rate differences: 2022 vs 2019
hf_contrast_2019_2022 <- year_contrast_by_frailty_poisson(
  model     = model_hf,
  data      = ds$hf,
  from_year = 2019,
  to_year   = 2022
)
hf_contrast_2019_2022

# Plot HF composite rates
hf_complication_plot <- plot_complication_outcome(
  preds   = pred_hf,
  y_label = "Rate of heart failure hospitalisation\n(per 1,000 person-years)"
)



# ---------------------------------------------------------
# 3c. Kidney failure 
# ---------------------------------------------------------

# Fit Poisson model for kidney failure 
model_kidney <- fit_poisson_rate_model(
  data        = ds$renal,
  outcome_var = "event_renal",
  offset_var  = "person_years_renal"
)

# Marginal predictions by year × frailty
pred_kidney <- poisson_predictions_by_year_frailty(
  model = model_kidney,
  data  = ds$renal
)

# Adjusted rate differences: 2022 vs 2019
kidney_contrast_2019_2022 <- year_contrast_by_frailty_poisson(
  model     = model_kidney,
  data      = ds$renal,
  from_year = 2019,
  to_year   = 2022
)
kidney_contrast_2019_2022

# Plot kidney complication rates
kidney_complication_plot <- plot_complication_outcome(
  preds   = pred_kidney,
  y_label = "Rate of kidney failure\n(per 1,000 person-years)"
)