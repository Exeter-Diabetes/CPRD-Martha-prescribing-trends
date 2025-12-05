# ---------------------------------------------------------
# 04_interrupted_time_series.R
#
# Interrupted time series (ITS) analysis of second-line SGLT2i use.
#
# This script:
#   1. Loads CPRD data & builds the second-line cohort
#   2. Calculates monthly proportions of SGLT2i initiations
#   3. Creates ITS variables:
#        - time 
#        - intervention indicator
#        - months since intervention
#   4. Fits:
#        (a) ITS model with AR(1) 
#        (b) Counterfactual model using only pre-intervention trend
#   5. Predicts factual vs counterfactual trends and plots them
# ---------------------------------------------------------

# Load shared setup 
source("00_setup.R")

# ---------------------------------------------------------
# 1. CPRD setup and second-line cohort
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
  min_start_date = "2019-01-01",
  verbose        = TRUE
)

# ---------------------------------------------------------
# 2. Monthly SGLT2 initiation proportions
# ---------------------------------------------------------

# Study window & intervention date 
study_start       <- as.Date("2019-01-01")  # Study start date
study_end         <- as.Date("2024-03-01")  # Study end date
intervention_date <- as.Date("2022-02-01")  # NICE guideline update



# Calculates, for each calendar month:
#   - number of SGLT2i initiations
#   - total second-line initiations
#   - % of initiations that are SGLT2i
its_df <- make_monthly_sglt2_proportions(
  cohort      = cohort,
  study_start = study_start,
  study_end   = study_end
)




# ---------------------------------------------------------
# 3. Create ITS variables
# ---------------------------------------------------------

# Adds:
#   - time: month index (1, 2, 3, …)
#   - intervention: 0 before guideline month, 1 after
#   - post_time_intervention: months since intervention began
#   - lagged versions of intervention and post_time_intervention
# Uses a 1-month lag, so the intervention effect starts one month 
# after the guidelines change (to reflect slight delay in uptake).
its_df <- add_lagged_its_variables(
  df                = its_df,
  intervention_date = intervention_date,
  lag               = 1L
)


# ---------------------------------------------------------
# 4a. ITS model with AR(1) 
# ---------------------------------------------------------

# Model terms:
#   - time: underlying trend
#   - intervention_lag1: immediate level change after the (lagged) intervention
#   - post_time_intervention_lag1: change in slope after the intervention
#
# The model includes AR(1) autocorrelation:
#   - prescribing in one month tends to be similar to the month before
its_model <- nlme::gls(
  proportion ~ time + intervention_lag1 + post_time_intervention_lag1,
  data       = its_df,
  correlation = nlme::corARMA(p = 1, q = 0, form = ~ time),
  method      = "ML"
)

summary(its_model)




# ---------------------------------------------------------
# 4b. Counterfactual model (pre-intervention only)
# ---------------------------------------------------------
#
# Fits the same type of AR(1) model, but restricted to the pre-intervention
# period. This represents the trend we would expect to continue in the absence
# of the guideline change.
pre_intervention_data <- its_df %>%
  dplyr::filter(date < intervention_date)

counterfactual_model <- nlme::gls(
  proportion ~ time,
  data       = pre_intervention_data,
  correlation = nlme::corARMA(p = 1, q = 0, form = ~ time),
  method      = "ML"
)

summary(counterfactual_model)



# ---------------------------------------------------------
# 5. Observed and counterfactual predictions
# ---------------------------------------------------------

observed_pred <- AICcmodavg::predictSE.gls(
  its_model,
  newdata = its_df,
  se.fit  = TRUE
)

counterfactual_pred <- AICcmodavg::predictSE.gls(
  counterfactual_model,
  newdata = its_df,
  se.fit  = TRUE
)

its_df <- its_df %>%
  dplyr::mutate(
    fit_its      = as.numeric(observed_pred$fit),
    se_its       = as.numeric(observed_pred$se),
    fit_counterf = as.numeric(counterfactual_pred$fit),
    se_counterf  = as.numeric(counterfactual_pred$se),
    
    
    # Observed
    factual_predictions = ifelse(
      intervention == 0,
      fit_counterf,
      fit_its
    ),
    factual_se = ifelse(
      intervention == 0,
      se_counterf,
      se_its
    ),
    factual_lower = factual_predictions - 1.96 * factual_se,
    factual_upper = factual_predictions + 1.96 * factual_se,
    
    # Counterfactual 
    counterfactual_predictions = fit_counterf,
    counterfactual_se          = se_counterf,
    counterfactual_lower       = counterfactual_predictions - 1.96 * counterfactual_se,
    counterfactual_upper       = counterfactual_predictions + 1.96 * counterfactual_se
  )


# ---------------------------------------------------------
# 6. Plot interrupted time series
# ---------------------------------------------------------
interrupted_time_series_plot <- plot_interrupted_time_series(
  its_df            = its_df,
  intervention_date = intervention_date
)

interrupted_time_series_plot
