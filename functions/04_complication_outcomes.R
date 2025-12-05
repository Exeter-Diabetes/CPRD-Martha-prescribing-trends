# ---------------------------------------------------------
# functions/04_complication_outcomes.R
# Contains functions to creates composite complication outcomes
# and estimates incidence rates using Poisson models
# ---------------------------------------------------------


#' Prepare dataset for complication analysis
#' - Restricts to patients with linked HES data (with_hes == 1)
#' - Creates death date variables where the flag == 1.
#'
#' @param cohort Tibble from `define_cohort()`.
#' @return Tibble restricted to HES-linked individuals with cause-of-death dates added.
prepare_complication_data <- function(cohort) {
  
  # Filter for patients with linked HES data
  data_hes <- cohort %>%
    dplyr::filter(with_hes == 1) 
  
  # If primary cause of death == 1, work out death date from that cause
  data_hes <- data_hes %>%
    dplyr::mutate(
      cv_death_primary_cause_date =
        dplyr::if_else(cv_death_primary_cause == 1, death_date, as.Date(NA)),
      pvd_death_primary_cause_date =
        dplyr::if_else(pvd_death_primary_cause == 1, death_date, as.Date(NA)),
      sudden_death_primary_cause_date =
        dplyr::if_else(sudden_death_primary_cause == 1, death_date, as.Date(NA)),
      hyperglycaemia_death_primary_cause_date =
        dplyr::if_else(hyperglycaemia_death_primary_cause == 1, death_date, as.Date(NA)),
      hypoglycaemia_death_primary_cause_date =
        dplyr::if_else(hypoglycaemia_death_primary_cause == 1, death_date, as.Date(NA)),
      kf_death_primary_cause_date =
        dplyr::if_else(kf_death_primary_cause == 1, death_date, as.Date(NA)),
      hf_death_primary_cause_date =
        dplyr::if_else(hf_death_primary_cause == 1, death_date, as.Date(NA))
    )
  
  data_hes
}
  



#' Create composite outcome definitions and follow-up times
#'   1. Outcome date: earliest of all GP/HES events  
#'   2. Censoring date: earliest of 1-year post-index, death,
#'      practice exit, therapy discontinuation, or end of HES coverage  
#'   3. Event indicator: outcome_date ≤ censor_date  
#'   4. Follow-up time: min(outcome_date, censor_date) − index_date  
#'
#' Composite outcomes:
#'   - UKPDS severe diabetes-related complications 
#'   - Heart failure (HES HHF + HF primary-cause death)  
#'   - Renal failure (CKD5 diagnosis + kidney-failure death)  
#'   - DKA 
#'
#' @param data_hes Input HES-linked cohort.
#'
#' @return Dataset with event dates, event flags, and person-years for each outcome.
create_composite_outcomes <- function(data_hes) {
  
  
  # Comorbidies defining the UKPDS severe complication 
  ukpds_components <- c(
    "cv_death_primary_cause_date",
    "pvd_death_primary_cause_date",
    "sudden_death_primary_cause_date",
    "hyperglycaemia_death_primary_cause_date",
    "hypoglycaemia_death_primary_cause_date",
    "postdrug_first_primary_incident_mi",
    "postdrug_first_primary_hhf",
    "hf_death_primary_cause_date",
    "postdrug_first_primary_incident_stroke.y",
    "kf_death_primary_cause_date",
    "postdrug_first_ckd5_code",
    "postdrug_first_amputation",
    "postdrug_first_vitreoushemorrhage",
    "postdrug_first_photocoagulation"
  )
  
  # Censors
  censor_vars <- c(
    "one_year_from_therapy",
    "gp_end_date",
    "dstopdate_class",
    "death_date",
    "hes_end_date"
  )
  
  
  data_hes <- data_hes %>%
    # Baseline censoring at 1 year after therapy initiation
    mutate(one_year_from_therapy = dstartdate + lubridate::years(1)) %>%
    
    # UKPDS composite
    mutate(
      ukpds_outcome_date = row_min_date(select(., all_of(ukpds_components))),
      censor_date        = row_min_date(select(., all_of(censor_vars))),
      event_ukpds        = as.integer(!is.na(ukpds_outcome_date) & ukpds_outcome_date <= censor_date),
      followup_end_ukpds = if_else(event_ukpds == 1, ukpds_outcome_date, censor_date),
      person_years_ukpds = as.numeric(difftime(followup_end_ukpds, dstartdate, units = "days")) / 365.25,
      
      # Heart failure composite
      heart_failure_date = safe_pmin_date(postdrug_first_primary_hhf, hf_death_primary_cause_date),
      event_hf           = as.integer(!is.na(heart_failure_date) & heart_failure_date <= censor_date),
      followup_end_hf    = if_else(event_hf == 1, heart_failure_date, censor_date),
      person_years_hf    = as.numeric(difftime(followup_end_hf, dstartdate, units="days")) / 365.25,
      
      # Renal composite
      renal_date         = safe_pmin_date(postdrug_first_ckd5_code, kf_death_primary_cause_date),
      event_renal        = as.integer(!is.na(renal_date) & renal_date <= censor_date),
      followup_end_renal = if_else(event_renal == 1, renal_date, censor_date),
      person_years_renal = as.numeric(difftime(followup_end_renal, dstartdate, units="days")) / 365.25,
      
      # DKA
      event_dka          = as.integer(!is.na(postdrug_first_dka) & postdrug_first_dka <= censor_date),
      followup_end_dka   = if_else(event_dka == 1, postdrug_first_dka, censor_date),
      person_years_dka   = as.numeric(difftime(followup_end_dka, dstartdate, units="days")) / 365.25
    )
  
  data_hes
}



#' Create outcome-specific analysis datasets
#'
#' Remove people where person_years < 0 (i.e they initiated therapy after hes_end_date)
#'
#' @param data_hes Output from create_composite_outcomes().
#'
#' @return Named list of tibbles.
make_outcome_datasets <- function(data_hes) {
  list(
    ukpds = data_hes %>% filter(person_years_ukpds > 0),
    hf    = data_hes %>% filter(person_years_hf > 0),
    renal = data_hes %>% filter(person_years_renal > 0),
    dka   = data_hes %>% filter(person_years_dka > 0)
  )
}



#' Fit a Poisson regression model for incidence rate adjusting for standard covariate set
#'
#' The offset is the log of person-years of follow-up.
#'
#' @param data        Dataset for outcome.
#' @param outcome_var Name of binary event variable.
#' @param offset_var  Name of person-years variable.
#'
#' @return glm() object (Poisson family).
fit_poisson_rate_model <- function(data,
                                   outcome_var,
                                   offset_var) {
  
  standard_covariate_set <- c(
    "factor(year) * predrug_frail_elderly_cat",
    "dstartdate_age",
    "diabetes_duration",
    "factor(ethnicity_5cat)",
    "gender",
    "imd_decile",
    "predrug_cvd",
    "predrug_primary_incident_stroke.y",
    "predrug_hf_all_cause",
    "predrug_ckd",
    "predrug_ckd5_code",
    "predrug_amputation",
    "predrug_vitreoushemorrhage",
    "predrug_photocoagulation",
    "predrug_hypoglycaemia",
    "predrug_ukpds_hyperglycaemia"
  )
  
  rhs  <- paste(standard_covariate_set, collapse = " + ")
  form <- as.formula(
    paste(outcome_var, "~", paste(rhs, collapse = " + "))
  )
  
  glm(
    formula = form,
    family  = poisson(link = "log"),
    offset  = log(data[[offset_var]]),
    data    = data
  )
}


#' Counterfactual average predicted rates per 1,000 person-years
#'
#' For each individual, this computes counterfactual predicted rates as if
#' they had initiated therapy in each calendar year (with all other covariates
#' held at their observed values).
#'
#' Each individual contributes one prediction per year, and predictions are
#' averaged over individuals within each year × frailty group.
#'
#' @param model Fitted Poisson model.
#' @param data  Dataset used for predictions.
#'
#' @return Tibble with marginal predicted rates.
poisson_predictions_by_year_frailty <- function(model, data) {
  
  avg_predictions(
    model,
    variables = "year",
    by        = c("year", "predrug_frail_elderly_cat"),
    newdata   = data
  ) %>%
    mutate(across(c(estimate, conf.low, conf.high), ~ . * 1000)) %>%
    filter(year != 2023)
}



#' Counterfactual rate differences between two calendar years
#'
#' Computes adjusted contrasts of predicted rates:
#'
#'   - For each individual, predict outcomes as if therapy began in  
#'     `from_year` and again in `to_year`  
#'   - Compute the rate difference  
#'   - Average differences within each frailty group  
#'
#' Results are expressed per 1,000 person-years.
#'
#' @param model     Poisson model.
#' @param data      Dataset for predictions.
#' @param from_year Reference year (e.g., 2019).
#' @param to_year   Comparison year (e.g., 2022).
#'
#' @return Tibble of rate differences by frailty group.
year_contrast_by_frailty_poisson <- function(model,
                                             data,
                                             from_year = 2019,
                                             to_year   = 2022) {
  
  year_vals <- c(from_year, to_year)
  
  cmp <- marginaleffects::comparisons(
    model,
    variables = list(year = year_vals),
    by        = "predrug_frail_elderly_cat",
    newdata   = data
  )
  
  cmp %>%
    dplyr::mutate(
      dplyr::across(
        .cols = c(estimate, conf.low, conf.high),
        .fns  = ~ . * 1000
      )
    )
}




