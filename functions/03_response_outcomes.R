# -----------------------------------------------
# functions/03_response_outcomes.R
# Contains functions to prepare response outcomes, impute missing data,
# fit regression models and generate counterfactual predictions
# -----------------------------------------------


#' Prepare response outcome dataset
#'
#' @param cohort Tibble from `define_cohort()`.
#' @return Tibble with completed HbA1c/weight responses and ethnicity recoded.
prepare_response_dataset <- function(cohort) {
  cohort %>%
    dplyr::mutate(
      # Use 6-month response when 12-month is missing
      hba1cresp12m_all = dplyr::if_else(
        is.na(hba1cresp12m) & !is.na(hba1cresp6m),
        hba1cresp6m,
        hba1cresp12m
      ),
      weightresp12m_all = dplyr::if_else(
        is.na(weightresp12m) & !is.na(weightresp6m),
        weightresp6m,
        weightresp12m
      ),
      # Treat missing ethnicity as "Missing"
      ethnicity_5cat = forcats::fct_na_value_to_level(
        as.factor(ethnicity_5cat),
        level = "Missing"
      )
    )
}



#' Impute missing IMD and ethnicity using `mice`
#'
#' @param data Dataset returned by `prepare_response_dataset()`.
#' @param m Number of imputations.
#' @param seed Random seed for reproducibility.
#'
#' @return Tibble with imputed ethnicity and IMD.
mice_imputation <- function(data,
                            m    = 5,
                            seed = 123) {
  
  # Variables used for imputation models
  vars <- c(
    "patid",
    "imd_decile",
    "prehba1c",
    "prebmi",
    "preweight",
    "ethnicity_5cat",
    "gender",
    "diabetes_duration",
    "dstartdate_age",
    "drug_class",
    "predrug_cvd",
    "predrug_ckd",
    "efi_n_deficits"
  )
  
  data_to_impute <- data[, vars]
  
  # Initialise 
  init <- mice::mice(data_to_impute, maxit = 0, printFlag = FALSE)
  meth <- init$method
  pred <- init$predictorMatrix
  
  # Specify which variables to impute
  meth["imd_decile"]      <- "polr"      # ordered
  meth["ethnicity_5cat"]  <- "polyreg"   # unordered multinomial
  
  # Do not impute patid or use it as predictor
  pred["patid", ] <- 0
  pred[, "patid"] <- 0
  
  # Variables used as predictors only
  meth["prehba1c"]   <- ""
  meth["prebmi"]     <- ""
  meth["preweight"]  <- ""
  
  # Run imputation
  imputed <- mice::mice(
    data_to_impute,
    method          = meth,
    predictorMatrix = pred,
    m               = m,
    seed            = seed,
    printFlag       = FALSE
  )
  
  # Get a completed dataset
  completed <- mice::complete(imputed, action = 2)
  
  # Append imputed fields to original data
  data %>%
    dplyr::select(-imd_decile, -ethnicity_5cat) %>%
    dplyr::left_join(
      completed %>%
        dplyr::select(patid, ethnicity_5cat, imd_decile),
      by = "patid"
    )
}



# Fit regression models
#
# - All models adjust for a standard covariate set 
# - HbA1c and weight are modelled with linear regression.
# - Discontinuation is modelled with logistic regression and restricted to
#   years 2019–2022, as these are the years with complete follow-up.
#
# The `response_time` argument switches between 6- and 12-month response.
#
#' @param data Analysis dataset.
#' @param outcome "hba1c", "weight", or "discontinuation".
#' @param response_time "12m" (default) or "6m".
#'
#' @return Fitted `lm` or `glm` object.
fit_regression_model <- function(data,
                                 outcome       = c("hba1c", "weight", "discontinuation"),
                                 response_time = c("12m", "6m")) {
  
  outcome       <- match.arg(outcome)
  response_time <- match.arg(response_time)
  
  # Choose outcome variable based 6- or 12-month response
  outcome_var <- switch(
    outcome,
    hba1c = if (response_time == "12m") "hba1cresp12m_all" else "hba1cresp6m",
    weight = if (response_time == "12m") "weightresp12m_all" else "weightresp6m",
    discontinuation = if (response_time == "12m") "stopdrug_12m_3mFU" else "stopdrug_6m_3mFU"
  )
  
  # Standard covariate set
  base_terms <- c(
    "factor(year) * predrug_frail_elderly_cat",
    "ethnicity_5cat",
    "gender",
    "imd_decile",
    "prehba1c",
    "dstartdate_age",
    "diabetes_duration"
  )
  
  # Weight response with additional baseline weight adjustment
  rhs_terms <- base_terms
  if (outcome == "weight") rhs_terms <- c(rhs_terms, "preweight")
  
  form <- stats::as.formula(paste(outcome_var, "~", paste(rhs_terms, collapse = " + ")))
  
  # Fit models
  if (outcome == "discontinuation") {
    
    data_use <- data %>%
      dplyr::filter(year %in% c(2019, 2020, 2021, 2022)) %>%
      droplevels()
    
    stats::glm(form, data = data_use, family = stats::binomial())
    
  } else {
    
    stats::lm(form, data = data)
  }
}



# Compute counterfactual average predictions by year and frailty
# For each individual, we use the fitted model to predict the outcome as if they
# had initiated therapy in each calendar year while holding all other
# covariates fixed at their observed values.
#  – Each person has one prediction per year (e.g., 5 predictions for 2019–2023).
#  – For a given year and frailty group, we then average these predictions across
#    individuals to obtain a marginal estimate.
#
#' @param model Fitted model.
#' @param data Analysis dataset.
#' @param outcome "hba1c", "weight", or "discontinuation".
#'
#' @return Tibble of marginal predictions with confidence intervals.
average_predictions_by_year_frailty <- function(model,
                                                data,
                                                outcome = c("hba1c", "weight", "discontinuation")) {
  
  outcome <- match.arg(outcome)
  
  if (outcome == "discontinuation") {
    
    newdata <- data %>%
      dplyr::filter(year %in% c(2019, 2020, 2021, 2022)) %>%
      droplevels()
    
    marginaleffects::avg_predictions(
      model,
      variables = "year",
      by        = c("year", "predrug_frail_elderly_cat"),
      newdata   = newdata,
      type      = "response"
    )
    
  } else {
    
    marginaleffects::avg_predictions(
      model,
      variables = "year",
      by        = c("year", "predrug_frail_elderly_cat"),
      newdata   = data
    )
  }
}



# Year contrasts by frailty group
#
# Compare two calendar years (e.g., 2019 vs 2023) for each frailty group
# For each individual, we compute counterfactual predictions as if they had 
# started therapy in 'from_year' and in 'to_year' while holding all other 
# covariates fixed. The difference between these two predictions is then 
# averaged within each frailty group.
#
#' @param model Fitted model.
#' @param data Analysis dataset.
#' @param outcome "hba1c", "weight", or "discontinuation".
#' @param from_year Baseline year.
#' @param to_year Comparison year.
#'
#' @return Tibble of contrasts with confidence intervals.
year_contrast_by_frailty <- function(model,
                                     data,
                                     outcome   = c("hba1c", "weight", "discontinuation"),
                                     from_year = 2019,
                                     to_year   = 2023) {
  
  outcome <- match.arg(outcome)
  year_vals <- c(from_year, to_year)
  
  if (outcome == "discontinuation") {
    
    data_use <- data %>%
      dplyr::filter(year %in% c(2019, 2020, 2021, 2022)) %>%
      droplevels()
    
    cmp <- marginaleffects::comparisons(
      model,
      variables = list(year = year_vals),
      by        = "predrug_frail_elderly_cat",
      newdata   = data_use,
      type      = "response"
    )
    
    cmp %>%
      dplyr::mutate(
        dplyr::across(
          .cols = c(estimate, conf.low, conf.high),
          .fns  = ~ . * 100
        )
      )
    
  } else {
    
    marginaleffects::comparisons(
      model,
      variables = list(year = year_vals),
      by        = "predrug_frail_elderly_cat",
      newdata   = data
    )
  }
}