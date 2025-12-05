# -----------------------------------------------
# 02_descriptives.R
# Functions for descriptive summaries and prescribing trends
# -----------------------------------------------


#' Create a baseline descriptive Table 1
#'
#' @param cohort Data frame of the study cohort.
#' @param strata_var Variable used to stratify the table (character).
#' @param continuous_vars Optional vector of continuous variables.
#' @param categorical_vars Optional vector of categorical variables.
#' @param add_continuous Optional additional continuous variables.
#' @param add_categorical Optional additional categorical variables.
#' @param includeNA Logical; include missing values as a category.
#'
#' @return A `CreateTableOne` object.
make_table <- function(cohort,
                       strata_var       = "predrug_frail_elderly_cat",
                       continuous_vars  = NULL,
                       categorical_vars = NULL,
                       add_continuous   = NULL,
                       add_categorical  = NULL,
                       includeNA        = TRUE,
                       ...) {
  
  # Default continuous and categorical variables
  default_continuous <- c(
    "dstartdate_age",
    "prebmi",
    "preweight",
    "prehba1c",
    "diabetes_duration"
  )
  
  default_categorical <- c(
    "gender",
    "ethnicity_5cat",
    "imd_quintile",
    "predrug_cvd",
    "predrug_ckd",
    "predrug_ckd5_code",
    "predrug_hf_all_cause",
    "predrug_amputation",
    "predrug_primary_hyperglycaemia",
    "predrug_primary_hypoglycaemia",
    "predrug_severe_retinopathy",
    "predrug_efi_cat"
  )
  
  # Use defaults variables if no extra wanted
  if (is.null(continuous_vars))   continuous_vars  <- default_continuous
  if (is.null(categorical_vars))  categorical_vars <- default_categorical
  
  # Add any extra variables
  if (!is.null(add_continuous))   continuous_vars  <- unique(c(continuous_vars, add_continuous))
  if (!is.null(add_categorical))  categorical_vars <- unique(c(categorical_vars, add_categorical))
  
  vars       <- c(continuous_vars, categorical_vars)
  factorVars <- categorical_vars
  
  # Build Table 1 using tableone
  tableone::CreateTableOne(
    vars       = vars,
    factorVars = factorVars,
    strata     = strata_var,
    data       = cohort,
    includeNA  = includeNA,
    ...
  )
}




#' Calculate yearly prescribing proportions by group
#'
#' @param cohort Cohort output from `define_cohort()`.
#' @param group_var Name of grouping variable (character).
#'
#' @return Tibble with:
#'   year, group_var, drug_class, n, total, proportion.
drug_proportions <- function(cohort, group_var) {
  
  group_sym <- rlang::sym(group_var)
  
  cohort %>%
    # Count initiations by year × group × drug class
    dplyr::count(year, !!group_sym, drug_class, name = "n") %>%
    
    # Compute proportions within each year × group
    dplyr::group_by(year, !!group_sym) %>%
    dplyr::mutate(
      total      = sum(n),
      proportion = 100 * n / total
    ) %>%
    dplyr::ungroup()
}