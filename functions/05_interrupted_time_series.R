# ---------------------------------------------------------
# functions/04_interrupted_time_series_functions.R
# Functions to calculate monthly SGLT2 initiations and 
# create variables used for ITS model
# ---------------------------------------------------------


#' Monthly SGLT2 proportions from the second-line cohort
#'
#' Starting from second-line cohort, this function:
#'   - extracts calendar year and month of initiation (dstartdate)
#'   - counts how many people start each drug class per year-month
#'   - computes the total number of second-line initiations per year-month
#'   - calculates the percentage of initiations which are SGLT2i
#'   - returns one row per month with a Date and SGLT2 proportion (%)
#'   
#' @param cohort      Output from define_cohort().
#' @param study_start Start date of the study window (Date).
#' @param study_end   End date of the study window (Date).
#'
#' @return Tibble with one row per month and columns including:
#'         year, month, drug_class, new_prescriptions,
#'         total_prescriptions, proportion, date.
make_monthly_sglt2_proportions <- function(cohort,
                                           study_start,
                                           study_end) {
  
  study_start <- as.Date(study_start)
  study_end   <- as.Date(study_end)
  
  cohort %>%
    dplyr::mutate(
      year  = format(as.Date(dstartdate), "%Y"),
      month = format(as.Date(dstartdate), "%m")
    ) %>%
    dplyr::group_by(year, month, drug_class) %>%
    dplyr::summarise(
      new_prescriptions = dplyr::n(),
      .groups           = "drop"
    ) %>%
    dplyr::group_by(year, month) %>%
    dplyr::mutate(
      total_prescriptions = sum(new_prescriptions, na.rm = TRUE),
      proportion          = 100 * new_prescriptions / total_prescriptions
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(drug_class == "SGLT2") %>%
    dplyr::mutate(
      Date = zoo::as.yearmon(paste(year, month), "%Y %m"),
      day  = 1L,
      date = lubridate::make_date(as.integer(year),
                                  as.integer(month),
                                  day),
      .keep = "all"
    ) %>%
    dplyr::arrange(date)
}




#' Add ITS time and lagged intervention variables
#'
#' Creates variables required for an interrupted time series model:
#'   * `time`: sequential month number  
#'   * `intervention`: 1 after (and including) the intervention date, else 0  
#'   * `post_time_intervention`: months since intervention began  
#'   * A lagged version of both `intervention` and `post_time_intervention`
#'     to allow for delayed implementation effects  
#'
#' @param df Monthly dataset (e.g., from `make_monthly_sglt2_proportions()`).
#' @param intervention_date Date of intervention.
#' @param lag Number of months to lag the effect (default: 1).
#'
#' @return Tibble with ITS variables added.
add_lagged_its_variables <- function(df,
                                     intervention_date,
                                     lag = 1L) {
  
  intervention_date <- as.Date(intervention_date)
  
  df <- df %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      # Sequential month index
      time = dplyr::row_number(),
      
      # Indicator for post-intervention period
      intervention = ifelse(date >= intervention_date, 1L, 0L),
      
      # Number of months since intervention (0 before)
      post_time_intervention = dplyr::if_else(
        intervention == 1L,
        time - min(time[intervention == 1L]) + 1L,
        0L
      )
    )
  
  # Add lagged variables 
  df %>%
    dplyr::mutate(
      intervention_lag1           = dplyr::lag(intervention, n = lag),
      post_time_intervention_lag1 = dplyr::lag(post_time_intervention, n = lag)
    ) %>%
    dplyr::filter(
      !is.na(intervention_lag1),
      !is.na(post_time_intervention_lag1)
    )
}