# -----------------------------------------------
# functions/01_cohort_definition.R
# Build cohort of individuals with type 2 diabetes
# initiating second-line therapy after metformin
# -----------------------------------------------

#' Define second-line therapy cohort
#'
#' @param data CPRD dataset.
#' @param min_start_date second-line therapy initiation date to start from ("YYYY-MM-DD").
#' @param verbose Logical; print patient.
#'
#' @return Tibble containing the cleaned and filtered cohort.
define_cohort <- function(data,
                          min_start_date = "2019-01-01",
                          verbose        = TRUE) {
  
  min_start_date <- as.Date(min_start_date)
  
  # Helper to print patient counts at processing steps
  print_counts <- function(df, label) {
    if (!verbose) return(invisible(NULL))
    cat(sprintf("%s: %s patients\n",
                label, format(length(df$patid), big.mark = ",")))
  }
  
  # Basic cleaning
  # Remove undefined gender code, convert patid to character,
  data <- data %>%
    dplyr::filter(gender != 3) %>%
    dplyr::mutate(patid = as.character(patid)) %>%
    convert_integer64_to_integer()
  
  # Identify individuals with metformin as first-line therapy
  metformin_first_line <- data %>%
    dplyr::filter(
      drug_class    == "MFN",
      drugline_all  == 1,
      drug_instance == 1
    ) %>%
    dplyr::distinct(patid)
  
  # Apply inclusion criteria for second-line therapy initiation
  data <- data %>%
    dplyr::filter(
      dm_diag_date_all > regstartdate,         # diagnosed after registration
      drugline_all     == 2,                   # second-line therapy
      drug_instance    == 1,                   # first occurrence of second-line therapy
      as.Date(dstartdate) >= min_start_date,   # study window
      dstartdate_age   >= 18                   # adults
    )
  
  print_counts(data, "Second-line therapy initiations from study start date")
  
  # Recode infrequent drug classes as "Other"
  data <- data %>%
    dplyr::mutate(
      drug_class = dplyr::if_else(
        drug_class %in% c("Acarbose", "GIPGLP1", "Glinide", "INS", "TZD"),
        "Other",
        drug_class
      )
    )
  
  # Restrict to people whose first-line therapy was metformin
  data <- data %>%
    dplyr::semi_join(metformin_first_line, by = "patid")
  
  print_counts(data, "After restricting to patients with metformin first-line")
  
  # Add baseline covariates
  data <- data %>%
    
    # Diabetes duration
    dplyr::mutate(
      diabetes_duration = as.numeric(
        difftime(as.Date(dstartdate), as.Date(dm_diag_date), units = "days")
      ) / 365.25,
      year = as.numeric(format(as.Date(dstartdate), "%Y")),
      age_group = factor(
        dplyr::if_else(dstartdate_age > 70, "Age > 70", "Age <= 70"),
        levels = c("Age <= 70", "Age > 70")
      )
    ) %>%
    
    # Sex (male or female)
    dplyr::mutate(
      sex = dplyr::case_when(
        gender == 1 ~ "Male",
        gender == 2 ~ "Female",
        TRUE        ~ NA_character_
      ),
      sex = factor(sex, levels = c("Male", "Female"))
    ) %>%
    
    # Cardiovascular, renal, eye disease, and heart failure history
    dplyr::mutate(
      predrug_cvd = dplyr::if_else(
        predrug_myocardialinfarction == 1 |
          predrug_stroke == 1 |
          predrug_angina == 1 |
          predrug_ihd == 1 |
          predrug_pad == 1 |
          predrug_revasc == 1 |
          predrug_tia == 1 |
          predrug_primary_incident_mi == 1,
        1L, 0L
      ),
      predrug_cvd = dplyr::if_else(is.na(predrug_cvd), 0L, predrug_cvd),
      
      predrug_ckd = dplyr::if_else(
        preckdstage %in% c("stage_3a", "stage_3b", "stage_4"),
        1L, 0L
      ),
      
      predrug_severe_retinopathy = dplyr::if_else(
        predrug_vitreoushemorrhage == 1 |
          predrug_photocoagulation == 1,
        1L, 0L
      ),
      
      predrug_hf_all_cause = dplyr::if_else(
        predrug_heartfailure == 1 |
          predrug_primary_hhf == 1,
        1L, 0L
      )
    ) %>%
    
    # Convert comorbidities to factors
    dplyr::mutate(
      predrug_cvd = factor(predrug_cvd, levels = c(0L, 1L)),
      predrug_ckd = factor(predrug_ckd, levels = c(0L, 1L))
    ) %>%
    
    # Ethnicity 
    dplyr::mutate(
      ethnicity_5cat = dplyr::recode(
        as.character(ethnicity_5cat),
        "0" = "White",
        "1" = "South Asian",
        "2" = "Black",
        "3" = "Other",
        "4" = "Mixed",
        .default = NA_character_
      ),
      ethnicity_5cat = factor(
        ethnicity_5cat,
        levels = c("White", "South Asian", "Black", "Other", "Mixed")
      )
    ) %>%
    
    # Combined age + frailty subgroup 
    dplyr::mutate(
      predrug_frail_elderly_cat = dplyr::case_when(
        dstartdate_age <= 70                                     ~ "Age ≤ 70 years",
        dstartdate_age > 70 & predrug_efi_cat %in% c("fit", "mild") ~ "Non-frail > 70 years",
        dstartdate_age > 70 & predrug_efi_cat %in% c("moderate", "severe") ~ "Frail > 70 years",
        TRUE ~ NA_character_
      ),
      predrug_frail_elderly_cat = factor(
        predrug_frail_elderly_cat,
        levels = c("Age ≤ 70 years", "Non-frail > 70 years", "Frail > 70 years")
      )
    ) %>%
    
    # IMD quintiles
    dplyr::mutate(
      imd_decile = factor(imd_decile, levels = 1:10, ordered = TRUE)
    ) %>%
    dplyr::mutate(
      imd_quintile = dplyr::case_when(
        imd_decile %in% 1:2   ~ "1-2",
        imd_decile %in% 3:4   ~ "3-4",
        imd_decile %in% 5:6   ~ "5-6",
        imd_decile %in% 7:8   ~ "7-8",
        imd_decile %in% 9:10  ~ "9-10",
        TRUE ~ NA_character_
      ),
      imd_quintile = factor(
        imd_quintile,
        levels = c("1-2", "3-4", "5-6", "7-8", "9-10"),
        ordered = TRUE
      )
    )
  
  # Remove patients with >1 second-line therapy recorded on the same day
  data_cohort <- data %>%
    dplyr::group_by(patid) %>%
    dplyr::filter(dplyr::n() == 1) %>%
    dplyr::ungroup()
  
  print_counts(data_cohort, "After removing patients with >1 same-day second-line therapy")
  print_counts(data_cohort, "Final number of patients in cohort")
  
  data_cohort
}

