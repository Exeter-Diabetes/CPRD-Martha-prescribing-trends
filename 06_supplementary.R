# ---------------------------------------------------------
# 06_supplementary.R
#
# This script produces the tables and figures for the
# supplementary material.
# ---------------------------------------------------------

# Load shared setup 
source("00_setup.R")


# ---------------------------------------------------------
# 1. CPRD environment and cohort
# ---------------------------------------------------------

cprd <- CPRDData$new(
  cprdEnv  = "diabetes-2024",
  cprdConf = "~/.aurum.yaml"
)

analysis <- cprd$analysis("mm")

# Load drug initiation data
drug_classes <- analysis$cached("martha_20250327_t2d_1stinstance") %>%
  dplyr::collect()

# Define second-line therapy initiation cohort
cohort <- define_cohort(
  data           = drug_classes,
  min_start_date = "2019-01-01"
)

# Ensure output folders exist
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures/supplementary", recursive = TRUE, showWarnings = FALSE)



# ---------------------------------------------------------
# Supplementary Table 1:
# Baseline characteristics by calendar year
# ---------------------------------------------------------

supplementary_table1 <- make_table(
  cohort,
  strata_var       = "year",
  categorical_vars = "drug_class"
)

print(
  supplementary_table1,
  showAllLevels = TRUE,
  quote         = FALSE,
  noSpaces      = TRUE
)

supplementary_table1_df <- print(
  supplementary_table1,
  showAllLevels = TRUE,
  quote         = FALSE,
  noSpaces      = TRUE,
  printToggle   = FALSE
)

write.csv(
  supplementary_table1_df,
  file      = "output/tables/supplementary_table1.csv",
  row.names = FALSE
)



# ---------------------------------------------------------
# Supplementary Table 2:
# Baseline characteristics with frailty split into
# moderate and severe
# ---------------------------------------------------------

cohort_severe <- cohort %>%
  dplyr::mutate(
    predrug_frail_elderly_cat = dplyr::case_when(
      dstartdate_age <= 70 ~ "Age ≤ 70 years",
      dstartdate_age > 70 & predrug_efi_cat %in% c("fit", "mild") ~ "Non-frail > 70 years",
      dstartdate_age > 70 & predrug_efi_cat == "moderate"         ~ "Moderate frailty > 70 years",
      dstartdate_age > 70 & predrug_efi_cat == "severe"           ~ "Severe frailty > 70 years"
    ),
    predrug_frail_elderly_cat = factor(
      predrug_frail_elderly_cat,
      levels = c(
        "Age ≤ 70 years",
        "Non-frail > 70 years",
        "Moderate frailty > 70 years",
        "Severe frailty > 70 years"
      )
    )
  )

supplementary_table2 <- make_table(
  cohort_severe,
  strata_var = "predrug_frail_elderly_cat"
)

print(
  supplementary_table2,
  showAllLevels = TRUE,
  quote         = FALSE,
  noSpaces      = TRUE
)

supplementary_table2_df <- print(
  supplementary_table2,
  showAllLevels = TRUE,
  quote         = FALSE,
  noSpaces      = TRUE,
  printToggle   = FALSE
)

write.csv(
  supplementary_table2_df,
  file      = "output/tables/supplementary_table2.csv",
  row.names = FALSE
)



# ---------------------------------------------------------
# Supplementary Figures 1–6:
# Prescribing trends in subgroups
# ---------------------------------------------------------

# Sex
trends_sex <- drug_proportions(cohort, group_var = "sex")

supplementary_figure1 <- plot_drug_trends_by_group(
  trends    = trends_sex,
  cohort    = cohort,
  group_var = "sex",
  y_lab     = "Second-line initiations (%)"
)

ggplot2::ggsave(
  filename = "output/figures/supplementary/supplementary_figure1.png",
  plot     = supplementary_figure1,
  width    = 14,
  height   = 8,
  dpi      = 600
)


# Ethnicity
trends_ethnicity <- drug_proportions(cohort, group_var = "ethnicity_5cat") %>%
  dplyr::filter(!is.na(ethnicity_5cat))

supplementary_figure2 <- plot_drug_trends_by_group(
  trends    = trends_ethnicity,
  cohort    = cohort,
  group_var = "ethnicity_5cat",
  y_lab     = "Second-line initiations (%)"
)

ggplot2::ggsave(
  filename = "output/figures/supplementary/supplementary_figure2.png",
  plot     = supplementary_figure2,
  width    = 14,
  height   = 8,
  dpi      = 600
)


# Deprivation (IMD quintile)
trends_deprivation <- drug_proportions(cohort, group_var = "imd_quintile") %>%
  dplyr::filter(!is.na(imd_quintile))

supplementary_figure3 <- plot_drug_trends_by_group(
  trends    = trends_deprivation,
  cohort    = cohort,
  group_var = "imd_quintile",
  y_lab     = "Second-line initiations (%)"
)

ggplot2::ggsave(
  filename = "output/figures/supplementary/supplementary_figure3.png",
  plot     = supplementary_figure3,
  width    = 14,
  height   = 8,
  dpi      = 600
)


# Pre-existing CVD
trends_cvd <- drug_proportions(cohort, group_var = "predrug_cvd")

supplementary_figure4 <- plot_drug_trends_by_group(
  trends    = trends_cvd,
  cohort    = cohort,
  group_var = "predrug_cvd",
  y_lab     = "Second-line initiations (%)"
)

ggplot2::ggsave(
  filename = "output/figures/supplementary/supplementary_figure4.png",
  plot     = supplementary_figure4,
  width    = 14,
  height   = 8,
  dpi      = 600
)


# Pre-existing CKD 
trends_ckd <- drug_proportions(cohort, group_var = "predrug_ckd")

supplementary_figure5 <- plot_drug_trends_by_group(
  trends    = trends_ckd,
  cohort    = cohort,
  group_var = "predrug_ckd",
  y_lab     = "Second-line initiations (%)"
)

ggplot2::ggsave(
  filename = "output/figures/supplementary/supplementary_figure5.png",
  plot     = supplementary_figure5,
  width    = 14,
  height   = 8,
  dpi      = 600
)



# Frailty (split into moderate / severe)
trends_severe_frailty <- drug_proportions(
  cohort_severe,
  group_var = "predrug_frail_elderly_cat"
)

supplementary_figure6 <- plot_drug_trends_by_group(
  trends    = trends_severe_frailty,
  cohort    = cohort_severe,
  group_var = "predrug_frail_elderly_cat",
  y_lab     = "Second-line initiations (%)"
)

ggplot2::ggsave(
  filename = "output/figures/supplementary/supplementary_figure6.png",
  plot     = supplementary_figure6,
  width    = 14,
  height   = 8,
  dpi      = 600
)



# ---------------------------------------------------------
# Supplementary Figure 7:
# 6-month response outcomes
# ---------------------------------------------------------

# Prepare response data (6m)
cohort6m <- prepare_response_dataset(cohort)
cohort6m <- mice_imputation(cohort6m)

# 6-month HbA1c response
model_hba1c_6m <- fit_regression_model(
  cohort6m,
  outcome       = "hba1c",
  response_time = "6m"
)

pred_hba1c_6m <- average_predictions_by_year_frailty(
  model   = model_hba1c_6m,
  data    = cohort6m,
  outcome = "hba1c"
)

hba1c_response_plot_6m <- plot_response_outcome(
  preds    = pred_hba1c_6m,
  y_label  = "HbA1c response (mmol/mol)",
  y_limits = c(-15, 0),
  y_breaks = seq(-15, 0, by = 3)
)


# 6-month weight response
model_weight_6m <- fit_regression_model(
  cohort6m,
  outcome       = "weight",
  response_time = "6m"
)

pred_weight_6m <- average_predictions_by_year_frailty(
  model   = model_weight_6m,
  data    = cohort6m,
  outcome = "weight"
)

weight_response_plot_6m <- plot_response_outcome(
  preds    = pred_weight_6m,
  y_label  = "Weight change (kg)",
  y_limits = c(-5, 0),
  y_breaks = seq(-5, 0, by = 1)
)


# 6-month treatment discontinuation (with 3m follow-up)
model_discontinuation_6m <- fit_regression_model(
  cohort6m,
  outcome       = "discontinuation",
  response_time = "6m"
)

pred_disc_6m <- average_predictions_by_year_frailty(
  model   = model_discontinuation_6m,
  data    = cohort6m,
  outcome = "discontinuation"
)

discontinuation_plot_6m <- plot_response_outcome(
  preds    = pred_disc_6m,
  y_label  = "Treatment discontinuation (%)",
  y_limits = c(0, 0.5),
  y_breaks = seq(0, 0.5, by = 0.1)
)

# Combine panels
hba1c_response_plot_6m   <- hba1c_response_plot_6m   + theme(legend.position = "none")
discontinuation_plot_6m  <- discontinuation_plot_6m  + theme(legend.position = "none")

supplementary_figure7 <- (
  hba1c_response_plot_6m /
    weight_response_plot_6m /
    discontinuation_plot_6m
) +
  patchwork::plot_annotation(
    tag_levels = "A",
    theme      = theme(plot.tag = element_text(size = 20, face = "bold"))
  )

ggplot2::ggsave(
  filename = "output/figures/supplementary/supplementary_figure7.png",
  plot     = supplementary_figure7,
  width    = 15,
  height   = 19,
  dpi      = 600
)



# ---------------------------------------------------------
# Supplementary Figure 8:
# Response outcomes with frailty split into
# moderate and severe
# ---------------------------------------------------------

# Prepare response data in severe-frailty stratification
cohort_severe <- prepare_response_dataset(cohort_severe)
cohort_severe <- mice_imputation(cohort_severe)

frailty_colors <- c(
  "Age ≤ 70 years"              = "#F8766D",
  "Non-frail > 70 years"        = "#00BA38",
  "Moderate frailty > 70 years" = "#AC85F7",
  "Severe frailty > 70 years"   = "#0152AB"
)

# HbA1c response (12m)
model_hba1c_severe <- fit_regression_model(
  cohort_severe,
  outcome       = "hba1c",
  response_time = "12m"
)

pred_hba1c_severe <- average_predictions_by_year_frailty(
  model   = model_hba1c_severe,
  data    = cohort_severe,
  outcome = "hba1c"
)

hba1c_response_plot_severe <- plot_response_outcome(
  preds        = pred_hba1c_severe,
  y_label      = "HbA1c response (mmol/mol)",
  y_limits     = c(-15, 0),
  y_breaks     = seq(-15, 0, by = 3),
  palette      = frailty_colors,
  legend_title = "Frailty group"
)


# Weight response (12m)
model_weight_severe <- fit_regression_model(
  cohort_severe,
  outcome       = "weight",
  response_time = "12m"
)

pred_weight_severe <- average_predictions_by_year_frailty(
  model   = model_weight_severe,
  data    = cohort_severe,
  outcome = "weight"
)

weight_response_plot_severe <- plot_response_outcome(
  preds        = pred_weight_severe,
  y_label      = "Weight change (kg)",
  y_limits     = c(-5, 0),
  y_breaks     = seq(-5, 0, by = 1),
  palette      = frailty_colors,
  legend_title = "Frailty group"
)


# Treatment discontinuation (12m)
model_discontinuation_severe <- fit_regression_model(
  cohort_severe,
  outcome       = "discontinuation",
  response_time = "12m"
)

pred_disc_severe <- average_predictions_by_year_frailty(
  model   = model_discontinuation_severe,
  data    = cohort_severe,
  outcome = "discontinuation"
)

discontinuation_plot_severe <- plot_response_outcome(
  preds        = pred_disc_severe,
  y_label      = "Treatment discontinuation (%)",
  y_limits     = c(0, 0.5),
  y_breaks     = seq(0, 0.5, by = 0.1),
  palette      = frailty_colors,
  legend_title = "Frailty group"
)

hba1c_response_plot_severe  <- hba1c_response_plot_severe  + theme(legend.position = "none")
discontinuation_plot_severe <- discontinuation_plot_severe + theme(legend.position = "none")

supplementary_figure8 <- (
  hba1c_response_plot_severe /
    weight_response_plot_severe /
    discontinuation_plot_severe
) +
  patchwork::plot_annotation(
    tag_levels = "A",
    theme      = theme(plot.tag = element_text(size = 20, face = "bold"))
  )

ggplot2::ggsave(
  filename = "output/figures/supplementary/supplementary_figure8.png",
  plot     = supplementary_figure8,
  width    = 15,
  height   = 19,
  dpi      = 600
)



# ---------------------------------------------------------
# Supplementary Table 3:
# Crude incidence rates of complications
# ---------------------------------------------------------

# Incidence summary table by year, frailty and outcome
make_incidence_table <- function(
    data,
    outcome_prefixes = c("ukpds", "hf", "renal", "dka"),
    outcome_labels   = c(
      ukpds = "Severe diabetes-related complications",
      hf    = "Heart failure",
      renal = "Kidney failure",
      dka   = "DKA"
    ),
    group_var = "predrug_frail_elderly_cat",
    year_var  = "year",
    min_year  = 2019,
    max_year  = 2022
) {
  group_sym <- rlang::sym(group_var)
  year_sym  <- rlang::sym(year_var)
  
  long_outcomes <- data %>%
    tidyr::pivot_longer(
      cols        = tidyselect::matches("^event_|^person_years_"),
      names_to    = c("measure", "outcome"),
      names_pattern = "(event|person_years)_(.*)",
      values_to   = "value"
    ) %>%
    dplyr::filter(outcome %in% outcome_prefixes) %>%
    tidyr::pivot_wider(
      names_from  = measure,
      values_from = value
    )
  
  inc <- long_outcomes %>%
    dplyr::filter(!is.na(person_years)) %>%
    dplyr::group_by(!!group_sym, !!year_sym, outcome) %>%
    dplyr::summarise(
      n       = dplyr::n(),
      nevents = sum(event, na.rm = TRUE),
      py      = sum(person_years, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      ir1000_py = round(1000 * nevents / py, 1)
    )
  
  if (!is.null(min_year)) {
    inc <- inc %>% dplyr::filter(!!year_sym >= min_year)
  }
  if (!is.null(max_year)) {
    inc <- inc %>% dplyr::filter(!!year_sym <= max_year)
  }
  
  inc <- inc %>%
    dplyr::mutate(
      Outcome = outcome_labels[outcome]
    )
  
  inc %>%
    dplyr::transmute(
      Frailty = !!group_sym,
      Outcome,
      Year    = !!year_sym,
      n,
      `No. of outcome events`              = nevents,
      `Crude incidence rate /1000 person-years` = ir1000_py
    ) %>%
    dplyr::arrange(Frailty, Outcome, Year)
}

# HES-linked cohort and composite outcomes
data_hes <- prepare_complication_data(cohort) %>%
  create_composite_outcomes()

supplementary_table3 <- make_incidence_table(data_hes)

write.csv(
  supplementary_table3,
  file      = "output/tables/supplementary_table3.csv",
  row.names = FALSE
)



# ---------------------------------------------------------
# Supplementary Figure 9:
# HF hospitalisation in people with pre-existing HF or CVD
# ---------------------------------------------------------

data_hes <- data_hes %>%
  dplyr::mutate(
    predrug_hf_all_cause = dplyr::if_else(
      predrug_heartfailure == 1 | predrug_primary_hhf == 1, 1L, 0L
    ),
    predrug_hf_cvd       = dplyr::if_else(
      predrug_hf_all_cause == 1L | predrug_cvd == 1L, 1L, 0L
    )
  )

data_hes_hf <- data_hes %>%
  dplyr::filter(
    predrug_hf_cvd == 1,
    !is.na(event_hf),
    !is.na(person_years_hf),
    person_years_hf > 0
  )

model_hf_with_prev_hf <- fit_poisson_rate_model(
  data        = data_hes_hf,
  outcome_var = "event_hf",
  offset_var  = "person_years_hf"
)

pred_hf_with_prev_hf <- poisson_predictions_by_year_frailty(
  model = model_hf_with_prev_hf,
  data  = data_hes_hf
)

hf_with_prev_hf_contrast_2019_2022 <- year_contrast_by_frailty_poisson(
  model     = model_hf_with_prev_hf,
  data      = data_hes_hf,
  from_year = 2019,
  to_year   = 2022
)

supplementary_figure9 <- plot_complication_outcome(
  preds   = pred_hf_with_prev_hf,
  y_label = "Rate of heart failure hospitalisation\n(per 1,000 person-years)"
)

ggplot2::ggsave(
  filename = "output/figures/supplementary/supplementary_figure9.png",
  plot     = supplementary_figure9,
  width    = 14,
  height   = 8,
  dpi      = 600
)



# ---------------------------------------------------------
# Supplementary Figure 10:
# ITS by frailty subgroup
# ---------------------------------------------------------

study_start       <- as.Date("2019-01-01")
study_end         <- as.Date("2024-03-01")
intervention_date <- as.Date("2022-02-01")

run_its_for_frailty_group <- function(cohort,
                                      frailty_level,
                                      study_start,
                                      study_end,
                                      intervention_date,
                                      y_limits = c(0, 78)) {
  
  cohort_sub <- cohort %>%
    dplyr::filter(predrug_frail_elderly_cat == frailty_level)
  
  its_df <- make_monthly_sglt2_proportions(
    cohort      = cohort_sub,
    study_start = study_start,
    study_end   = study_end
  )
  
  its_df <- add_lagged_its_variables(
    df                = its_df,
    intervention_date = intervention_date,
    lag               = 1L
  )
  
  its_model <- nlme::gls(
    proportion ~ time + intervention_lag1 + post_time_intervention_lag1,
    data        = its_df,
    correlation = nlme::corARMA(p = 1, q = 0, form = ~ time),
    method      = "ML"
  )
  
  pre_intervention_data <- its_df %>%
    dplyr::filter(date < intervention_date)
  
  counterfactual_model <- nlme::gls(
    proportion ~ time,
    data        = pre_intervention_data,
    correlation = nlme::corARMA(p = 1, q = 0, form = ~ time),
    method      = "ML"
  )
  
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
      
      factual_predictions = ifelse(intervention == 0, fit_counterf, fit_its),
      factual_se          = ifelse(intervention == 0, se_counterf, se_its),
      factual_lower       = factual_predictions - 1.96 * factual_se,
      factual_upper       = factual_predictions + 1.96 * factual_se,
      
      counterfactual_predictions = fit_counterf,
      counterfactual_se          = se_counterf,
      counterfactual_lower       = counterfactual_predictions - 1.96 * counterfactual_se,
      counterfactual_upper       = counterfactual_predictions + 1.96 * counterfactual_se,
      
      predrug_frail_elderly_cat  = frailty_level
    )
  
  p <- plot_interrupted_time_series(
    its_df            = its_df,
    intervention_date = intervention_date,
    y_limits          = y_limits
  ) +
    ggplot2::ggtitle(frailty_level)
  
  list(
    its_df               = its_df,
    its_model            = its_model,
    counterfactual_model = counterfactual_model,
    plot                 = p
  )
}

frailty_levels <- cohort %>%
  dplyr::pull(predrug_frail_elderly_cat) %>%
  unique()

its_results_by_frailty <- purrr::map(
  frailty_levels,
  ~ run_its_for_frailty_group(
    cohort           = cohort,
    frailty_level    = .x,
    study_start      = study_start,
    study_end        = study_end,
    intervention_date = intervention_date
  )
)

names(its_results_by_frailty) <- frailty_levels

its_under70_plot        <- its_results_by_frailty[["Age ≤ 70 years"]]$plot
its_non_frail_over70    <- its_results_by_frailty[["Non-frail > 70 years"]]$plot
its_frail_over70        <- its_results_by_frailty[["Frail > 70 years"]]$plot

supplementary_figure10 <- (
  its_under70_plot |
    its_non_frail_over70 |
    its_frail_over70
) +
  patchwork::plot_annotation(tag_levels = "A")

ggplot2::ggsave(
  filename = "output/figures/supplementary/supplementary_figure10.png",
  plot     = supplementary_figure10,
  width    = 22,
  height   = 10,
  dpi      = 600
)
