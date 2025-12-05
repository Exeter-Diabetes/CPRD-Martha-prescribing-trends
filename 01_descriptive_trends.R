# ---------------------------------------------------------
# 01_descriptive_trends.R
#
# This script:
#   1. Sets up the CPRD environment and defines the study cohort
#   2. Produces Table 1 (baseline characteristics)
#   3. Computes and plots prescribing trends by frailty group
# ---------------------------------------------------------


# Load shared setup
source("00_setup.R")


# ---------------------------------------------------------
# 1. Set up CPRD environment and build second-line cohort
# ---------------------------------------------------------

cprd     <- CPRDData$new(
  cprdEnv  = "diabetes-2024",
  cprdConf = "~/.aurum.yaml"
)

analysis <- cprd$analysis("mm")

# Load drug initiation data
drug_classes <- analysis$cached(name = "martha_20250327_t2d_1stinstance") %>% collect()

# Define second-line therapy cohort
cohort <- define_cohort(
  data           = drug_classes,
  min_start_date = "2019-01-01"
)

# Create output directories
dir.create("output/tables",  recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)



# ---------------------------------------------------------
# 2. Table 1: Baseline characteristics
# ---------------------------------------------------------

# Build Table 1 stratified by frailty-age group
table1 <- make_table(cohort)

print(
  table1,
  showAllLevels = TRUE,
  quote         = FALSE,
  noSpaces      = TRUE
)

# Export to CSV
table1_df <- print(
  table1,
  showAllLevels = TRUE,
  quote         = FALSE,
  noSpaces      = TRUE,
  printToggle   = FALSE
)

write.csv(
  table1_df,
  file      = "output/tables/main_table1.csv",
  row.names = FALSE
)



# ---------------------------------------------------------
# 3. Prescribing trends by frailty group
# ---------------------------------------------------------

# Compute yearly proportions of drug-class initiations by frailty category
trends_frailty <- drug_proportions(
  cohort,
  group_var = "predrug_frail_elderly_cat"
)


# Plot prescribing trends by frailty group
prescribing_trends_frailty_plot <- plot_drug_trends_by_group(
  trends    = trends_frailty,
  cohort    = cohort,
  group_var = "predrug_frail_elderly_cat",
  y_lab     = "Second-line initiations (%)"
)



