# ---------------------------------------------------------
# 00_setup.R
# Load libraries and source project functions
# ---------------------------------------------------------

# Load required libraries
library(aurum)
library(tidyverse)
library(lubridate)
library(tableone)
library(patchwork)
library(rlang)
library(mice)
library(marginaleffects)
library(nlme)
library(AICcmodavg)


# Source project functions
source("R/00_utils.R")
source("R/01_cohort_definition.R")
source("R/02_descriptive_trends.R")
source("R/03_response_outcomes.R")
source("R/04_complication_outcomes.R")
source("R/05_interrupted_time_series.R")
source("R/06_plotting.R")
