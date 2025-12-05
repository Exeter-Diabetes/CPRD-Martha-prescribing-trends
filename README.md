# **Trends in second-line glucose-lowering therapy initiations and the impact on treatment outcomes across age and frailty groups in people with type 2 diabetes, 2019-2024: UK population-based study**

#### Analysis code repository

## Introduction

This repository contains the R code for "Trends in second-line glucose-lowering therapy initiations and the impact on treatment outcomes across age and frailty groups in people with type 2 diabetes, 2019-2024: UK population-based study" (link to be added).

The study uses data from the UK Clinical Practice Research Datalink (CPRD) to:

-   Describe contemporary population-level second-line prescribing trends after metformin.
-   Evaluate trends in short-term clinical outcomes following second-line treatment initiation including:
    -   12-month response outcomes: HbA1c, weight, treatment discontinuation
    -   Complicaiton outcomes with 12-months: severe diabetes-related complication, heart failure, kidney failure
-   Evaluate the impact of the 2022 NICE guideline update recommending broader use of SGLT2 inhibitors.
-   Assess whether trends differ in older adults with frailty, a high-risk population historically under-represented in clinical trials.

The analysis includes 117,064 adults initiating second-line therapy after metformin between 2019–2024.

## Key outcome definitions

| Outcome                                                     | Definition                                                                                                                                      |
|-----------------------------|-------------------------------------------|
| **HbA1c response**                                          | Change in HbA1c from baseline to 12 months (±3 months).                                                                                         |
| **Weight change**                                           | Weight change from baseline to 12 months (±3 months).                                                                                           |
| **Treatment discontinuation**                               | Stopping the initiated drug within 12 months, confirmed with 3-month follow-up.                                                                 |
| **Severe diabetes-related complications (UKPDS composite)** | First event among: MI, stroke, heart failure, amputation, kidney failure, severe retinopathy, sudden death, hyperglycaemia/hypoglycaemia death. |
| **Heart failure**                                           | Hospital admission for heart failure or primary-cause HF death.                                                                                 |
| **Kidney failure**                                          | CKD stage 5 diagnosis or primary-cause kidney failure death.                                                                                    |
| **DKA**                                                     | Hospital admission for diabetic ketoacidosis.                                                                                                   |

## Age and frailty groups

Frailty is based on the electronic frailty index (eFI). The three main analysis groups are:

-   **Age ≤ 70 years**
-   **Non-frail \> 70 years** (eFI: fit or mild)
-   **Frail \> 70 years** (eFI: moderate or severe)

## Repository Structure

This project is organised into:

-   **Analysis scripts** (in the main project folder): these use the functions to run the full analysis and produce all tables and figures for the paper.
-   **Function scripts** (in the functions/ folder): these contain all the functions for building the cohort, creating outcomes, running models and making plots. For every analysis script, there is a corresponding function script that contains the functions needed to run that part of the analysis.

``` text
 ├── 00_setup.R
 ├── 01_cohort_definition.R
 ├── 02_descriptive_trends.R
 ├── 03_response_outcomes.R
 ├── 04_complication_outcomes.R
 ├── 05_interrupted_time_series.R
 ├── 05_main_figures.R
 ├── 06_supplementary.R
 │
 ├── functions/
 │   ├── 00_utils.R
 │   ├── 01_cohort_definition.R
 │   ├── 02_descriptive_trends.R
 │   ├── 03_response_outcomes.R
 │   ├── 04_complication_outcomes.R
 │   ├── 05_interrupted_time_series_functions.R
 │   └── 06_plotting.R
 │
 └── output/
     ├── tables/
     └── figures/
         ├── main/
         └── supplementary/
```

## What each analysis script does

| Script                           | Description                                                                                                                                 |
|-------------------------|-----------------------------------------------|
| **00_setup.R**                   | Loads required packages and sources all function files.                                                                                     |
| **01_cohort_definition.R**       | Builds the second-line therapy cohort.                                                                                                      |
| **02_descriptive_trends.R**      | Creates baseline descriptive tables and prescribing trends plot by year and frailty group.                                                  |
| **03_response_outcomes.R**       | Uses regression models to predict average 12-month HbA1c response, weight change and treatment discontinuation and plots these trends.      |
| **04_complication_outcomes.R**   | Creates composite complication outcomes. Uses poisson models to predict complication rates (per 1,000 person years) and plots these trends. |
| **05_interrupted_time_series.R** | Performs the interrupted time-series analysis of monthly SGLT2i use.                                                                        |
| **05_main_figures.R**            | Combines individual plots into the final figures used in the manuscript.                                                                    |
| **06_supplementary.R**           | Produces all supplementary tables and figures in the paper.                                                                                 |

If you want to reproduce the analysis, run each of the following analysis scripts in order. All outputs are saved to the output/ folder.

## Software Requirements

All analyses were conducted using R (version 4.4.0), with key packages including mice (version 3.18.0) and tidyverse (version 2.0.0).
