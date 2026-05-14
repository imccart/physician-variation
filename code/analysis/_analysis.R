# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-04-23
## Description:   Driver for the analysis pipeline. Loads packages and runs
##                descriptive, specifications, robustness, and map scripts.
##                Sub-scripts inherit the prepared session.
##
##                Assumes data-build has been run (analysis_panel.csv exists).
##
##                Run from project root:
##                  Rscript code/analysis/_analysis.R

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, fixest, splines, broom, kableExtra, sf)


# Analysis scripts --------------------------------------------------------

source("code/analysis/1_descriptive.R")
source("code/analysis/2_specifications.R")
source("code/analysis/3_robustness.R")
source("code/analysis/4_hrr_map.R")
source("code/analysis/5_selection.R")
source("code/analysis/7_rank.R")
source("code/analysis/8_aha_training.R")
source("code/analysis/9_rank_x_cath.R")
source("code/analysis/11_dynamic.R")
source("code/analysis/12_event_study.R")
source("code/analysis/13_heterogeneity.R")
source("code/analysis/14_mover_balance.R")
source("code/analysis/15_training_pipeline.R")
source("code/analysis/16_recent_grad_residency.R")
