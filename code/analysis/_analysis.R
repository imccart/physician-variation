# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-04-23
## Description:   Driver for the analysis pipeline. Loads packages and runs
##                descriptive, specifications, robustness, and map scripts.
# --------------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, fixest, splines, broom, kableExtra, sf)


# Analysis scripts --------------------------------------------------------

source("code/analysis/1_descriptive.R")
source("code/analysis/2_hrr_map.R")
source("code/analysis/3_selection.R")
source("code/analysis/4_rank.R")
source("code/analysis/5_aha_training.R")
source("code/analysis/6_rank_x_cath.R")
source("code/analysis/7_dynamic.R")
source("code/analysis/8_event_study.R")
source("code/analysis/9_heterogeneity.R")
source("code/analysis/10_mover_balance.R")
source("code/analysis/11_training_pipeline.R")
source("code/analysis/12_recent_grad_residency.R")
source("code/analysis/13_permutation.R")
