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
