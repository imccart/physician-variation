# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-04-23
## Description:   Driver for the full data-build pipeline. Loads packages
##                and runs crosswalks + main scripts in dependency order.
##                Sub-scripts inherit the prepared session.
##
##                Run from project root:
##                  Rscript code/data-build/_data-build.R

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, data.table, stringi)


# Crosswalks --------------------------------------------------------------

source("code/data-build/crosswalks/0_zip_hrr.R")
source("code/data-build/crosswalks/1_medschool_list.R")
source("code/data-build/crosswalks/2_medschool_hrr.R")
source("code/data-build/crosswalks/3_doximity_residency.R")
source("code/data-build/crosswalks/4_doximity_aha.R")


# Main pipeline -----------------------------------------------------------

source("code/data-build/1_physicians.R")
source("code/data-build/2_intensity_measures.R")
source("code/data-build/3_movers.R")
source("code/data-build/4_training_exposure.R")
