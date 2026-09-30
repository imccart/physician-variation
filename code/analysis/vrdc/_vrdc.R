# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-09-29
## Description:   Driver for the patient-level analysis that runs inside the
##                VRDC R container. Loads packages and sources the steps.
##                Inputs are the SAS step 8 export and the uploaded
##                cardiologist exposure file, both in the container's
##                data/input/. Outputs go to results/ in the container for
##                manual export.
##
##                Run from the container project root:
##                  source("code/analysis/vrdc/_vrdc.R")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, fixest, broom)

source("code/analysis/vrdc/1_threshold.R")
