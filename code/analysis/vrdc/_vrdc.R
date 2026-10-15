# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-09-29
## Description:   Driver for the patient-level analysis that runs inside the
##                VRDC R container. Loads packages and sources the steps.
##                Inputs are the SAS step 8 and step 9 exports and the uploaded
##                cardiologist exposure file, both in the container's
##                data/input/. Outputs go to results/ in the container for
##                manual export.
##
##                Run from the container project root:
##                  source("code/analysis/vrdc/_vrdc.R")

# pacman is not available in the VRDC container
library(tidyverse)
library(fixest)
library(broom)

source("code/analysis/vrdc/1_threshold.R")
