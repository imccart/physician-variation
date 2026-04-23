# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-04-23
## Description:   Driver for the analysis pipeline. Sources setup + analysis
##                packages, then runs descriptive and specification scripts
##                in order.
##
##                Assumes data-build has been run (analysis_panel.csv exists)
##                and renv has fixest / kableExtra / broom installed.
##
##                Run from project root:
##                  Rscript code/analysis/_analysis.R

source("code/0-setup.R")
library(fixest)
library(splines)
library(broom)
library(kableExtra)


# Analysis scripts --------------------------------------------------------

source("code/analysis/1_descriptive.R")
source("code/analysis/2_specifications.R")
