# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-04-23
## Description:   Driver for the full data-build pipeline. Sources setup and
##                runs crosswalks + main scripts in dependency order.
##
##                Run from project root:
##                  Rscript code/data-build/_data-build.R

source("code/0-setup.R")


# Crosswalks --------------------------------------------------------------

source("code/data-build/crosswalks/0_zip_hrr.R")
source("code/data-build/crosswalks/1_medschool_list.R")
source("code/data-build/crosswalks/2_medschool_hrr.R")


# Main pipeline -----------------------------------------------------------

source("code/data-build/1_physicians.R")
source("code/data-build/2_intensity_measures.R")
source("code/data-build/3_movers.R")
