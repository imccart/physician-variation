# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-03-30
## Description:   Activates renv and loads core packages used by every
##                script (data-build and analysis). Analysis scripts load
##                their own additional packages (fixest, modelsummary,
##                kableExtra, splines) directly.

# renv activation ---------------------------------------------------------
source("renv/activate.R")

# Packages ----------------------------------------------------------------
library(tidyverse)
library(readxl)


# modelsummary defaults ---------------------------------------------------
# (We don't use modelsummary() directly -- all LaTeX tables are built by
# hand via kable(format = "latex") + save_kable() -- but set the default
# backend just in case it gets used interactively.)
options(modelsummary_factory_default = "kableExtra")
