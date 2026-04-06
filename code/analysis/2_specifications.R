# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-04-06
## Description:   Mover design regressions. Three specifications:
##                (1) Level of med school HRR intensity
##                (2) Semi-parametric with cubic spline
##                (3) Change in intensity (destination LOO - med school)

source("code/0-setup.R")
library(fixest)
library(splines)
library(modelsummary)


# 1. Load data ------------------------------------------------------------

analysis <- read_csv("data/analysis-panel.csv", show_col_types = FALSE)

# Restrict to movers with non-missing intensity measures
movers <- analysis %>%
  filter(mover == 1,
         !is.na(intensity_med_school),
         !is.na(intensity_dest_loo),
         !is.nan(intensity_dest_loo))

cat("Mover cardiologist-years:", nrow(movers), "\n")
cat("Unique mover cardiologists:", n_distinct(movers$npi), "\n")


# 2. Specification 1: Level of med school HRR intensity -------------------

# Regress cardiologist intensity on med school HRR intensity,
# controlling for destination HRR FE and year FE
m1 <- feols(mean_resid_cath ~ intensity_med_school | hrr + year,
            data = movers, weights = ~n_nstemi,
            cluster = ~hrr_med_school)

summary(m1)


# 3. Specification 2: Semi-parametric (cubic spline) ----------------------

# Replace linear med school intensity with natural cubic spline
# to allow nonlinear relationship
movers <- movers %>%
  mutate(spline_basis = ns(intensity_med_school, df = 4))

m2 <- feols(mean_resid_cath ~ spline_basis | hrr + year,
            data = movers, weights = ~n_nstemi,
            cluster = ~hrr_med_school)

summary(m2)

# Joint F-test on spline terms
wald(m2, "spline")


# 4. Specification 3: Change in intensity ---------------------------------

# Regress cardiologist intensity on the change in environment intensity
# (destination LOO minus med school HRR). Controls for destination HRR FE
# absorb the level of destination intensity.
m3 <- feols(mean_resid_cath ~ intensity_change | hrr + year,
            data = movers, weights = ~n_nstemi,
            cluster = ~hrr_med_school)

summary(m3)


# 5. Robustness: add graduation cohort FE --------------------------------

m4 <- feols(mean_resid_cath ~ intensity_med_school | hrr + year + grad_year,
            data = movers, weights = ~n_nstemi,
            cluster = ~hrr_med_school)

m5 <- feols(mean_resid_cath ~ intensity_change | hrr + year + grad_year,
            data = movers, weights = ~n_nstemi,
            cluster = ~hrr_med_school)


# 6. Full sample: stayers + movers ----------------------------------------

full <- analysis %>%
  filter(!is.na(intensity_med_school),
         !is.na(intensity_dest_loo),
         !is.nan(intensity_dest_loo))

m6 <- feols(mean_resid_cath ~ intensity_med_school | hrr + year,
            data = full, weights = ~n_nstemi,
            cluster = ~hrr_med_school)


# 7. Export results -------------------------------------------------------

# Main table: specifications 1, 3, 4, 5
options(modelsummary_factory_default = "kableExtra")
modelsummary(
  list("Level" = m1, "Change" = m3,
       "Level + Cohort" = m4, "Change + Cohort" = m5),
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared", "FE: hrr", "FE: year", "FE: grad_year"),
  output = "results/tables/main-regressions.tex"
)

# Comparison: movers only vs full sample
modelsummary(
  list("Movers" = m1, "Full Sample" = m6),
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared"),
  output = "results/tables/movers-vs-full.tex"
)
