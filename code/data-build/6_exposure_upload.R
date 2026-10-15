# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-09-29
## Description:   Cardiologist-year exposure file for upload to the VRDC.
##                One row per cardiologist-year in the paper's analytic
##                sample (graduation 1983-2006, non-missing training-period
##                cath lab share and destination LOO measure), carrying the
##                variables the patient-level threshold analysis needs
##                (code/analysis/vrdc/1_threshold.R).
##
##                Input:
##                  data/output/analysis_panel.csv
##
##                Output:
##                  data/output/cardiologist_exposure.csv

# 1. Inputs ----------------------------------------------------------------

analysis <- read_csv("data/output/analysis_panel.csv",
                     col_types = cols(npi = col_character(),
                                      year = col_integer(),
                                      .default = col_guess()),
                     show_col_types = FALSE)


# 2. Analytic sample and output -------------------------------------------

exposure <- analysis %>%
  filter(!is.na(train_cath_lab), !is.na(mean_resid_cath),
         !is.na(intensity_dest_loo), !is.nan(intensity_dest_loo),
         grad_year >= 1983, grad_year <= 2006) %>%
  select(npi, year, hrr_practice, hrr_med_school, grad_year, gender, specialty,
         n_nstemi, mover, train_cath_lab, intensity_dest_loo)

write_csv(exposure, "data/output/cardiologist_exposure.csv")
