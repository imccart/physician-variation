# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-09-29
## Description:   Cardiologist-year exposure file for upload to the VRDC.
##                One row per cardiologist-year in the paper's analytic
##                sample (graduation 1983-2006, non-missing training-period
##                cath lab share and destination LOO measure), carrying the
##                variables the patient-level threshold analysis needs
##                (code/analysis/vrdc/1_threshold.R). The training-period
##                cath lab share is built exactly as in the analysis scripts.
##
##                Inputs:
##                  data/output/analysis_panel.csv
##                  data/input/aha_hospital.csv
##
##                Output:
##                  data/output/cardiologist_exposure.csv

# 1. Inputs ----------------------------------------------------------------

analysis <- read_csv("data/output/analysis_panel.csv",
                     col_types = cols(npi = col_character(),
                                      year = col_integer(),
                                      .default = col_guess()),
                     show_col_types = FALSE)

aha_hosp <- read_csv("data/input/aha_hospital.csv", show_col_types = FALSE,
                     na = c("", "NA", "."),
                     col_types = cols(HRRCODE = col_integer(), year = col_integer(),
                                      CCLABHOS = col_character(),
                                      .default = col_guess()))


# 2. Training-period cath lab share ----------------------------------------

aha_hrr <- aha_hosp %>%
  filter(!is.na(HRRCODE), !is.na(year)) %>%
  mutate(has_cath_lab = as.integer(CCLABHOS == "1")) %>%
  group_by(HRRCODE, year) %>%
  summarize(cath_lab_share = mean(has_cath_lab, na.rm = TRUE), .groups = "drop") %>%
  rename(hrr = HRRCODE) %>%
  mutate(cath_lab_share = if_else(is.nan(cath_lab_share), NA_real_, cath_lab_share))

phys_train <- analysis %>%
  filter(!is.na(grad_year), !is.na(hrr_med_school)) %>%
  distinct(npi, hrr_med_school, grad_year) %>%
  mutate(med_school_start = grad_year - 3,
         aha_match_year   = pmin(pmax(med_school_start, 1980L), 2003L)) %>%
  left_join(aha_hrr %>% select(hrr, year, train_cath_lab = cath_lab_share),
            by = c("hrr_med_school" = "hrr", "aha_match_year" = "year"))


# 3. Analytic sample and output -------------------------------------------

exposure <- analysis %>%
  left_join(phys_train %>% select(npi, hrr_med_school, train_cath_lab),
            by = c("npi", "hrr_med_school")) %>%
  filter(!is.na(train_cath_lab), !is.na(mean_resid_cath),
         !is.na(intensity_dest_loo), !is.nan(intensity_dest_loo),
         grad_year >= 1983, grad_year <= 2006) %>%
  select(npi, year, hrr_practice, hrr_med_school, grad_year, gender, specialty,
         n_nstemi, mover, train_cath_lab, intensity_dest_loo)

write_csv(exposure, "data/output/cardiologist_exposure.csv")
