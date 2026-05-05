# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-04-06
## Description:   Compute HRR-level intensity measures for mover design.
##                Three measures: (1) med school HRR mean intensity,
##                (2) destination HRR leave-one-out mean, (3) change
##                (destination LOO minus med school).

# 1. Load data ------------------------------------------------------------

# Cardiologist-year VRDC export: NPI, Year, N_NSTEMI, Mean_Resid_Cath
# Rename at the boundary to clean snake_case for downstream code.
cardio_year <- read_csv("data/output/CARDIOLOGIST_YEAR_EXPORT.csv",
                        col_types = cols(NPI = col_character(),
                                         Year = col_integer(),
                                         N_NSTEMI = col_integer(),
                                         Mean_Resid_Cath = col_double())) %>%
  rename(npi = NPI, year = Year,
         n_nstemi = N_NSTEMI, mean_resid_cath = Mean_Resid_Cath)

physician_panel <- read_csv("data/output/physician_panel.csv",
                            col_types = cols(npi = col_character(),
                                             year = col_integer(),
                                             .default = col_guess()))

# Merge practice HRR and med school HRR onto cardiologist-year panel
analysis <- cardio_year %>%
  inner_join(
    physician_panel %>%
      select(npi, year, hrr_practice, hrr_med_school, med_school,
             grad_year, gender),
    by = c("npi", "year")
  )

cat("Cardiologist-year rows after merge:", nrow(analysis), "\n")
cat("Lost in merge:", nrow(cardio_year) - nrow(analysis), "\n")


# 2. Med school HRR intensity --------------------------------------------

# Mean residualized cath rate among all cardiologists trained at the same
# medical school HRR (pooled across years)
med_school_intensity <- analysis %>%
  filter(!is.na(hrr_med_school)) %>%
  group_by(hrr_med_school) %>%
  summarize(
    intensity_med_school = weighted.mean(mean_resid_cath, n_nstemi),
    n_cardio_med_school = n_distinct(npi),
    .groups = "drop"
  )

cat("Med school HRRs:", nrow(med_school_intensity), "\n")
cat("Intensity range:",
    round(range(med_school_intensity$intensity_med_school), 4), "\n")


# 3. Destination HRR leave-one-out intensity ------------------------------

# For each cardiologist-year, compute the volume-weighted mean residual
# in their practice HRR excluding themselves
destination_loo <- analysis %>%
  filter(!is.na(hrr_practice)) %>%
  group_by(hrr_practice, year) %>%
  mutate(
    hrr_total_resid = sum(mean_resid_cath * n_nstemi),
    hrr_total_n = sum(n_nstemi),
    intensity_dest_loo = (hrr_total_resid - mean_resid_cath * n_nstemi) /
                         (hrr_total_n - n_nstemi)
  ) %>%
  ungroup() %>%
  select(npi, year, intensity_dest_loo)

cat("LOO computed for", nrow(destination_loo), "cardiologist-years\n")
cat("LOO NaN (solo in HRR-year):", sum(is.nan(destination_loo$intensity_dest_loo)), "\n")


# 4. Combine measures -----------------------------------------------------

analysis <- analysis %>%
  left_join(med_school_intensity, by = "hrr_med_school") %>%
  left_join(destination_loo, by = c("npi", "year"))

# Change in intensity: destination LOO minus med school HRR
analysis <- analysis %>%
  mutate(intensity_change = intensity_dest_loo - intensity_med_school)


# 5. Save ----------------------------------------------------------------

write_csv(analysis, "data/output/analysis_panel.csv")

cat("\nFinal panel:", nrow(analysis), "cardiologist-years\n")
cat("Non-missing med school intensity:", sum(!is.na(analysis$intensity_med_school)), "\n")
cat("Non-missing destination LOO:", sum(!is.na(analysis$intensity_dest_loo)), "\n")
cat("Non-missing change:", sum(!is.na(analysis$intensity_change)), "\n")
