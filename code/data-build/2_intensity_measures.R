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
             grad_year, gender, specialty),
    by = c("npi", "year")
  )

cat("Cardiologist-year rows after merge:", nrow(analysis), "\n")
cat("Lost in merge:", nrow(cardio_year) - nrow(analysis), "\n")


# 2. Med school HRR intensity (leave-one-out) ----------------------------

# Mean residualized cath rate among cardiologists trained at the same
# medical school HRR, EXCLUDING the focal cardiologist (all years).
# Built from HRR-level totals minus NPI-level totals so the focal
# cardiologist's own cath rate doesn't enter their training intensity.

hrr_totals <- analysis %>%
  filter(!is.na(hrr_med_school)) %>%
  group_by(hrr_med_school) %>%
  summarize(
    hrr_total_resid = sum(mean_resid_cath * n_nstemi, na.rm = TRUE),
    hrr_total_n     = sum(n_nstemi, na.rm = TRUE),
    hrr_n_cardio    = n_distinct(npi),
    .groups = "drop"
  )

npi_totals <- analysis %>%
  filter(!is.na(hrr_med_school)) %>%
  group_by(npi, hrr_med_school) %>%
  summarize(
    npi_total_resid = sum(mean_resid_cath * n_nstemi, na.rm = TRUE),
    npi_total_n     = sum(n_nstemi, na.rm = TRUE),
    .groups = "drop"
  )

med_school_intensity_loo <- npi_totals %>%
  left_join(hrr_totals, by = "hrr_med_school") %>%
  mutate(
    intensity_med_school = (hrr_total_resid - npi_total_resid) /
                           (hrr_total_n     - npi_total_n),
    n_cardio_med_school  = hrr_n_cardio - 1
  ) %>%
  select(npi, hrr_med_school, intensity_med_school, n_cardio_med_school)

cat("Med school HRRs:", nrow(hrr_totals), "\n")
cat("Per-NPI LOO rows:", nrow(med_school_intensity_loo), "\n")
cat("LOO range:",
    round(range(med_school_intensity_loo$intensity_med_school,
                na.rm = TRUE, finite = TRUE), 4), "\n")
cat("LOO NaN (solo trainee in HRR):",
    sum(is.nan(med_school_intensity_loo$intensity_med_school)), "\n")


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
  left_join(med_school_intensity_loo, by = c("npi", "hrr_med_school")) %>%
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
