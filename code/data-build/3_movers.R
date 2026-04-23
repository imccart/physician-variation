# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-04-06
## Description:   Identify movers (med school HRR != practice HRR) and
##                flag mover status on the analysis panel.

source("code/0-setup.R")


# 1. Load data ------------------------------------------------------------

analysis <- read_csv("data/output/analysis_panel.csv",
                     col_types = cols(npi = col_character(),
                                      year = col_integer(),
                                      .default = col_guess()))


# 2. Flag movers ----------------------------------------------------------

analysis <- analysis %>%
  mutate(mover = as.integer(!is.na(hrr_med_school) & !is.na(hrr_practice) &
                            hrr_med_school != hrr_practice))

cat("Mover status:\n")
analysis %>%
  filter(!is.na(hrr_med_school), !is.na(hrr_practice)) %>%
  count(mover) %>%
  mutate(pct = n / sum(n)) %>%
  print()


# 3. Mover characteristics ------------------------------------------------

cat("\nMover vs stayer summary:\n")
analysis %>%
  filter(!is.na(mover)) %>%
  group_by(mover) %>%
  summarize(
    n_cardio_years = n(),
    n_cardiologists = n_distinct(npi),
    mean_volume = mean(n_nstemi),
    mean_resid = mean(mean_resid_cath),
    mean_grad_year = mean(grad_year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()


# 4. Save ----------------------------------------------------------------

write_csv(analysis, "data/output/analysis_panel.csv")
