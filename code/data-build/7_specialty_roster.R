# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-11-09
## Description:   Clinician roster for the specialty-composition test: every
##                Physician Compare clinician whose medical school maps to an
##                HRR, with the training-period cath-lab share matched as for
##                the cardiologist panel (the first full year of attendance,
##                grad_year - 3, clamped to the AHA coverage years 1980-2003,
##                in the medical-school HRR) and an indicator for a cardiology
##                primary specialty.
##
##                Inputs:
##                  data/output/physician_pc.csv
##                  data/crosswalks/med-school-hrr-crosswalk.csv
##                  data/output/aha_hrr_year.csv
##
##                Outputs:
##                  data/output/physician_roster.csv

# 1. Inputs ----------------------------------------------------------------

pc_roster <- read_csv("data/output/physician_pc.csv",
                      col_types = cols(npi = col_character(),
                                       grad_year = col_integer(),
                                       .default = col_character()),
                      show_col_types = FALSE)

med_school_hrr <- read_csv("data/crosswalks/med-school-hrr-crosswalk.csv",
                           show_col_types = FALSE) %>%
  select(med_school, hrr_med_school = hrrnum)

aha_hrr <- read_csv("data/output/aha_hrr_year.csv",
                    col_types = cols(hrr = col_integer(), year = col_integer(),
                                     .default = col_double()),
                    show_col_types = FALSE)

# Physician Compare primary-specialty labels for the four cardiology
# specialties that define the cardiologist sample in 1_physicians.R.
cardio_specialties <- c("CARDIOVASCULAR DISEASE (CARDIOLOGY)",
                        "INTERVENTIONAL CARDIOLOGY",
                        "CARDIAC ELECTROPHYSIOLOGY",
                        "ADVANCED HEART FAILURE AND TRANSPLANT CARDIOLOGY")


# 2. Roster ----------------------------------------------------------------

roster <- pc_roster %>%
  filter(!is.na(grad_year), !is.na(specialty)) %>%
  inner_join(med_school_hrr, by = "med_school", relationship = "many-to-one") %>%
  filter(!is.na(hrr_med_school)) %>%
  mutate(med_school_start  = grad_year - 3L,
         aha_match_year    = pmin(pmax(med_school_start, 1980L), 2003L),
         is_cardiologist   = as.integer(specialty %in% cardio_specialties)) %>%
  left_join(aha_hrr %>% select(hrr, year, train_cath_lab = cath_lab_share),
            by = c("hrr_med_school" = "hrr", "aha_match_year" = "year"),
            relationship = "many-to-one") %>%
  select(npi, med_school, hrr_med_school, grad_year, gender, specialty,
         is_cardiologist, med_school_start, aha_match_year, train_cath_lab)

write_csv(roster, "data/output/physician_roster.csv")
