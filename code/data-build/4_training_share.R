# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-10-15
## Description:   HRR-year cardiology infrastructure from the AHA survey and
##                each cardiologist's training-period exposure. An AHA service
##                code of "1" means the hospital provides the service directly;
##                every other code counts as 0, and a share is the mean over
##                the hospitals in the HRR-year. The training period is the
##                year of medical school matriculation (grad_year - 3) clamped
##                to the AHA cath-lab coverage years 1980-2003, matched to the
##                medical-school HRR.
##
##                Inputs:
##                  data/input/aha_hospital.csv     (hospital-year; file symlink,
##                                                   so read_csv rather than fread)
##                  data/output/analysis_panel.csv
##
##                Outputs:
##                  data/output/aha_hrr_year.csv      (hrr, year, shares)
##                  data/output/aha_national_year.csv (year, national shares)
##                  data/output/analysis_panel.csv    (adds med_school_start,
##                                                   aha_match_year, train_*)

# 1. Inputs ----------------------------------------------------------------

# "." is how the 1981 survey codes a missing response.
aha_hosp <- read_csv("data/input/aha_hospital.csv", show_col_types = FALSE,
                     na = c("", "NA", "."),
                     col_types = cols(HRRCODE = col_integer(), year = col_integer(),
                                      CCLABHOS = col_character(),
                                      OHSRGHOS = col_character(),
                                      CICHOS = col_character(),
                                      teach_major = col_double(),
                                      teach_minor = col_double(),
                                      .default = col_guess())) %>%
  mutate(has_cath_lab    = as.integer(CCLABHOS == "1"),
         has_open_heart  = as.integer(OHSRGHOS == "1"),
         has_cardiac_icu = as.integer(CICHOS == "1"),
         # major or minor teaching, the rotation network a student sees
         is_teaching     = as.integer((!is.na(teach_major) & teach_major == 1) |
                                      (!is.na(teach_minor) & teach_minor == 1)))

analysis <- read_csv("data/output/analysis_panel.csv",
                     col_types = cols(npi = col_character(),
                                      year = col_integer(),
                                      .default = col_guess()),
                     show_col_types = FALSE)


# 2. HRR-year and national shares ------------------------------------------

aha_hrr <- aha_hosp %>%
  filter(!is.na(HRRCODE), !is.na(year)) %>%
  group_by(hrr = HRRCODE, year) %>%
  summarize(cath_lab_share       = mean(has_cath_lab, na.rm = TRUE),
            open_heart_share     = mean(has_open_heart, na.rm = TRUE),
            cardiac_icu_share    = mean(has_cardiac_icu, na.rm = TRUE),
            cath_lab_share_teach = mean(has_cath_lab[is_teaching == 1], na.rm = TRUE),
            n_teach              = sum(is_teaching == 1),
            .groups = "drop") %>%
  mutate(across(c(ends_with("_share"), cath_lab_share_teach),
                ~ if_else(is.nan(.x), NA_real_, .x)))

# national_cath_share averages over hospitals, including those with no HRR
# code; hrr_mean_cath_share averages the HRR-level shares.
aha_national <- aha_hosp %>%
  filter(!is.na(year)) %>%
  group_by(year) %>%
  summarize(national_cath_share = mean(has_cath_lab, na.rm = TRUE), .groups = "drop") %>%
  left_join(aha_hrr %>%
              group_by(year) %>%
              summarize(hrr_mean_cath_share = mean(cath_lab_share, na.rm = TRUE),
                        .groups = "drop"),
            by = "year", relationship = "one-to-one")

write_csv(aha_hrr, "data/output/aha_hrr_year.csv")
write_csv(aha_national, "data/output/aha_national_year.csv")


# 3. Training-period exposure onto the panel --------------------------------

# The step rewrites its own input, so drop the columns it adds before adding them.
analysis <- analysis %>%
  select(-any_of(c("med_school_start", "aha_match_year", "train_cath_lab",
                   "train_open_heart", "train_cardiac_icu", "train_cath_lab_teach"))) %>%
  mutate(med_school_start = grad_year - 3L,
         aha_match_year   = pmin(pmax(med_school_start, 1980L), 2003L)) %>%
  left_join(aha_hrr %>%
              select(hrr, year,
                     train_cath_lab       = cath_lab_share,
                     train_open_heart     = open_heart_share,
                     train_cardiac_icu    = cardiac_icu_share,
                     train_cath_lab_teach = cath_lab_share_teach),
            by = c("hrr_med_school" = "hrr", "aha_match_year" = "year"),
            relationship = "many-to-one")

write_csv(analysis, "data/output/analysis_panel.csv")
