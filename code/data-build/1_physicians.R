# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-04-06
## Description:   Build physician panel from MDPPAS + Physician Compare.
##                Outputs physician-level file with practice HRR, med school,
##                graduation year, and mover status.

# 1. MDPPAS: practice location -------------------------------------------

# MDPPAS columns of interest:
#   npi, Year               identifiers
#   spec_prim_1_name        fine-grained specialty string
#   phy_zip_perf1           primary practice ZIP (ranked by allowed $)
#
# Restrict to the analysis sample: cardiologists in CARDIOLOGIST_YEAR_EXPORT
# (NSTEMI volume >= 11). These are the physicians who enter the mover design.
sample_npis <- read_csv("data/output/CARDIOLOGIST_YEAR_EXPORT.csv",
                        col_types = cols(NPI = col_character(),
                                         .default = col_guess())) %>%
  pull(NPI) %>%
  unique()

cardio_specialties <- c("Cardiology", "Interventional Cardiology",
                        "Clinical Cardiac Electrophysiology",
                        "Advanced Heart Failure and Transplant Cardiology")

mdppas_files <- list.files("data/input/mdppas", pattern = "\\.csv$",
                           full.names = TRUE)

read_mdppas <- function(f) {
  read_csv(f,
           col_select = c("npi", "Year", "spec_prim_1_name", "phy_zip_perf1"),
           col_types  = cols(npi = col_character(),
                             Year = col_integer(),
                             spec_prim_1_name = col_character(),
                             phy_zip_perf1 = col_character()),
           show_col_types = FALSE) %>%
    rename(year = Year, zip5 = phy_zip_perf1, specialty = spec_prim_1_name)
}

mdppas <- map_dfr(mdppas_files, read_mdppas) %>%
  filter(npi %in% sample_npis,
         specialty %in% cardio_specialties) %>%
  mutate(zip5 = str_pad(zip5, width = 5, pad = "0")) %>%
  select(npi, year, specialty, zip5)


# 2. Zip to HRR crosswalk ------------------------------------------------

zip_hrr <- read_csv("data/crosswalks/zip-hrr-crosswalk.csv",
                    col_types = cols(zip5 = col_character(),
                                     hrrnum = col_integer(),
                                     .default = col_character())) %>%
  select(zip5, hrr_practice = hrrnum)

mdppas <- mdppas %>%
  left_join(zip_hrr, by = "zip5")

cat("MDPPAS cardiologist-years:", nrow(mdppas), "\n")
cat("Unique NPIs:", n_distinct(mdppas$npi), "\n")
cat("Missing practice HRR:", sum(is.na(mdppas$hrr_practice)), "\n")


# 3. Physician Compare: med school + graduation year + gender ------------

# Built by code/data-build/crosswalks/1_medschool_list.R: one row per
# cardiologist NPI with modal (med_school, grad_year, gender) across all
# PC quarterly files 2013 + 2014 Q3-Q4 + 2015-2018.
phys_compare <- read_csv("data/output/cardiologist_pc.csv",
                         col_types = cols(npi = col_character(),
                                          grad_year = col_integer(),
                                          .default = col_character()))

cat("Physician Compare records:", nrow(phys_compare), "\n")


# 4. Med school to HRR crosswalk -----------------------------------------

med_school_hrr <- read_csv("data/crosswalks/med-school-hrr-crosswalk.csv",
                           show_col_types = FALSE) %>%
  select(med_school, hrr_med_school = hrrnum)

phys_compare <- phys_compare %>%
  left_join(med_school_hrr, by = "med_school")

cat("Med school HRR match rate:",
    mean(!is.na(phys_compare$hrr_med_school)), "\n")


# 5. Merge MDPPAS + Physician Compare ------------------------------------

physician_panel <- mdppas %>%
  left_join(phys_compare, by = "npi")

cat("Physician panel rows:", nrow(physician_panel), "\n")
cat("Missing med school:", mean(is.na(physician_panel$med_school)), "\n")
cat("Missing med school HRR:", mean(is.na(physician_panel$hrr_med_school)), "\n")


# 6. GME-duration filter -------------------------------------------------

# Drop physician-years where the listed graduation year is implausibly
# recent given the GME training required for the observed specialty.
# Cardiology = 3yr internal medicine + 3yr cardiology fellowship = 6yr.
# Subspecialties tack on additional fellowship time.
gme_duration <- c(
  "Cardiology" = 6L,
  "Interventional Cardiology" = 7L,
  "Clinical Cardiac Electrophysiology" = 8L,
  "Advanced Heart Failure and Transplant Cardiology" = 7L
)

physician_panel <- physician_panel %>%
  mutate(gme_yrs            = unname(gme_duration[specialty]),
         min_practice_year  = grad_year + gme_yrs,
         keep_gme           = is.na(grad_year) | year >= min_practice_year)

cat("GME-filter dropped:",
    sum(!physician_panel$keep_gme, na.rm = TRUE),
    "of", nrow(physician_panel), "rows\n")

physician_panel <- physician_panel %>%
  filter(keep_gme) %>%
  select(-gme_yrs, -min_practice_year, -keep_gme)


# 7. Save ----------------------------------------------------------------

write_csv(physician_panel, "data/output/physician_panel.csv")
