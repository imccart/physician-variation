# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-04-06
## Description:   Build physician panel from MDPPAS + Physician Compare.
##                Outputs physician-level file with practice HRR, med school,
##                graduation year, and mover status.

source("code/0-setup.R")


# 1. MDPPAS: practice location -------------------------------------------

# Read MDPPAS for 2009-2017 (covers 2008-2018 with fallbacks)
mdppas_files <- list.files("data/mdppas", pattern = "\\.csv$", full.names = TRUE)
mdppas <- map_dfr(mdppas_files, read_csv, show_col_types = FALSE)

# Keep cardiologists, select relevant columns
cardio_specialties <- c("Cardiology", "Interventional Cardiology",
                        "Clinical Cardiac Electrophysiology",
                        "Advanced Heart Failure and Transplant Cardiology")

mdppas <- mdppas %>%
  filter(spec_broad %in% cardio_specialties | spec_prim_1_name %in% cardio_specialties) %>%
  select(npi, year, zip5 = phy_zip5, state = phy_st)


# 2. Zip to HRR crosswalk ------------------------------------------------

zip_hrr <- read_csv("data/crosswalks/zip-hrr-crosswalk.csv", show_col_types = FALSE)

mdppas <- mdppas %>%
  left_join(zip_hrr, by = "zip5") %>%
  select(npi, year, zip5, state, hrr = hrrnum)

cat("MDPPAS cardiologist-years:", nrow(mdppas), "\n")
cat("Unique NPIs:", n_distinct(mdppas$npi), "\n")
cat("Missing HRR:", sum(is.na(mdppas$hrr)), "\n")


# 3. Physician Compare: med school + graduation year ----------------------

phys_compare <- read_csv("data/physician-compare/physician-compare.csv",
                         show_col_types = FALSE)

phys_compare <- phys_compare %>%
  select(npi, med_school = med_school_name, grad_year = graduation_year,
         gender) %>%
  distinct(npi, .keep_all = TRUE)

cat("Physician Compare records:", nrow(phys_compare), "\n")


# 4. Med school to HRR crosswalk -----------------------------------------

# Map medical school names to HRR via school location
med_school_hrr <- read_csv("data/crosswalks/med-school-hrr-crosswalk.csv",
                           show_col_types = FALSE)

phys_compare <- phys_compare %>%
  left_join(med_school_hrr, by = "med_school") %>%
  rename(hrr_med_school = hrrnum)

cat("Med school HRR match rate:",
    mean(!is.na(phys_compare$hrr_med_school)), "\n")


# 5. Merge MDPPAS + Physician Compare ------------------------------------

physician_panel <- mdppas %>%
  left_join(phys_compare, by = "npi")

cat("Physician panel rows:", nrow(physician_panel), "\n")
cat("Missing med school:", mean(is.na(physician_panel$med_school)), "\n")
cat("Missing med school HRR:", mean(is.na(physician_panel$hrr_med_school)), "\n")


# 6. Save ----------------------------------------------------------------

write_csv(physician_panel, "data/physician-panel.csv")
