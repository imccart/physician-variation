# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-04-22
## Description:   Pull med school, graduation year, and gender from Physician
##                Compare for our cardiologist sample. Walks PC 2013,
##                2014 Q3-Q4, 2015-2018 (skips 2014 Q1-Q2, which ship without
##                headers). Handles both PC schema variants (long names
##                pre-2017 Q4, short names after) and collapses across
##                quarterly files via modal tuple per NPI.
##
##                Outputs:
##                  data/output/cardiologist_pc.csv     (NPI -> med_school, grad_year, gender)
##                  data/output/medschool_list.csv      (med_school -> count)
##                  data/output/physician_pc.csv        (every clinician with a
##                                                       medical school: med_school,
##                                                       grad_year, gender, and the
##                                                       primary specialty in the
##                                                       latest file listing the NPI)

# 1. Load cardiologist NPI sample ----------------------------------------

cardio_npis <- read_csv("data/output/CARDIOLOGIST_YEAR_EXPORT.csv",
                        col_types = cols(NPI = col_character(),
                                         .default = col_guess())) %>%
  pull(NPI) %>%
  unique()

cat("Cardiologist NPIs in export:", length(cardio_npis), "\n")


# 2. Read Physician Compare (schema detected per file) --------------------

# Two schema variants across 2013-2018 (transition happens in 2017 Q4):
#   Old: "NPI", "Medical school name", "Graduation year", "Gender", "Primary specialty"
#   New: "NPI", "Med_sch",             "Grd_yr",          "gndr",   "pri_spec"
# 2014 files ship without a header row and are skipped here.

old_names <- c(med_school = "Medical school name",
               grad_year  = "Graduation year",
               gender     = "Gender",
               specialty  = "Primary specialty")
new_names <- c(med_school = "Med_sch",
               grad_year  = "Grd_yr",
               gender     = "gndr",
               specialty  = "pri_spec")

# One row per distinct (NPI, school, year, gender, specialty) within a file;
# a clinician's several practice locations collapse here rather than being
# carried through the stack of files.
read_pc <- function(f) {
  header <- str_trim(str_split(readLines(f, n = 1), ",")[[1]])

  cols_map <- if (all(old_names %in% header)) old_names
              else if (all(new_names %in% header)) new_names
              else stop("Unrecognized PC schema in ", f)

  raw <- read_csv(f,
                  col_types = cols(.default = col_character()),
                  name_repair = ~ str_trim(.x),
                  show_col_types = FALSE)

  tibble(npi        = raw[["NPI"]],
         med_school = raw[[cols_map[["med_school"]]]],
         grad_year  = raw[[cols_map[["grad_year"]]]],
         gender     = raw[[cols_map[["gender"]]]],
         specialty  = raw[[cols_map[["specialty"]]]],
         pc_file    = basename(f)) %>%
    distinct()
}

files <- list.files("data/input/physician-compare",
                    pattern = "^(2013|2015|2016|2017|2018)_Q[1-4]\\.csv$|^2014_Q[34]\\.csv$",
                    recursive = TRUE, full.names = TRUE)
# Note: 2014_Q1.csv and 2014_Q2.csv ship without a header row and with a
# non-standard column count; they are excluded here. Q3/Q4 are fine.

cat("Physician Compare files:", length(files), "\n")

pc_raw <- map_dfr(files, read_pc) %>%
  mutate(across(c(med_school, grad_year, gender, specialty), str_trim),
         across(c(med_school, grad_year, gender, specialty),
                ~ if_else(.x == "", NA_character_, .x))) %>%
  filter(!is.na(med_school))

pc_all <- pc_raw %>%
  select(npi, med_school, grad_year, gender) %>%
  distinct()

cat("Physician-row count (all physicians, non-missing med_school):",
    nrow(pc_all), "\n")


# 3. Filter to cardiologists ---------------------------------------------

pc_cardio <- pc_all %>%
  filter(npi %in% cardio_npis)

cat("Rows after cardiologist filter:", nrow(pc_cardio), "\n")
cat("Unique cardiologist NPIs matched:", n_distinct(pc_cardio$npi), "\n")


# 4. Collapse to one row per NPI (modal tuple) ---------------------------

# Fields are time-invariant, but quarterly files occasionally disagree
# (data entry updates). Take the modal (med_school, grad_year, gender)
# tuple per NPI.
pc_cardio_modal <- pc_cardio %>%
  count(npi, med_school, grad_year, gender) %>%
  group_by(npi) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(npi, med_school, grad_year, gender)

coverage <- nrow(pc_cardio_modal) / length(cardio_npis)
cat("Cardiologists with PC data:", nrow(pc_cardio_modal),
    sprintf("(%.1f%%)", 100 * coverage), "\n")
cat("  non-NA grad_year:", sum(!is.na(pc_cardio_modal$grad_year)), "\n")
cat("  non-NA gender:   ", sum(!is.na(pc_cardio_modal$gender)), "\n")

write_csv(pc_cardio_modal, "data/output/cardiologist_pc.csv")


# 5. Unique school list with counts --------------------------------------

medschool_list <- pc_cardio_modal %>%
  count(med_school, sort = TRUE, name = "n_cardiologists")

cat("Unique med schools:", nrow(medschool_list), "\n")
cat("Top 10:\n")
print(head(medschool_list, 10))

write_csv(medschool_list, "data/output/medschool_list.csv")


# 6. Full clinician roster for the specialty-composition test -------------

# One row per NPI across every clinician with a medical school, not only the
# cardiologist sample. School, graduation year and gender are the first
# listed tuple, as for the cardiologists above. Specialty is taken from the
# latest file that lists the NPI, because "Interventional Cardiology" became
# a Medicare specialty only in 2015 and earlier files list those
# cardiologists under cardiovascular disease.
pc_roster <- pc_raw %>%
  arrange(npi, pc_file) %>%
  group_by(npi) %>%
  summarize(med_school = first(med_school),
            grad_year  = first(grad_year),
            gender     = first(gender),
            specialty  = if (all(is.na(specialty))) NA_character_
                         else last(specialty[!is.na(specialty)]),
            .groups = "drop")

write_csv(pc_roster, "data/output/physician_pc.csv")
