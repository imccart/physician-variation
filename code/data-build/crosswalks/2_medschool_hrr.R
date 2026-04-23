# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-04-22
## Date Updated:  2026-04-23
## Description:   Build med-school -> HRR crosswalk from the hand-curated
##                med-school-xw.csv (raw PC string -> current LCME program
##                name + defunct / non-MD / foreign flags) joined to the
##                LCME accreditation list (program -> city, state, ZIP),
##                then to the zip -> HRR crosswalk.
##
##                Inputs (med-school-xw.csv and LCME accreditation.xlsx
##                were compiled by Shirley Cai):
##                  data/input/med-school-xw.csv           (raw name + current_name + flags)
##                  data/input/LCME accreditation.xlsx     (LCME program + ZIP)
##                  data/crosswalks/zip-hrr-crosswalk.csv  (zip -> HRR)
##
##                Output:
##                  data/crosswalks/med-school-hrr-crosswalk.csv

source("code/0-setup.R")


# 1. Load inputs ----------------------------------------------------------

xw <- read_csv("data/input/med-school-xw.csv",
               col_types = cols(defunct_year = col_integer(),
                                defunct_zip  = col_character(),
                                Ddefunct     = col_integer(),
                                Dnonmd       = col_integer(),
                                Dforeign     = col_integer(),
                                .default     = col_character()))

cat("med-school-xw rows:", nrow(xw), "\n")
cat("  Dnonmd == 1:  ", sum(xw$Dnonmd == 1, na.rm = TRUE), "\n")
cat("  Dforeign == 1:", sum(xw$Dforeign == 1, na.rm = TRUE), "\n")
cat("  Ddefunct == 1:", sum(xw$Ddefunct == 1, na.rm = TRUE), "\n")
cat("  has current_name:",
    sum(!is.na(xw$current_name) & xw$current_name != ""), "\n")

lcme <- read_excel("data/input/LCME accreditation.xlsx") %>%
  transmute(current_name = Program,
            lcme_city    = City,
            lcme_state   = State,
            zip          = str_pad(as.character(ZIP), width = 5, pad = "0"))

cat("LCME programs:", nrow(lcme), "\n")

zip_hrr <- read_csv("data/crosswalks/zip-hrr-crosswalk.csv",
                    col_types = cols(zip5 = col_character(),
                                     hrrnum = col_integer(),
                                     .default = col_character())) %>%
  select(zip = zip5, hrrnum, hrrcity, hrrstate)


# 2. Resolve ZIP for each xw row -----------------------------------------

# Rule: use LCME-joined ZIP when current_name matches; otherwise fall back
# to defunct_zip if present; else NA (will drop at HRR join).
resolved <- xw %>%
  mutate(is_nonmd   = coalesce(Dnonmd   == 1, FALSE),
         is_foreign = coalesce(Dforeign == 1, FALSE),
         is_defunct = coalesce(Ddefunct == 1, FALSE)) %>%
  left_join(lcme, by = "current_name") %>%
  mutate(zip = coalesce(zip, defunct_zip),
         # drop ZIP for non-MD and foreign so they fail the HRR join
         zip = if_else(is_nonmd | is_foreign, NA_character_, zip))

match_lcme <- sum(!is.na(resolved$zip) & !is.na(resolved$current_name) &
                    resolved$current_name %in% lcme$current_name)
match_defunct <- sum(!is.na(resolved$defunct_zip) & !resolved$is_nonmd &
                       !resolved$is_foreign)

cat("\nZIP resolution:\n")
cat("  via LCME join (current_name):", match_lcme, "\n")
cat("  via defunct_zip fallback:    ", match_defunct, "\n")
cat("  no ZIP assigned:              ",
    sum(is.na(resolved$zip)), "\n")


# 3. Join ZIP -> HRR ------------------------------------------------------

cw <- resolved %>%
  left_join(zip_hrr, by = "zip")

hrr_rate <- cw %>%
  filter(!is.na(zip)) %>%
  summarise(matched = mean(!is.na(hrrnum))) %>%
  pull(matched)
cat(sprintf("\nHRR match rate among ZIP-assigned rows: %.1f%%\n", 100 * hrr_rate))

unmatched_zip <- cw %>% filter(!is.na(zip), is.na(hrrnum))
if (nrow(unmatched_zip) > 0) {
  cat("ZIPs that didn't match a HRR (likely PR or bad ZIPs):\n")
  unmatched_zip %>%
    select(medical_school_name, current_name, zip, lcme_city, lcme_state) %>%
    print(n = Inf)
}


# 4. Attach cardiologist counts for coverage reporting -------------------

counts <- read_csv("data/output/medschool_list.csv", show_col_types = FALSE)

cw <- cw %>%
  rename(med_school = medical_school_name) %>%
  left_join(counts, by = "med_school")

coverage <- cw %>%
  filter(!is.na(n_cardiologists)) %>%
  mutate(hrr_assignable = !is.na(hrrnum)) %>%
  group_by(hrr_assignable) %>%
  summarise(n_schools = n(),
            n_cardiologists = sum(n_cardiologists, na.rm = TRUE)) %>%
  ungroup()

cat("\nCardiologist coverage (among schools that appear in sample):\n")
print(coverage)


# 5. Save -----------------------------------------------------------------

if (!dir.exists("data/crosswalks")) dir.create("data/crosswalks")

final <- cw %>%
  select(med_school, current_name, lcme_city, lcme_state, zip,
         hrrnum, hrrcity, hrrstate,
         Ddefunct, Dnonmd, Dforeign, n_cardiologists)

write_csv(final, "data/crosswalks/med-school-hrr-crosswalk.csv")
cat("\nWrote data/crosswalks/med-school-hrr-crosswalk.csv (",
    nrow(final), "rows)\n")
