# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-05-12
## Description:   Build per-cardiologist training-stage exposure measures
##                from the Doximity crosswalk + the training-program <-> AHA
##                hospital map. Outputs one row per matched cardiologist
##                with residency and fellowship cath-lab availability at
##                the program (own hospital) and at the same-HRR system.
##
##                Measures (per stage):
##                  *_own_cath          mean of CCLABHOS at trainee's program
##                                      over PGY1-PGY3 (residency: grad_year
##                                      to grad_year+2; fellowship:
##                                      grad_year+3 to grad_year+5)
##                  *_sys_size          mean # hospitals in same-HRR system
##                  *_sys_any_cath      max indicator: any cath in same-HRR-
##                                      system (always >= own_cath)
##                  *_sys_n_cath        mean # cath-equipped hospitals in
##                                      same-HRR-system
##                  *_sys_share_cath    *_sys_n_cath / *_sys_size
##
##                Outputs:
##                  data/output/cardiologist_training_exposure.csv

# 1. Inputs ----------------------------------------------------------------

cw <- read_csv("data/output/cardiologist_doximity.csv",
               col_types = cols(npi = col_character(),
                                .default = col_guess()),
               show_col_types = FALSE)
aha <- fread("data/input/aha_hospital.csv",
             select = c("ID", "SYSID", "MNAME", "MSTATE", "HRRCODE", "year",
                        "CCLABHOS"),
             na.strings = c("", "NA"), showProgress = FALSE)
setDF(aha)
aha <- aha %>%
  mutate(year = as.integer(year),
         ID = as.character(ID),
         SYSID = as.character(SYSID),
         HRRCODE = suppressWarnings(as.integer(HRRCODE)),
         hosp_cath = as.integer(CCLABHOS == "1"))

cw_aha <- read_csv("data/output/training_aha_crosswalk.csv",
                   col_types = cols(ahaid = col_character(),
                                    .default = col_guess()),
                   show_col_types = FALSE)
pc <- read_csv("data/output/cardiologist_pc.csv",
               col_types = cols(npi = col_character(),
                                grad_year = col_integer(),
                                .default = col_guess()),
               show_col_types = FALSE) %>%
  select(npi, grad_year_pc = grad_year)


# 2. Hospital-year + same-HRR-system rollup -------------------------------

hosp_year <- aha %>%
  filter(!is.na(ID), !is.na(year), year >= 1980, year <= 2010) %>%
  select(ID, year, SYSID, HRRCODE, hosp_cath) %>%
  distinct(ID, year, .keep_all = TRUE)

# System rollup keyed on (SYSID, HRR, year). Restricting to same-HRR is the
# substantively right scope: a multi-state system is not all available to
# any one resident.
sys_hrr <- aha %>%
  filter(!is.na(SYSID), SYSID != "0", !is.na(year), !is.na(HRRCODE),
         year >= 1980, year <= 2010) %>%
  group_by(SYSID, HRRCODE, year) %>%
  summarize(sys_size      = n_distinct(ID),
            sys_n_cath    = sum(hosp_cath, na.rm = TRUE),
            sys_any_cath  = as.integer(any(hosp_cath == 1, na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(sys_share_cath = sys_n_cath / sys_size)


# 3. Attach AHA IDs back to cardiologists ---------------------------------

res_aha <- cw_aha %>% filter(stage == "residency") %>%
  select(residency_institution = dox_string,
         ahaid_residency = ahaid, res_match_kind = match_kind)
fel_aha <- cw_aha %>% filter(stage == "fellowship") %>%
  select(fellowship_institution = dox_string,
         ahaid_fellowship = ahaid, fel_match_kind = match_kind)

cw_join <- cw %>%
  left_join(res_aha, by = "residency_institution") %>%
  left_join(fel_aha, by = "fellowship_institution") %>%
  left_join(pc, by = "npi") %>%
  # Residency starts at grad_year; fellowship at grad_year + 3 (after 3-yr
  # IM residency). Clamp to 1980-2001 so PGY+2 stays in 1980-2003.
  mutate(start_yr_res = pmin(pmax(grad_year_pc,      1980L), 2001L),
         start_yr_fel = pmin(pmax(grad_year_pc + 3L, 1980L), 2001L))


# 4. Stage-level measure builder -----------------------------------------

attach_measures <- function(df, ahaid_col, start_col, prefix) {
  long <- df %>%
    filter(!is.na(.data[[ahaid_col]]), !is.na(.data[[start_col]])) %>%
    select(npi, ahaid = all_of(ahaid_col), start = all_of(start_col)) %>%
    uncount(3, .id = "k") %>%
    mutate(year = start + k - 1L) %>%
    left_join(hosp_year, by = c("ahaid" = "ID", "year" = "year")) %>%
    left_join(sys_hrr, by = c("SYSID", "HRRCODE", "year"))

  long %>%
    group_by(npi) %>%
    summarize(!!paste0(prefix, "_own_cath")       := mean(hosp_cath,       na.rm = TRUE),
              !!paste0(prefix, "_sys_size")       := mean(sys_size,        na.rm = TRUE),
              !!paste0(prefix, "_sys_any_cath")   := suppressWarnings(max(sys_any_cath, na.rm = TRUE)),
              !!paste0(prefix, "_sys_n_cath")     := mean(sys_n_cath,      na.rm = TRUE),
              !!paste0(prefix, "_sys_share_cath") := mean(sys_share_cath,  na.rm = TRUE),
              !!paste0(prefix, "_hosp_hrr")       := suppressWarnings(max(HRRCODE, na.rm = TRUE)),
              .groups = "drop") %>%
    mutate(across(where(is.numeric),
                  ~ if_else(is.infinite(.x) | is.nan(.x), NA_real_, .x))) %>%
    # Solo hospitals (no SYSID) get system measures = own-hospital values
    mutate(!!paste0(prefix, "_sys_size")        := coalesce(.data[[paste0(prefix, "_sys_size")]], 1),
           !!paste0(prefix, "_sys_n_cath")      := coalesce(.data[[paste0(prefix, "_sys_n_cath")]],
                                                            .data[[paste0(prefix, "_own_cath")]]),
           !!paste0(prefix, "_sys_any_cath")    := coalesce(.data[[paste0(prefix, "_sys_any_cath")]],
                                                            as.numeric(.data[[paste0(prefix, "_own_cath")]] > 0)),
           !!paste0(prefix, "_sys_share_cath")  := coalesce(.data[[paste0(prefix, "_sys_share_cath")]],
                                                            .data[[paste0(prefix, "_own_cath")]]))
}

res_meas <- attach_measures(cw_join, "ahaid_residency",  "start_yr_res", "res")
fel_meas <- attach_measures(cw_join, "ahaid_fellowship", "start_yr_fel", "fel")


# 5. Assemble final per-cardiologist crosswalk ----------------------------

out <- cw_join %>%
  select(npi, ahaid_residency, res_match_kind,
         ahaid_fellowship, fel_match_kind) %>%
  left_join(res_meas, by = "npi") %>%
  left_join(fel_meas, by = "npi")

cat("Cardiologists in exposure file:", nrow(out), "\n")
cat("  with residency AHA ID  :", sum(!is.na(out$ahaid_residency)),
    sprintf("(%.1f%%)", 100 * mean(!is.na(out$ahaid_residency))), "\n")
cat("  with res_own_cath       :", sum(!is.na(out$res_own_cath)),
    sprintf("(%.1f%%)", 100 * mean(!is.na(out$res_own_cath))), "\n")
cat("  with fellowship AHA ID :", sum(!is.na(out$ahaid_fellowship)),
    sprintf("(%.1f%%)", 100 * mean(!is.na(out$ahaid_fellowship))), "\n")
cat("  with fel_own_cath       :", sum(!is.na(out$fel_own_cath)),
    sprintf("(%.1f%%)", 100 * mean(!is.na(out$fel_own_cath))), "\n")

write_csv(out, "data/output/cardiologist_training_exposure.csv")
cat("\nWrote data/output/cardiologist_training_exposure.csv\n")
