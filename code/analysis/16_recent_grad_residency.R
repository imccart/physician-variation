# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-05-14
## Description:   Direct-exposure residency test using the VRDC hospital-year
##                NSTEMI cath-within-2-days rate panel. For cardiologists who
##                graduated medical school in 2006 or later, the residency
##                program's contemporaneous cath rate (averaged over PGY1-PGY3
##                where data is available) is observable on the VRDC seat. This
##                is a tighter analogue to the medical-school AHA cath-share
##                imprint than the cross-sectional hospital cath-lab indicator
##                used in 15_training_pipeline.R.
##
##                The test exploits within-origin variation: practice HRR FE,
##                medical-school HRR FE, and year FE soak up current and
##                origin location, so beta on residency_cath_rate captures the
##                imprint of residency exposure on a recent-grad cardiologist
##                in steady-state practice.
##
##                Inputs:
##                  data/input/hospital_year_cath.csv          (VRDC export)
##                  data/input/aha_hospital.csv                (ID-year-MCRNUM)
##                  data/output/analysis_panel.csv
##                  data/output/cardiologist_training_exposure.csv
##
##                Output:
##                  results/tables/training-recent-grad.tex

# 1. Inputs ----------------------------------------------------------------

hosp_cath <- read_csv("data/input/hospital_year_cath.csv",
                      col_types = cols(prvdr_num = col_character(),
                                       year = col_integer(),
                                       n_nstemi = col_double(),
                                       n_cath_d2 = col_double(),
                                       rate_cath_d2 = col_double()),
                      show_col_types = FALSE)

aha_bridge <- fread("data/input/aha_hospital.csv",
                    select = c("ID", "year", "MCRNUM"),
                    colClasses = c(ID = "character", MCRNUM = "character"),
                    na.strings = c("", "NA"), showProgress = FALSE)
setDF(aha_bridge)
aha_bridge <- aha_bridge %>%
  filter(!is.na(MCRNUM), !is.na(year)) %>%
  mutate(MCRNUM = str_pad(MCRNUM, 6, pad = "0")) %>%
  distinct(ID, year, MCRNUM)

panel <- read_csv("data/output/analysis_panel.csv",
                  col_types = cols(npi = col_character(),
                                   year = col_integer(),
                                   .default = col_guess()),
                  show_col_types = FALSE)

exposure <- read_csv("data/output/cardiologist_training_exposure.csv",
                     col_types = cols(npi = col_character(),
                                      ahaid_residency = col_character(),
                                      ahaid_fellowship = col_character(),
                                      .default = col_guess()),
                     show_col_types = FALSE)


# 2. Per-AHA-year residency cath rate (via MCRNUM->CCN bridge) ------------

ahaid_year_cath <- aha_bridge %>%
  inner_join(hosp_cath %>%
               mutate(prvdr_num = str_pad(prvdr_num, 6, pad = "0")) %>%
               select(prvdr_num, year, rate_cath_d2),
             by = c("MCRNUM" = "prvdr_num", "year" = "year")) %>%
  filter(!is.na(rate_cath_d2)) %>%
  distinct(ID, year, rate_cath_d2)


# 3. Recent-grad residency exposure measure -------------------------------

# For each cardiologist with grad_year >= 2006 and an AHA-mapped residency
# program, average the residency hospital's contemporaneous cath rate over
# PGY1-PGY3 (= grad_year, grad_year+1, grad_year+2). VRDC covers 2008-2018,
# so PGY years before 2008 are dropped from the average.

card <- panel %>%
  distinct(npi, grad_year, hrr_med_school) %>%
  inner_join(exposure %>%
               select(npi, ahaid_residency, res_match_kind),
             by = "npi") %>%
  filter(!is.na(ahaid_residency), grad_year >= 2006)

pgy_grid <- card %>%
  mutate(pgy = list(0:2)) %>%
  unnest(pgy) %>%
  mutate(pgy_year = grad_year + pgy) %>%
  filter(pgy_year >= 2008, pgy_year <= 2018)

res_rate <- pgy_grid %>%
  left_join(ahaid_year_cath, by = c("ahaid_residency" = "ID",
                                    "pgy_year" = "year")) %>%
  group_by(npi) %>%
  summarize(res_cath_rate_pgy = mean(rate_cath_d2, na.rm = TRUE),
            n_pgy_years_obs = sum(!is.na(rate_cath_d2)),
            .groups = "drop") %>%
  mutate(res_cath_rate_pgy = if_else(is.nan(res_cath_rate_pgy),
                                     NA_real_, res_cath_rate_pgy))


# 4. Build analysis panel --------------------------------------------------

p <- panel %>%
  inner_join(res_rate, by = "npi") %>%
  filter(grad_year >= 2006,
         !is.na(res_cath_rate_pgy),
         n_pgy_years_obs >= 1)

cat(sprintf("\nRecent-grad sample: %d cardiologist-years, %d cardiologists\n",
            nrow(p), n_distinct(p$npi)))
cat(sprintf("PGY-years observed per cardiologist: mean = %.2f, min = %d, max = %d\n",
            mean(p$n_pgy_years_obs[!duplicated(p$npi)]),
            min(p$n_pgy_years_obs[!duplicated(p$npi)]),
            max(p$n_pgy_years_obs[!duplicated(p$npi)])))


# 5. Specifications --------------------------------------------------------

est <- function(m) {
  td <- tidy(m, conf.int = TRUE)
  td %>% filter(!grepl("Intercept", term)) %>%
    mutate(across(c(estimate, std.error, conf.low, conf.high), ~ round(.x, 4))) %>%
    select(term, estimate, std.error, conf.low, conf.high, p.value)
}

# Spec 1: residency PGY cath rate alone, within practice HRR + year FE.
m1 <- feols(mean_resid_cath ~ res_cath_rate_pgy | hrr_practice + year,
            data = p, weights = ~n_nstemi, cluster = ~npi)

# Spec 2: add medical-school HRR FE (within-origin identification).
m2 <- feols(mean_resid_cath ~ res_cath_rate_pgy | hrr_med_school + hrr_practice + year,
            data = p, weights = ~n_nstemi, cluster = ~hrr_med_school)

# Spec 3: manual+exact residency matches only (drop fuzzy tier).
p_clean <- p %>%
  left_join(exposure %>% select(npi, res_match_kind), by = "npi") %>%
  filter(res_match_kind %in% c("manual", "exact"))
m3 <- feols(mean_resid_cath ~ res_cath_rate_pgy | hrr_med_school + hrr_practice + year,
            data = p_clean, weights = ~n_nstemi, cluster = ~hrr_med_school)

cat("\n=== Spec 1: practice-HRR + year FE ===\n");                  print(est(m1))
cat("\n=== Spec 2: + medical-school HRR FE (within-origin) ===\n"); print(est(m2))
cat("\n=== Spec 3: manual+exact matches only ===\n");               print(est(m3))


# 6. Output table ----------------------------------------------------------

fmt_coef <- function(m, term) {
  td <- tidy(m)
  row <- td %>% filter(.data$term == !!term)
  if (nrow(row) == 0) return("")
  stars <- ifelse(row$p.value < 0.01, "***",
                  ifelse(row$p.value < 0.05, "**",
                         ifelse(row$p.value < 0.10, "*", "")))
  sprintf("\\shortstack[c]{%.3f%s \\\\ (%.3f)}",
          row$estimate, stars, row$std.error)
}
n_obs  <- function(m) format(nobs(m), big.mark = ",")
n_card <- function(d) format(n_distinct(d$npi), big.mark = ",")

tbl <- paste0(
  "\\begin{tabular}{lccc}\n",
  "\\toprule\n",
  " & (1) & (2) & (3) \\\\\n",
  " & Practice & Within & Manual+exact \\\\\n",
  " & HRR FE & origin & only \\\\\n",
  "\\midrule\n",
  "Residency cath rate (PGY1-3) & ",
    fmt_coef(m1, "res_cath_rate_pgy"), " & ",
    fmt_coef(m2, "res_cath_rate_pgy"), " & ",
    fmt_coef(m3, "res_cath_rate_pgy"), " \\\\\n",
  "\\midrule\n",
  "Cardiologist-years & ", n_obs(m1), " & ", n_obs(m2), " & ", n_obs(m3), " \\\\\n",
  "Cardiologists & ", n_card(p), " & ", n_card(p), " & ", n_card(p_clean), " \\\\\n",
  "Practice HRR FE & Yes & Yes & Yes \\\\\n",
  "Medical-school HRR FE & No & Yes & Yes \\\\\n",
  "Year FE & Yes & Yes & Yes \\\\\n",
  "\\bottomrule\n",
  "\\end{tabular}\n"
)
writeLines(tbl, "results/tables/training-recent-grad.tex")
cat("\nWrote results/tables/training-recent-grad.tex\n")
