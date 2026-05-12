# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-05-12
## Description:   Training-pipeline analyses (parallel to 8_aha_training.R
##                for medical school, but extending to residency and
##                fellowship via the Doximity crosswalk and per-cardiologist
##                training exposure).
##
##                Specifications (residency, headline):
##                  y_it = beta * res_*  + delta_med + delta_dest + gamma_t + e
##                where delta_med is medical-school HRR FE and delta_dest is
##                practice-HRR FE. Each residency measure is also reported
##                jointly with the medical-school cath share. Robustness:
##                hand-curated + exact only (drop fuzzy), and between-stage
##                movers (med-school HRR != residency hospital HRR).
##
##                Outputs:
##                  results/tables/training-pipeline.tex   (main table)
##                  results/tables/training-pipeline-robust.tex   (robustness)
##                  results/tables/training-selection.tex   (selection tests)

# 1. Inputs ----------------------------------------------------------------

panel <- read_csv("data/output/analysis_panel.csv",
                  col_types = cols(npi = col_character(),
                                   year = col_integer(),
                                   .default = col_guess()),
                  show_col_types = FALSE)

exposure <- read_csv("data/output/cardiologist_training_exposure.csv",
                     col_types = cols(npi = col_character(),
                                      .default = col_guess()),
                     show_col_types = FALSE)

# Med-school cath share at matriculation year (parallel to 8_aha_training.R).
aha <- fread("data/input/aha_hospital.csv",
             select = c("HRRCODE", "year", "CCLABHOS"),
             na.strings = c("", "NA"), showProgress = FALSE)
setDF(aha)
hrr_year_cath <- aha %>%
  mutate(year = as.integer(year),
         HRRCODE = suppressWarnings(as.integer(HRRCODE)),
         has_cath = as.integer(CCLABHOS == "1")) %>%
  filter(!is.na(HRRCODE), !is.na(year), year >= 1980, year <= 2003) %>%
  group_by(HRRCODE, year) %>%
  summarize(cath_lab_share = mean(has_cath, na.rm = TRUE), .groups = "drop") %>%
  mutate(cath_lab_share = if_else(is.nan(cath_lab_share), NA_real_, cath_lab_share)) %>%
  rename(hrr = HRRCODE)


# 2. Build the analysis panel ----------------------------------------------

p <- panel %>%
  left_join(exposure, by = "npi") %>%
  mutate(aha_match_year_med = pmin(pmax(grad_year - 3L, 1980L), 2003L)) %>%
  left_join(hrr_year_cath %>% rename(med_cath_lab = cath_lab_share),
            by = c("hrr_med_school" = "hrr", "aha_match_year_med" = "year")) %>%
  filter(grad_year >= 1983, grad_year <= 2003)


# 3. Spec helpers ----------------------------------------------------------

est <- function(m) {
  td <- tidy(m, conf.int = TRUE)
  td %>% filter(!grepl("Intercept", term)) %>%
    mutate(across(c(estimate, std.error, conf.low, conf.high), ~ round(.x, 4))) %>%
    select(term, estimate, std.error, conf.low, conf.high, p.value)
}

run_within_origin <- function(df, lhs = "mean_resid_cath", rhs, cluster = "hrr_med_school") {
  feols(as.formula(paste(lhs, "~", rhs,
                         "| hrr_med_school + hrr_practice + year")),
        data = df, weights = ~n_nstemi,
        cluster = as.formula(paste0("~", cluster)))
}

run_joint <- function(df, rhs, cluster = "hrr_med_school") {
  feols(as.formula(paste("mean_resid_cath ~ med_cath_lab +", rhs,
                         "| hrr_med_school + hrr_practice + year")),
        data = df, weights = ~n_nstemi,
        cluster = as.formula(paste0("~", cluster)))
}


# 4. Headline residency regressions ---------------------------------------

cat("\n=== Residency exposure: standalone + joint with medical school ===\n")
res_measures <- c("res_own_cath", "res_sys_any_cath", "res_sys_share_cath")
for (m in res_measures) {
  d <- p %>% filter(!is.na(.data[[m]]), !is.na(med_cath_lab))
  cat(sprintf("\n%-22s  n_obs = %d  n_card = %d\n", m, nrow(d), n_distinct(d$npi)))
  cat("Standalone within-origin:\n"); print(est(run_within_origin(d, rhs = m)))
  cat("Joint with med school:\n");    print(est(run_joint(d, rhs = m)))
}


# 5. Robustness 1: hand-curated + exact only (drop fuzzy) -----------------

cat("\n=== R1: drop fuzzy-tier residency matches ===\n")
p_clean <- p %>% filter(res_match_kind %in% c("manual", "exact"))
cat(sprintf("Sample: %d rows; %d cardiologists\n", nrow(p_clean), n_distinct(p_clean$npi)))
for (m in res_measures) {
  d <- p_clean %>% filter(!is.na(.data[[m]]), !is.na(med_cath_lab))
  cat(sprintf("\n%-22s  n_obs = %d  n_card = %d\n", m, nrow(d), n_distinct(d$npi)))
  cat("Joint with med school:\n"); print(est(run_joint(d, rhs = m)))
}


# 6. Robustness 2: between-stage movers (med HRR != residency hosp HRR) --

cat("\n=== R2: between-stage movers (med-school HRR != residency-hospital HRR) ===\n")
p_mov <- p %>% filter(!is.na(res_hosp_hrr), !is.na(hrr_med_school),
                       res_hosp_hrr != hrr_med_school)
cat(sprintf("Sample: %d rows; %d cardiologists\n", nrow(p_mov), n_distinct(p_mov$npi)))
for (m in res_measures) {
  d <- p_mov %>% filter(!is.na(.data[[m]]), !is.na(med_cath_lab))
  cat(sprintf("\n%-22s  n_obs = %d  n_card = %d\n", m, nrow(d), n_distinct(d$npi)))
  cat("Joint with med school:\n"); print(est(run_joint(d, rhs = m)))
}


# 7. Robustness 3: within-residency-hospital cohort FE --------------------

cat("\n=== R3: within-residency-hospital cohort FE ===\n")
d3 <- p %>% filter(!is.na(res_own_cath), !is.na(med_cath_lab))
cat(sprintf("Sample: %d rows; %d cardiologists; %d residency hospitals\n",
            nrow(d3), n_distinct(d3$npi), n_distinct(d3$ahaid_residency)))
m_r3 <- feols(mean_resid_cath ~ med_cath_lab + res_own_cath |
                hrr_med_school + ahaid_residency + hrr_practice + year,
              data = d3, weights = ~n_nstemi,
              cluster = ~hrr_med_school)
cat("Joint with residency-hospital FE:\n"); print(est(m_r3))


# 8. Selection tests -------------------------------------------------------

cat("\n=== Selection: does med-school cath share predict residency placement? ===\n")
card <- p %>%
  arrange(npi, year) %>% group_by(npi) %>% slice(1) %>% ungroup()
for (rhs in c("res_own_cath", "res_sys_share_cath",
              "fel_own_cath", "fel_sys_share_cath")) {
  d <- card %>% filter(!is.na(.data[[rhs]]), !is.na(med_cath_lab))
  if (nrow(d) < 200) next
  m <- feols(as.formula(paste(rhs, "~ med_cath_lab | hrr_med_school + grad_year")),
             data = d, cluster = ~hrr_med_school)
  cat(sprintf("  %-22s  n=%5d  beta=%7.4f  SE=%6.4f  p=%5.3f\n",
              rhs, nrow(d),
              coef(m)["med_cath_lab"], se(m)["med_cath_lab"],
              fixest::pvalue(m)["med_cath_lab"]))
}


# 9. Fellowship -----------------------------------------------------------

cat("\n=== Fellowship exposure: standalone + joint ===\n")
for (m in c("fel_own_cath", "fel_sys_share_cath")) {
  d <- p %>% filter(!is.na(.data[[m]]), !is.na(med_cath_lab))
  cat(sprintf("\n%-22s  n_obs = %d  n_card = %d\n", m, nrow(d), n_distinct(d$npi)))
  cat("Standalone within-origin:\n"); print(est(run_within_origin(d, rhs = m)))
  cat("Joint with med school:\n");    print(est(run_joint(d, rhs = m)))
}


# 10. Main paper table -----------------------------------------------------

# Headline: medical school + residency own-hospital, with a second column
# showing the same-HRR-system share. Three rows: medical school coefficient,
# residency coefficient, joint vs standalone label.
m1 <- run_joint(p %>% filter(!is.na(res_own_cath),       !is.na(med_cath_lab)), "res_own_cath")
m2 <- run_joint(p %>% filter(!is.na(res_sys_share_cath), !is.na(med_cath_lab)), "res_sys_share_cath")
m3 <- run_joint(p_clean %>% filter(!is.na(res_own_cath), !is.na(med_cath_lab)), "res_own_cath")
m4 <- run_joint(p_clean %>% filter(!is.na(res_sys_share_cath), !is.na(med_cath_lab)), "res_sys_share_cath")

fmt_coef <- function(m, term) {
  td <- tidy(m)
  row <- td %>% filter(.data$term == !!term)
  if (nrow(row) == 0) return("")
  stars <- ifelse(row$p.value < 0.01, "***",
                  ifelse(row$p.value < 0.05, "**",
                         ifelse(row$p.value < 0.10, "*", "")))
  sprintf("%.3f%s\\\\(%.3f)", row$estimate, stars, row$std.error)
}

cell <- function(m, term) fmt_coef(m, term)
n_obs  <- function(m) format(nobs(m), big.mark = ",")
n_card <- function(d, m) format(n_distinct(d$npi[!is.na(d[[m]]) & !is.na(d$med_cath_lab)]), big.mark = ",")

# Build a simple 2-panel table (full sample vs clean-only subsample),
# 2 columns each (own-hospital, system-share).
tbl <- paste0(
  "\\begin{tabular}{lcccc}\n",
  "\\toprule\n",
  " & \\multicolumn{2}{c}{Full sample} & \\multicolumn{2}{c}{Manual+exact only} \\\\\n",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}\n",
  " & Own & System & Own & System \\\\\n",
  " & hospital & share & hospital & share \\\\\n",
  "\\midrule\n",
  "Medical school cath share & ",
    cell(m1, "med_cath_lab"), " & ",
    cell(m2, "med_cath_lab"), " & ",
    cell(m3, "med_cath_lab"), " & ",
    cell(m4, "med_cath_lab"), " \\\\\n",
  "Residency exposure & ",
    cell(m1, "res_own_cath"), " & ",
    cell(m2, "res_sys_share_cath"), " & ",
    cell(m3, "res_own_cath"), " & ",
    cell(m4, "res_sys_share_cath"), " \\\\\n",
  "\\midrule\n",
  "Cardiologist-years & ",
    n_obs(m1), " & ", n_obs(m2), " & ", n_obs(m3), " & ", n_obs(m4), " \\\\\n",
  "Cardiologists & ",
    n_card(p,       "res_own_cath"), " & ",
    n_card(p,       "res_sys_share_cath"), " & ",
    n_card(p_clean, "res_own_cath"), " & ",
    n_card(p_clean, "res_sys_share_cath"), " \\\\\n",
  "Med-school HRR FE & Yes & Yes & Yes & Yes \\\\\n",
  "Practice HRR FE & Yes & Yes & Yes & Yes \\\\\n",
  "Year FE & Yes & Yes & Yes & Yes \\\\\n",
  "\\bottomrule\n",
  "\\end{tabular}\n"
)
# kableExtra writes a \makecell-style cell with "\\" inside a single cell.
# The fmt_coef output above embeds the SE on a new line; rewrite to use
# explicit \shortstack so the LaTeX is valid in tabular cells.
tbl <- str_replace_all(tbl,
                       "([0-9\\.\\-]+\\*{0,3})\\\\\\\\\\(([0-9\\.\\-]+)\\)",
                       "\\\\shortstack[c]{\\1 \\\\\\\\ (\\2)}")
writeLines(tbl, "results/tables/training-pipeline.tex")
cat("\nWrote results/tables/training-pipeline.tex\n")
