# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-05-06
## Description:   Selection diagnostics and IPW-weighted main specs.
##                The balance table shows movers come from systematically
##                lower-intensity training environments (0.022 vs 0.035,
##                p<0.001). This script:
##                  (1) Reports within-subspecialty balance,
##                  (2) Reports IPW-weighted main specs (level + change),
##                  (3) Reports common-support-restricted spec dropping
##                      origins with extreme mover shares.

# 1. Load -----------------------------------------------------------------

analysis <- read_csv("data/output/analysis_panel.csv",
                     col_types = cols(npi = col_character(),
                                      year = col_integer(),
                                      .default = col_guess()))

# Cardiologist-level: one row per NPI for diagnostics. `mover` is ever-a-mover
# (some NPIs change practice HRR mid-panel, so first(mover) would mis-classify).
cardio_lvl <- analysis %>%
  filter(!is.na(mover)) %>%
  arrange(npi, year) %>%
  group_by(npi) %>%
  summarize(
    mover                = as.integer(any(mover == 1, na.rm = TRUE)),
    grad_year            = first(grad_year),
    gender               = first(gender),
    specialty            = first(specialty),
    intensity_med_school = first(intensity_med_school),
    hrr_med_school       = first(hrr_med_school),
    .groups = "drop"
  )


# 2. Within-subspecialty balance -----------------------------------------

# For each subspecialty, compare origin intensity for movers vs stayers
within_spec_balance <- cardio_lvl %>%
  filter(!is.na(intensity_med_school), !is.na(specialty)) %>%
  group_by(specialty, mover) %>%
  summarize(n           = n(),
            train_int   = mean(intensity_med_school, na.rm = TRUE),
            train_int_sd = sd(intensity_med_school, na.rm = TRUE),
            .groups = "drop")

# Pivot so movers and stayers are side by side, plus diff and t-test p-value
within_spec_wide <- within_spec_balance %>%
  pivot_wider(names_from = mover,
              values_from = c(n, train_int, train_int_sd),
              names_glue = "{.value}_{ifelse(mover==1,'mover','stayer')}")

# Recompute t-test per specialty
ttest_within_spec <- cardio_lvl %>%
  filter(!is.na(intensity_med_school), !is.na(specialty)) %>%
  group_by(specialty) %>%
  summarize(
    p = tryCatch(
      t.test(intensity_med_school[mover == 1],
             intensity_med_school[mover == 0])$p.value,
      error = function(e) NA_real_),
    .groups = "drop")

within_spec_out <- within_spec_balance %>%
  pivot_wider(names_from = mover,
              values_from = c(n, train_int),
              names_glue = "{.value}_{ifelse(mover==1,'mover','stayer')}") %>%
  left_join(ttest_within_spec, by = "specialty") %>%
  mutate(diff = train_int_mover - train_int_stayer) %>%
  arrange(desc(n_mover + n_stayer))

cat("\n=== Within-subspecialty balance: origin intensity ===\n")
print(within_spec_out)

write_csv(within_spec_out, "results/tables/balance-by-specialty.csv")


# 3. Propensity score model and IPW ---------------------------------------

# Model P(mover | observables) at the cardiologist level using
# pre-treatment characteristics. We include intensity_med_school in the
# propensity precisely because that's where the imbalance is.
ps_data <- cardio_lvl %>%
  filter(!is.na(mover),
         !is.na(grad_year),
         !is.na(gender),
         !is.na(specialty),
         !is.na(intensity_med_school))

ps_model <- glm(mover ~ intensity_med_school + grad_year + gender + specialty,
                data = ps_data, family = binomial())

ps_data <- ps_data %>%
  mutate(pscore = predict(ps_model, type = "response"),
         # Stabilized IPW: marginal P(mover) / P(mover|X) for movers,
         # marginal P(stayer) / P(stayer|X) for stayers
         pi_marg = mean(mover),
         ipw = if_else(mover == 1,
                       pi_marg / pscore,
                       (1 - pi_marg) / (1 - pscore)))

cat("\n=== Propensity score summary ===\n")
print(summary(ps_data$pscore))
cat("\n=== IPW summary ===\n")
print(summary(ps_data$ipw))

# Trim extreme weights (1st/99th percentile) to avoid leverage from outliers
trim_lo <- quantile(ps_data$ipw, 0.01)
trim_hi <- quantile(ps_data$ipw, 0.99)
ps_data <- ps_data %>%
  mutate(ipw_trim = pmin(pmax(ipw, trim_lo), trim_hi))

# Diagnostic: post-IPW balance on origin intensity
post_ipw <- ps_data %>%
  group_by(mover) %>%
  summarize(train_int_unweighted = mean(intensity_med_school),
            train_int_ipw        = weighted.mean(intensity_med_school, ipw_trim),
            .groups = "drop")

cat("\n=== Origin intensity, unweighted vs IPW-weighted ===\n")
print(post_ipw)


# 4. IPW-weighted main specs ----------------------------------------------

# Bring IPW back onto the panel
analysis_ipw <- analysis %>%
  inner_join(ps_data %>% select(npi, ipw_trim), by = "npi") %>%
  mutate(w_ipw = n_nstemi * ipw_trim)

movers_ipw <- analysis_ipw %>%
  filter(mover == 1,
         !is.na(intensity_med_school),
         !is.na(intensity_dest_loo),
         !is.nan(intensity_dest_loo))

# Level spec, IPW-weighted
m_lvl_ipw <- feols(mean_resid_cath ~ intensity_med_school | hrr_practice + year,
                   data = movers_ipw, weights = ~w_ipw,
                   cluster = ~hrr_med_school)

# Change spec, IPW-weighted
m_chg_ipw <- feols(mean_resid_cath ~ intensity_change | hrr_practice + year,
                   data = movers_ipw, weights = ~w_ipw,
                   cluster = ~hrr_med_school)

cat("\n=== IPW-weighted level spec ===\n")
print(summary(m_lvl_ipw))
cat("\n=== IPW-weighted change spec ===\n")
print(summary(m_chg_ipw))


# 5. Side-by-side selection table ----------------------------------------

# Re-fit baseline (no IPW) on the SAME sample as the IPW spec so the
# comparison is apples-to-apples. The IPW sample is restricted to NPIs with
# complete propensity-score covariates; we mirror that restriction here.
movers_base <- analysis_ipw %>%
  filter(mover == 1,
         !is.na(intensity_med_school),
         !is.na(intensity_dest_loo),
         !is.nan(intensity_dest_loo))

m_lvl_base <- feols(mean_resid_cath ~ intensity_med_school | hrr_practice + year,
                    data = movers_base, weights = ~n_nstemi,
                    cluster = ~hrr_med_school)
m_chg_base <- feols(mean_resid_cath ~ intensity_change | hrr_practice + year,
                    data = movers_base, weights = ~n_nstemi,
                    cluster = ~hrr_med_school)

# Helper to pull (est, se, p) for a term
sel_row <- function(model, term) {
  td <- tidy(model)
  hit <- td %>% filter(term == !!term)
  list(est = hit$estimate[1], se = hit$std.error[1], p = hit$p.value[1])
}

fmt_est <- function(x, p = NULL) {
  if (length(x) == 0 || is.na(x)) return(" ")
  s <- sprintf("%.3f", x)
  if (!is.null(p) && !is.na(p)) {
    if (p < 0.01) s <- paste0(s, "***")
    else if (p < 0.05) s <- paste0(s, "**")
    else if (p < 0.10) s <- paste0(s, "*")
  }
  s
}
fmt_se <- function(x) {
  if (length(x) == 0 || is.na(x)) return(" ")
  paste0("(", sprintf("%.3f", x), ")")
}

lvl_rows <- map(list(m_lvl_base, m_lvl_ipw),
                sel_row, term = "intensity_med_school")
chg_rows <- map(list(m_chg_base, m_chg_ipw),
                sel_row, term = "intensity_change")

body_sel <- tribble(
  ~term, ~`(1)`, ~`(2)`,
  "Med school HRR intensity",
    fmt_est(lvl_rows[[1]]$est, lvl_rows[[1]]$p),
    fmt_est(lvl_rows[[2]]$est, lvl_rows[[2]]$p),
  "",
    fmt_se(lvl_rows[[1]]$se),
    fmt_se(lvl_rows[[2]]$se),
  "$\\Delta$ intensity",
    fmt_est(chg_rows[[1]]$est, chg_rows[[1]]$p),
    fmt_est(chg_rows[[2]]$est, chg_rows[[2]]$p),
  "",
    fmt_se(chg_rows[[1]]$se),
    fmt_se(chg_rows[[2]]$se)
)

footer_sel <- tribble(
  ~term, ~`(1)`, ~`(2)`,
  "Practice HRR FE", "Yes", "Yes",
  "Year FE",         "Yes", "Yes",
  "IPW",             "No",  "Yes",
  "Observations",
    format(nobs(m_lvl_base), big.mark = ","),
    format(nobs(m_lvl_ipw),  big.mark = ",")
)

table_sel <- bind_rows(body_sel, footer_sel)

kable(table_sel,
      format    = "latex",
      booktabs  = TRUE,
      linesep   = "",
      escape    = FALSE,
      align     = c("l", rep("c", 2)),
      col.names = c("", "Baseline", "IPW")) %>%
  row_spec(4, extra_latex_after = "\\addlinespace") %>%
  row_spec(7, extra_latex_after = "\\midrule") %>%
  save_kable("results/tables/selection.tex")

cat("\n=== Wrote results/tables/selection.tex ===\n")


# 6. IPW on the AHA training-imprint specification -----------------------

# The headline imprint specification in the main paper uses the year-
# matched AHA cath-lab share, identified within-origin across cohorts.
# The IPW exercise above uses the older peer-cath training measure for
# parallel-spec comparability. Here we re-run IPW on the AHA spec so the
# selection-robustness statement in the main paper rests on the same
# specification that produces the headline coefficient.

aha_hosp <- read_csv("data/input/aha_hospital.csv", show_col_types = FALSE,
                     col_types = cols(HRRCODE = col_integer(),
                                      year = col_integer(),
                                      CCLABHOS = col_character(),
                                      .default = col_guess()))
aha_hrr_local <- aha_hosp %>%
  filter(!is.na(HRRCODE), !is.na(year)) %>%
  mutate(has_cath = as.integer(CCLABHOS == "1")) %>%
  group_by(HRRCODE, year) %>%
  summarize(cath_lab_share = mean(has_cath, na.rm = TRUE), .groups = "drop") %>%
  rename(hrr = HRRCODE) %>%
  mutate(cath_lab_share = if_else(is.nan(cath_lab_share), NA_real_,
                                  cath_lab_share))

panel_aha <- analysis %>%
  mutate(med_school_start = grad_year - 3L,
         aha_match_year   = pmin(pmax(med_school_start, 1980L), 2003L)) %>%
  left_join(aha_hrr_local %>% rename(train_cath_lab = cath_lab_share),
            by = c("hrr_med_school" = "hrr",
                   "aha_match_year" = "year"))

# Cardiologist-level frame for propensity scoring on AHA training measure
cardio_aha <- panel_aha %>%
  filter(!is.na(mover)) %>%
  arrange(npi, year) %>%
  group_by(npi) %>%
  summarize(
    mover          = as.integer(any(mover == 1, na.rm = TRUE)),
    grad_year      = first(grad_year),
    gender         = first(gender),
    specialty      = first(specialty),
    train_cath_lab = first(train_cath_lab),
    hrr_med_school = first(hrr_med_school),
    .groups = "drop") %>%
  filter(!is.na(grad_year), !is.na(gender), !is.na(specialty),
         !is.na(train_cath_lab))

ps_aha <- glm(mover ~ train_cath_lab + grad_year + gender + specialty,
              data = cardio_aha, family = binomial())
cardio_aha <- cardio_aha %>%
  mutate(pscore  = predict(ps_aha, type = "response"),
         pi_marg = mean(mover),
         ipw     = if_else(mover == 1, pi_marg / pscore,
                           (1 - pi_marg) / (1 - pscore)))
trim_lo_aha <- quantile(cardio_aha$ipw, 0.01)
trim_hi_aha <- quantile(cardio_aha$ipw, 0.99)
cardio_aha <- cardio_aha %>%
  mutate(ipw_trim = pmin(pmax(ipw, trim_lo_aha), trim_hi_aha))

panel_aha_ipw <- panel_aha %>%
  inner_join(cardio_aha %>% select(npi, ipw_trim), by = "npi")

clean_aha <- panel_aha_ipw %>%
  filter(!is.na(train_cath_lab),
         !is.na(intensity_dest_loo),
         !is.nan(intensity_dest_loo),
         !is.na(mean_resid_cath),
         grad_year >= 1983, grad_year <= 2006) %>%
  mutate(w_unweighted = n_nstemi,
         w_ipw        = n_nstemi * ipw_trim)

# National cath-lab share by matriculation year: the measured national
# trend used in the cohort-robustness checks. A region-by-cohort treatment
# (cath lab availability) is partly the national rollout, so we want to
# confirm the imprint survives controlling for that national level.
nat_share <- aha_hosp %>%
  filter(!is.na(year)) %>%
  mutate(has_cath = as.integer(CCLABHOS == "1")) %>%
  group_by(year) %>%
  summarize(national_cath_share = mean(has_cath, na.rm = TRUE), .groups = "drop")

clean_aha <- clean_aha %>%
  left_join(nat_share, by = c("aha_match_year" = "year")) %>%
  mutate(grad_c = grad_year - 1995)   # centered for the cohort trend

# Main selection-aha table (cols 1-3):
#   (1) Baseline (matches Table 3 Panel B col 1)
#   (2) + IPW
#   (3) + linear graduation-year trend (isolates within-HRR variation net
#       of the national cohort trend)
m_aha_base <- feols(mean_resid_cath ~ train_cath_lab |
                      hrr_med_school + hrr_practice + year,
                    data = clean_aha, weights = ~w_unweighted,
                    cluster = ~hrr_med_school)
m_aha_ipw  <- feols(mean_resid_cath ~ train_cath_lab |
                      hrr_med_school + hrr_practice + year,
                    data = clean_aha, weights = ~w_ipw,
                    cluster = ~hrr_med_school)
m_aha_lin  <- feols(mean_resid_cath ~ train_cath_lab + grad_c |
                      hrr_med_school + hrr_practice + year,
                    data = clean_aha, weights = ~w_unweighted,
                    cluster = ~hrr_med_school)

cat("\n=== AHA spec, baseline ===\n");               print(summary(m_aha_base))
cat("\n=== AHA spec, IPW-weighted ===\n");           print(summary(m_aha_ipw))
cat("\n=== AHA spec, linear grad-year trend ===\n"); print(summary(m_aha_lin))

aha_b <- sel_row(m_aha_base, "train_cath_lab")
aha_i <- sel_row(m_aha_ipw,  "train_cath_lab")
aha_l <- sel_row(m_aha_lin,  "train_cath_lab")

obs_row_3 <- function(models) {
  paste0("Observations & ",
         paste(sapply(models, function(m) format(nobs(m), big.mark = ",")),
               collapse = " & "),
         " \\\\\n")
}

tbl_aha_sel <- paste0(
  "\\begin{tabular}{lccc}\n",
  "\\toprule\n",
  " & (1) & (2) & (3) \\\\\n",
  "\\midrule\n",
  "Training-HRR cath lab share & ",
    fmt_est(aha_b$est, aha_b$p), " & ",
    fmt_est(aha_i$est, aha_i$p), " & ",
    fmt_est(aha_l$est, aha_l$p), " \\\\\n",
  " & ",
    fmt_se(aha_b$se), " & ",
    fmt_se(aha_i$se), " & ",
    fmt_se(aha_l$se), " \\\\\n",
  "\\midrule\n",
  "IPW & No & Yes & No \\\\\n",
  "Linear cohort trend & No & No & Yes \\\\\n",
  "Med school HRR FE & Yes & Yes & Yes \\\\\n",
  "Practice HRR FE & Yes & Yes & Yes \\\\\n",
  "Year FE & Yes & Yes & Yes \\\\\n",
  obs_row_3(list(m_aha_base, m_aha_ipw, m_aha_lin)),
  "\\bottomrule\n",
  "\\end{tabular}\n"
)
writeLines(tbl_aha_sel, "results/tables/selection-aha.tex")

cat("\n=== Wrote results/tables/selection-aha.tex ===\n")


# 7. Appendix cohort-trend robustness table ------------------------------
# Full gradient on the same within-origin sample: baseline, linear trend,
# quadratic trend, and a control for the national cath-lab share in the
# matriculation year. None saturate the cohort dimension, so each leaves
# usable within-HRR variation in cath lab availability.
m_aha_quad <- feols(mean_resid_cath ~ train_cath_lab + grad_c + I(grad_c^2) |
                      hrr_med_school + hrr_practice + year,
                    data = clean_aha, weights = ~w_unweighted,
                    cluster = ~hrr_med_school)
m_aha_nat  <- feols(mean_resid_cath ~ train_cath_lab + national_cath_share |
                      hrr_med_school + hrr_practice + year,
                    data = clean_aha, weights = ~w_unweighted,
                    cluster = ~hrr_med_school)

cat("\n=== AHA spec, quadratic grad-year trend ===\n"); print(summary(m_aha_quad))
cat("\n=== AHA spec, national cath share ===\n");       print(summary(m_aha_nat))

coh_b <- sel_row(m_aha_base, "train_cath_lab")
coh_l <- sel_row(m_aha_lin,  "train_cath_lab")
coh_q <- sel_row(m_aha_quad, "train_cath_lab")
coh_n <- sel_row(m_aha_nat,  "train_cath_lab")

obs_cohort <- paste0("Observations & ",
  paste(sapply(list(m_aha_base, m_aha_lin, m_aha_quad, m_aha_nat),
               function(m) format(nobs(m), big.mark = ",")),
        collapse = " & "), " \\\\\n")

tbl_cohort <- paste0(
  "\\begin{tabular}{lcccc}\n",
  "\\toprule\n",
  " & (1) & (2) & (3) & (4) \\\\\n",
  " & Baseline & Linear & Quadratic & Nat.\\ share \\\\\n",
  "\\midrule\n",
  "Training-HRR cath lab share & ",
    fmt_est(coh_b$est, coh_b$p), " & ",
    fmt_est(coh_l$est, coh_l$p), " & ",
    fmt_est(coh_q$est, coh_q$p), " & ",
    fmt_est(coh_n$est, coh_n$p), " \\\\\n",
  " & ",
    fmt_se(coh_b$se), " & ",
    fmt_se(coh_l$se), " & ",
    fmt_se(coh_q$se), " & ",
    fmt_se(coh_n$se), " \\\\\n",
  "\\midrule\n",
  "Graduation-year trend & None & Linear & Quadratic & None \\\\\n",
  "National cath share & No & No & No & Yes \\\\\n",
  "Med school HRR FE & Yes & Yes & Yes & Yes \\\\\n",
  "Practice HRR FE & Yes & Yes & Yes & Yes \\\\\n",
  "Year FE & Yes & Yes & Yes & Yes \\\\\n",
  obs_cohort,
  "\\bottomrule\n",
  "\\end{tabular}\n"
)
writeLines(tbl_cohort, "results/tables/cohort-robust.tex")

cat("\n=== Wrote results/tables/cohort-robust.tex ===\n")
