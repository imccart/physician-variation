# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-05-06
## Description:   AHA-based training-period HRR cardiology infrastructure
##                as the training-environment proxy. Uses AHA hospital-
##                level cath lab / open heart / cardiac ICU indicators
##                aggregated to (HRR, year), then matched to each
##                cardiologist's actual training period.
##
##                Training period: cardiologist with grad_year G was a
##                medical student in years (G-3) to G. We match each
##                cardiologist to AHA HRR-year data in year G-3 (start of
##                med school), or the closest available year if G-3 is
##                outside the AHA coverage window.
##
##                AHA cath lab + open heart coverage: 1980-2003.
##                Cardiac ICU coverage: 1980-1985, 1994-2024.

# 1. Load -----------------------------------------------------------------

analysis <- read_csv("data/output/analysis_panel.csv",
                     col_types = cols(npi = col_character(),
                                      year = col_integer(),
                                      .default = col_guess()))

# Hospital-level AHA panel (1980-2024) symlinked from aha-data repo.
# We aggregate to HRR-year here rather than as a separate output, since
# the aggregation is specific to this project's training-environment proxy.
aha_hosp <- read_csv("data/input/aha_hospital.csv", show_col_types = FALSE,
                     col_types = cols(
                       HRRCODE = col_integer(), year = col_integer(),
                       CCLABHOS = col_character(), OHSRGHOS = col_character(),
                       CICHOS   = col_character(), ACARDHOS = col_character(),
                       CICBD    = col_double(),
                       teach_major = col_double(), teach_minor = col_double(),
                       MAPP3 = col_character(),
                       .default = col_guess()
                     ))

# AHA service codes: 1 = hospital provides directly. Other codes mean no /
# subsidiary / system / network / joint-venture; treat all as 0 here.
aha_hrr <- aha_hosp %>%
  filter(!is.na(HRRCODE), !is.na(year)) %>%
  mutate(
    has_cath_lab    = as.integer(CCLABHOS == "1"),
    has_open_heart  = as.integer(OHSRGHOS == "1"),
    has_cardiac_icu = as.integer(CICHOS   == "1")
  ) %>%
  group_by(HRRCODE, year) %>%
  summarize(
    cath_lab_share    = mean(has_cath_lab,    na.rm = TRUE),
    open_heart_share  = mean(has_open_heart,  na.rm = TRUE),
    cardiac_icu_share = mean(has_cardiac_icu, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(hrr = HRRCODE) %>%
  mutate(across(ends_with("_share"),
                ~ if_else(is.nan(.x), NA_real_, .x)))

# Teaching-hospital cath-lab share: same construction but restricted to
# hospitals flagged as teaching. We use the broad teaching definition,
# major OR minor, to capture the rotation network. Major teaching
# hospitals are the principal sites; minor teaching hospitals are
# typically community hospitals connected to a larger teaching center
# where students rotate during clerkships.
aha_hrr_teach <- aha_hosp %>%
  filter(!is.na(HRRCODE), !is.na(year)) %>%
  mutate(
    is_teaching = as.integer(
      (!is.na(teach_major) & teach_major == 1) |
      (!is.na(teach_minor) & teach_minor == 1)
    ),
    has_cath_lab = as.integer(CCLABHOS == "1")
  ) %>%
  filter(is_teaching == 1) %>%
  group_by(HRRCODE, year) %>%
  summarize(
    cath_lab_share_teach = mean(has_cath_lab, na.rm = TRUE),
    n_teach              = n(),
    .groups = "drop"
  ) %>%
  rename(hrr = HRRCODE) %>%
  mutate(cath_lab_share_teach = if_else(is.nan(cath_lab_share_teach),
                                        NA_real_, cath_lab_share_teach))

aha_hrr <- aha_hrr %>%
  left_join(aha_hrr_teach, by = c("hrr", "year"))

cat("HRR-year cardiology infrastructure panel (built inline):\n")
cat("  Rows:       ", nrow(aha_hrr), "\n")
cat("  HRRs:       ", n_distinct(aha_hrr$hrr), "\n")
cat("  Year range: ", range(aha_hrr$year), "\n\n")


# 2. Match each cardiologist to training-period HRR-year ------------------

# Year of medical school start: grad_year - 3. Med school is ~4 years.
# Use start-of-med-school as the "exposure" measurement.
phys_train <- analysis %>%
  filter(!is.na(grad_year), !is.na(hrr_med_school)) %>%
  distinct(npi, hrr_med_school, grad_year) %>%
  mutate(med_school_start = grad_year - 3)

# AHA cath lab coverage: 1980-2003. Truncate exposure year to that window.
# Pre-1980 grads (started med school before 1977) get the 1980 value.
# Post-2003 grads (started med school after 2003) get the 2003 value.
phys_train <- phys_train %>%
  mutate(
    aha_match_year = pmin(pmax(med_school_start, 1980L), 2003L)
  )

# Match: bring training-period AHA share onto each (npi, hrr_med_school)
training_intensity <- phys_train %>%
  left_join(aha_hrr %>% select(hrr, year,
                               cath_lab_share, open_heart_share,
                               cardiac_icu_share, cath_lab_share_teach),
            by = c("hrr_med_school" = "hrr",
                   "aha_match_year"  = "year"))

cat("Cardiologist x training-HRR matches:\n")
cat("  rows:                          ", nrow(training_intensity), "\n")
cat("  with cath_lab_share matched:   ",
    sum(!is.na(training_intensity$cath_lab_share)), "\n")
cat("  with open_heart_share matched: ",
    sum(!is.na(training_intensity$open_heart_share)), "\n")
cat("  with cardiac_icu_share matched:",
    sum(!is.na(training_intensity$cardiac_icu_share)), "\n")
cat("\nDistribution of training cath-lab share:\n")
print(summary(training_intensity$cath_lab_share))


# 3. Bring onto the panel for regression ---------------------------------

panel <- analysis %>%
  left_join(training_intensity %>%
              select(npi, hrr_med_school,
                     train_cath_lab       = cath_lab_share,
                     train_open_heart     = open_heart_share,
                     train_cardiac_icu    = cardiac_icu_share,
                     train_cath_lab_teach = cath_lab_share_teach,
                     med_school_start, aha_match_year),
            by = c("npi", "hrr_med_school"))


# 4. Main specs: cath rate ~ training-HRR cardiac infrastructure --------

# Full sample (movers + stayers). Training measure is a school-level fixed
# attribute, so identification comes from cross-cardiologist comparisons
# within destination HRR + year.
reg_data <- panel %>%
  filter(!is.na(train_cath_lab),
         !is.na(mean_resid_cath))

cat("\nRegression sample size:", nrow(reg_data), "rows,",
    n_distinct(reg_data$npi), "cardiologists\n")

m_cath_lab <- feols(mean_resid_cath ~ train_cath_lab |
                      hrr_practice + year,
                    data = reg_data, weights = ~n_nstemi,
                    cluster = ~hrr_med_school)
m_open_heart <- feols(mean_resid_cath ~ train_open_heart |
                        hrr_practice + year,
                      data = reg_data %>% filter(!is.na(train_open_heart)),
                      weights = ~n_nstemi, cluster = ~hrr_med_school)
m_cardiac_icu <- feols(mean_resid_cath ~ train_cardiac_icu |
                         hrr_practice + year,
                       data = reg_data %>% filter(!is.na(train_cardiac_icu)),
                       weights = ~n_nstemi, cluster = ~hrr_med_school)

cat("\n=== Spec A: training cath lab share ===\n");      print(summary(m_cath_lab))
cat("\n=== Spec B: training open heart share ===\n");    print(summary(m_open_heart))
cat("\n=== Spec C: training cardiac ICU share ===\n");   print(summary(m_cardiac_icu))


# 5. Movers-only ---------------------------------------------------------

mov <- reg_data %>% filter(mover == 1)

m_cath_mov  <- feols(mean_resid_cath ~ train_cath_lab |
                       hrr_practice + year,
                     data = mov, weights = ~n_nstemi,
                     cluster = ~hrr_med_school)
m_oh_mov    <- feols(mean_resid_cath ~ train_open_heart |
                       hrr_practice + year,
                     data = mov %>% filter(!is.na(train_open_heart)),
                     weights = ~n_nstemi, cluster = ~hrr_med_school)
m_cic_mov   <- feols(mean_resid_cath ~ train_cardiac_icu |
                       hrr_practice + year,
                     data = mov %>% filter(!is.na(train_cardiac_icu)),
                     weights = ~n_nstemi, cluster = ~hrr_med_school)

cat("\n=== Movers only ===\n")
cat("--- cath lab ---\n");        print(summary(m_cath_mov))
cat("--- open heart ---\n");      print(summary(m_oh_mov))
cat("--- cardiac ICU ---\n");     print(summary(m_cic_mov))


# 6. Restrict to grads with actual training-period coverage ---------------

# Above we clamped pre-1980 / post-2003 to the boundary. That introduces
# noise. Restrict to grad_year 1983-2006 so med_school_start = grad_year-3
# stays inside [1980, 2003] without truncation.
clean <- reg_data %>%
  filter(grad_year >= 1983, grad_year <= 2006)

m_cath_clean <- feols(mean_resid_cath ~ train_cath_lab |
                        hrr_practice + year,
                      data = clean, weights = ~n_nstemi,
                      cluster = ~hrr_med_school)
m_oh_clean   <- feols(mean_resid_cath ~ train_open_heart |
                        hrr_practice + year,
                      data = clean, weights = ~n_nstemi,
                      cluster = ~hrr_med_school)

cat("\n=== Clean grad-year range (1983-2006) ===\n")
cat("N rows:", nrow(clean), " N cardio:", n_distinct(clean$npi), "\n")
cat("--- cath lab ---\n");        print(summary(m_cath_clean))
cat("--- open heart ---\n");      print(summary(m_oh_clean))


# 7. Export side-by-side table -------------------------------------------

mc_row <- function(model, term) {
  td <- tidy(model)
  hit <- td %>% filter(term == !!term)
  if (nrow(hit) == 0) {
    list(est = NA_real_, se = NA_real_, p = NA_real_)
  } else {
    list(est = hit$estimate[1], se = hit$std.error[1], p = hit$p.value[1])
  }
}
fmt_e <- function(x, p = NULL) {
  if (length(x) == 0 || is.na(x)) return(" ")
  s <- sprintf("%.3f", x)
  if (!is.null(p) && !is.na(p)) {
    if (p < 0.01) s <- paste0(s, "***")
    else if (p < 0.05) s <- paste0(s, "**")
    else if (p < 0.10) s <- paste0(s, "*")
  }
  s
}
fmt_s <- function(x) {
  if (length(x) == 0 || is.na(x)) return(" ")
  paste0("(", sprintf("%.3f", x), ")")
}

cl_full <- mc_row(m_cath_lab,    "train_cath_lab")
oh_full <- mc_row(m_open_heart,  "train_open_heart")
ci_full <- mc_row(m_cardiac_icu, "train_cardiac_icu")
cl_mov  <- mc_row(m_cath_mov,    "train_cath_lab")
oh_mov  <- mc_row(m_oh_mov,      "train_open_heart")
ci_mov  <- mc_row(m_cic_mov,     "train_cardiac_icu")

body_aha <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`, ~`(5)`, ~`(6)`,
  "Cath lab share, training HRR",
    fmt_e(cl_full$est, cl_full$p),  " ", " ",  fmt_e(cl_mov$est, cl_mov$p),  " ", " ",
  "",
    fmt_s(cl_full$se),               " ", " ",  fmt_s(cl_mov$se),             " ", " ",
  "Open heart share, training HRR",
    " ", fmt_e(oh_full$est, oh_full$p), " ",   " ", fmt_e(oh_mov$est, oh_mov$p), " ",
  "",
    " ", fmt_s(oh_full$se),               " ", " ", fmt_s(oh_mov$se),               " ",
  "Cardiac ICU share, training HRR",
    " ", " ", fmt_e(ci_full$est, ci_full$p),   " ", " ", fmt_e(ci_mov$est, ci_mov$p),
  "",
    " ", " ", fmt_s(ci_full$se),               " ", " ", fmt_s(ci_mov$se)
)

footer_aha <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`, ~`(5)`, ~`(6)`,
  "Practice HRR FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
  "Year FE",         "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
  "Sample",          "Full", "Full", "Full", "Movers", "Movers", "Movers",
  "Observations",
    format(nobs(m_cath_lab),    big.mark = ","),
    format(nobs(m_open_heart),  big.mark = ","),
    format(nobs(m_cardiac_icu), big.mark = ","),
    format(nobs(m_cath_mov),    big.mark = ","),
    format(nobs(m_oh_mov),      big.mark = ","),
    format(nobs(m_cic_mov),     big.mark = ",")
)

table_aha <- bind_rows(body_aha, footer_aha)

kable(table_aha,
      format    = "latex",
      booktabs  = TRUE,
      linesep   = "",
      escape    = FALSE,
      align     = c("l", rep("c", 6)),
      col.names = c("", "Cath", "OH", "CICU",
                    "Cath", "OH", "CICU")) %>%
  row_spec(2, extra_latex_after = "\\addlinespace") %>%
  row_spec(4, extra_latex_after = "\\addlinespace") %>%
  row_spec(6, extra_latex_after = "\\midrule") %>%
  save_kable("results/tables/aha-training.tex")

cat("\n=== Wrote results/tables/aha-training.tex ===\n")


# 8. Heterogeneity by subspecialty ---------------------------------------

# Caveat: training measure is medical-school exposure, not residency or
# fellowship. So this isn't "med school taught IC's how to cath" -- it's
# "did the procedural orientation imprinted in med school predict cath
# rate today, conditional on the subspecialty they ended up in?".
# Subspecialty choice is partly endogenous to the imprint, so within-
# subspecialty estimates may attenuate (the selection channel is shut).

clean_spec <- clean %>% filter(!is.na(specialty))

m_genl <- feols(mean_resid_cath ~ train_cath_lab | hrr_practice + year,
                data = clean_spec %>% filter(specialty == "Cardiology"),
                weights = ~n_nstemi, cluster = ~hrr_med_school)
m_ic   <- feols(mean_resid_cath ~ train_cath_lab | hrr_practice + year,
                data = clean_spec %>% filter(specialty == "Interventional Cardiology"),
                weights = ~n_nstemi, cluster = ~hrr_med_school)
m_ep   <- feols(mean_resid_cath ~ train_cath_lab | hrr_practice + year,
                data = clean_spec %>% filter(specialty == "Clinical Cardiac Electrophysiology"),
                weights = ~n_nstemi, cluster = ~hrr_med_school)
m_hf   <- feols(mean_resid_cath ~ train_cath_lab | hrr_practice + year,
                data = clean_spec %>% filter(specialty == "Advanced Heart Failure and Transplant Cardiology"),
                weights = ~n_nstemi, cluster = ~hrr_med_school)

cat("\n=== Subspecialty heterogeneity (clean grad-year window) ===\n")
cat("\n--- General Cardiology ---\n");                  print(summary(m_genl))
cat("\n--- Interventional Cardiology ---\n");           print(summary(m_ic))
cat("\n--- Electrophysiology ---\n");                   print(summary(m_ep))
cat("\n--- Adv Heart Failure ---\n");                   print(summary(m_hf))

# Pooled with interaction (formal test of equality across specialties)
m_inter <- feols(mean_resid_cath ~ train_cath_lab * specialty |
                   hrr_practice + year,
                 data = clean_spec, weights = ~n_nstemi,
                 cluster = ~hrr_med_school)
cat("\n--- Interaction spec ---\n")
print(summary(m_inter))

# Build a table
gn  <- mc_row(m_genl, "train_cath_lab")
ic  <- mc_row(m_ic,   "train_cath_lab")
ep  <- mc_row(m_ep,   "train_cath_lab")
hf  <- mc_row(m_hf,   "train_cath_lab")

body_sp <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`,
  "Cath lab share, training HRR",
    fmt_e(gn$est, gn$p),
    fmt_e(ic$est, ic$p),
    fmt_e(ep$est, ep$p),
    fmt_e(hf$est, hf$p),
  "",
    fmt_s(gn$se), fmt_s(ic$se), fmt_s(ep$se), fmt_s(hf$se)
)

footer_sp <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`,
  "Practice HRR FE", "Yes", "Yes", "Yes", "Yes",
  "Year FE",         "Yes", "Yes", "Yes", "Yes",
  "Observations",
    format(nobs(m_genl), big.mark = ","),
    format(nobs(m_ic),   big.mark = ","),
    format(nobs(m_ep),   big.mark = ","),
    format(nobs(m_hf),   big.mark = ",")
)

table_sp <- bind_rows(body_sp, footer_sp)
kable(table_sp,
      format    = "latex",
      booktabs  = TRUE,
      linesep   = "",
      escape    = FALSE,
      align     = c("l", rep("c", 4)),
      col.names = c("",
                    "General Cards",
                    "Interventional",
                    "EP",
                    "Adv HF")) %>%
  row_spec(2, extra_latex_after = "\\midrule") %>%
  save_kable("results/tables/aha-training-by-subspecialty.tex")

cat("\n=== Wrote results/tables/aha-training-by-subspecialty.tex ===\n")


# 9. Head-to-head: training vs current-environment imprint ---------------

# train_cath_lab is on a 0-1 share scale. intensity_dest_loo is residualized
# cath rate. Standardize both regressors so coefficients are on common
# units ("standard-deviation of cath rate per standard-deviation of the
# regressor"). Then we can directly compare relative magnitudes.

both <- clean %>%
  filter(!is.na(intensity_dest_loo), !is.nan(intensity_dest_loo)) %>%
  mutate(
    z_train_cath = (train_cath_lab    - mean(train_cath_lab,    na.rm = TRUE)) /
                    sd(train_cath_lab,                          na.rm = TRUE),
    z_dest       = (intensity_dest_loo - mean(intensity_dest_loo, na.rm = TRUE)) /
                    sd(intensity_dest_loo,                        na.rm = TRUE)
  )

m_train_only <- feols(mean_resid_cath ~ z_train_cath |
                        hrr_practice + year,
                      data = both, weights = ~n_nstemi,
                      cluster = ~hrr_med_school)
m_dest_only  <- feols(mean_resid_cath ~ z_dest |
                        hrr_practice + year,
                      data = both, weights = ~n_nstemi,
                      cluster = ~hrr_med_school)
m_both       <- feols(mean_resid_cath ~ z_train_cath + z_dest |
                        hrr_practice + year,
                      data = both, weights = ~n_nstemi,
                      cluster = ~hrr_med_school)

cat("\n=== Standardized head-to-head: training vs current environment ===\n")
cat("(Coefficients in pp residualized-cath per 1 SD of regressor.)\n")
cat("\n--- Training only ---\n");          print(summary(m_train_only))
cat("\n--- Current only ---\n");           print(summary(m_dest_only))
cat("\n--- Both together ---\n");          print(summary(m_both))

tr1  <- mc_row(m_train_only, "z_train_cath")
de1  <- mc_row(m_dest_only,  "z_dest")
trb  <- mc_row(m_both,       "z_train_cath")
deb  <- mc_row(m_both,       "z_dest")

body_h2h <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`,
  "Training-HRR cath lab share (z)",
    fmt_e(tr1$est, tr1$p),    " ",                      fmt_e(trb$est, trb$p),
  "",
    fmt_s(tr1$se),             " ",                      fmt_s(trb$se),
  "Current-HRR cath culture (z)",
    " ",                       fmt_e(de1$est, de1$p),    fmt_e(deb$est, deb$p),
  "",
    " ",                       fmt_s(de1$se),            fmt_s(deb$se)
)

footer_h2h <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`,
  "Practice HRR FE", "Yes", "Yes", "Yes",
  "Year FE",         "Yes", "Yes", "Yes",
  "Observations",
    format(nobs(m_train_only), big.mark = ","),
    format(nobs(m_dest_only),  big.mark = ","),
    format(nobs(m_both),       big.mark = ",")
)

table_h2h <- bind_rows(body_h2h, footer_h2h)
kable(table_h2h,
      format    = "latex",
      booktabs  = TRUE,
      linesep   = "",
      escape    = FALSE,
      align     = c("l", rep("c", 3)),
      col.names = c("",
                    "Training only",
                    "Current only",
                    "Both")) %>%
  row_spec(4, extra_latex_after = "\\midrule") %>%
  save_kable("results/tables/aha-head-to-head.tex")

cat("\n=== Wrote results/tables/aha-head-to-head.tex ===\n")


# 10. Tightening identification ------------------------------------------

# Three comparisons in our data:
#   (A) Same med school, different practice destinations
#   (B) Same practice destination, different med schools  (our current spec)
#   (C) Mid-career movers: same NPI, different destinations
#
# Our headline AHA spec uses (B) via destination HRR FE. We can sharpen
# the training-imprint identification by also adding origin-HRR FE,
# which restricts the comparison to cardiologists from the same training
# HRR but different graduation cohorts (so different training-period
# cath-share values, since the AHA measure is year-matched).
#
# We can also add NPI FE to identify the destination effect off
# within-physician moves (mid-career movers). NPI FE absorbs train
# cath share entirely (fixed per cardiologist), so it cannot sharpen
# the training coefficient -- it sharpens destination.

clean2 <- panel %>%
  filter(!is.na(train_cath_lab),
         !is.na(intensity_dest_loo),
         !is.nan(intensity_dest_loo),
         !is.na(mean_resid_cath),
         grad_year >= 1983, grad_year <= 2006) %>%
  mutate(years_exp = year - grad_year,
         female    = as.integer(gender == "F"))

# Physician controls are restricted to fixed cardiologist characteristics
# (gender, years since graduation). Subspecialty is excluded because it is
# itself an outcome of the training environment.

# For practice-vars sample, also require non-missing practice characteristics.
clean2_prac <- clean2 %>%
  filter(!is.na(hospital_based_share),
         !is.na(log_tin_volume))

# Panel A: Destination FE only ------------------------------------------
# (1) baseline
a1 <- feols(mean_resid_cath ~ train_cath_lab + intensity_dest_loo |
              hrr_practice + year,
            data = clean2, weights = ~n_nstemi,
            cluster = ~hrr_med_school)
# (2) + physician characteristics
a2 <- feols(mean_resid_cath ~ train_cath_lab + intensity_dest_loo +
              female + years_exp |
              hrr_practice + year,
            data = clean2, weights = ~n_nstemi,
            cluster = ~hrr_med_school)
# (3) + practice characteristics
a3 <- feols(mean_resid_cath ~ train_cath_lab + intensity_dest_loo +
              female + years_exp +
              hospital_based_share + log_tin_volume |
              hrr_practice + year,
            data = clean2_prac, weights = ~n_nstemi,
            cluster = ~hrr_med_school)

# Panel B: Origin + Destination FE --------------------------------------
b1 <- feols(mean_resid_cath ~ train_cath_lab + intensity_dest_loo |
              hrr_med_school + hrr_practice + year,
            data = clean2, weights = ~n_nstemi,
            cluster = ~hrr_med_school)
b2 <- feols(mean_resid_cath ~ train_cath_lab + intensity_dest_loo +
              female + years_exp |
              hrr_med_school + hrr_practice + year,
            data = clean2, weights = ~n_nstemi,
            cluster = ~hrr_med_school)
b3 <- feols(mean_resid_cath ~ train_cath_lab + intensity_dest_loo +
              female + years_exp +
              hospital_based_share + log_tin_volume |
              hrr_med_school + hrr_practice + year,
            data = clean2_prac, weights = ~n_nstemi,
            cluster = ~hrr_med_school)

cat("\n=== Training imprint, identification strategies ===\n")
cat("\n--- Panel A1: destination FE, baseline ---\n");          print(summary(a1))
cat("\n--- Panel A2: destination FE + physician vars ---\n");   print(summary(a2))
cat("\n--- Panel A3: destination FE + physician + practice ---\n"); print(summary(a3))
cat("\n--- Panel B1: origin + destination FE, baseline ---\n"); print(summary(b1))
cat("\n--- Panel B2: origin + destination FE + physician vars ---\n"); print(summary(b2))
cat("\n--- Panel B3: origin + destination FE + physician + practice ---\n"); print(summary(b3))


# 11. Side-by-side table -------------------------------------------------

# Build a 2-panel x 3-column table by hand. Each panel reports
# Training-HRR cath lab share and Current-HRR cath culture coefficients
# across the (baseline, + physician vars, + practice vars) progression.

panel_row <- function(models) {
  tr <- lapply(models, mc_row, term = "train_cath_lab")
  de <- lapply(models, mc_row, term = "intensity_dest_loo")
  paste0(
    "Training-HRR cath lab share & ",
    paste(sapply(tr, function(x) fmt_e(x$est, x$p)), collapse = " & "), " \\\\\n",
    " & ",
    paste(sapply(tr, function(x) fmt_s(x$se)), collapse = " & "), " \\\\\n",
    "Current-HRR cath culture (LOO) & ",
    paste(sapply(de, function(x) fmt_e(x$est, x$p)), collapse = " & "), " \\\\\n",
    " & ",
    paste(sapply(de, function(x) fmt_s(x$se)), collapse = " & "), " \\\\\n"
  )
}

obs_row <- function(models) {
  paste0("Observations & ",
         paste(sapply(models, function(m) format(nobs(m), big.mark = ",")),
               collapse = " & "),
         " \\\\\n")
}

models_a <- list(a1, a2, a3)
models_b <- list(b1, b2, b3)

tbl <- paste0(
  "\\begin{tabular}{lccc}\n",
  "\\toprule\n",
  " & Baseline & + physician vars & + practice vars \\\\\n",
  "\\midrule\n",
  "\\multicolumn{4}{l}{\\textit{Panel A. Destination FE}} \\\\\n",
  panel_row(models_a),
  obs_row(models_a),
  "\\midrule\n",
  "\\multicolumn{4}{l}{\\textit{Panel B. Origin + Destination FE}} \\\\\n",
  panel_row(models_b),
  obs_row(models_b),
  "\\bottomrule\n",
  "\\end{tabular}\n"
)

writeLines(tbl, "results/tables/training-imprint.tex")
cat("\n=== Wrote results/tables/training-imprint.tex ===\n")


# 12. Mechanism: teaching-hospital cath-lab vs all-hospital cath-lab ----

# The HRR-level cath-lab share aggregates over all hospitals in the HRR,
# but medical students rotate primarily at teaching hospitals. We
# construct the same share restricted to teaching hospitals and run it
# head-to-head against the all-hospital measure to discipline the
# clerkship-era exposure interpretation.

mech <- panel %>%
  filter(!is.na(train_cath_lab),
         !is.na(train_cath_lab_teach),
         !is.na(mean_resid_cath),
         grad_year >= 1983, grad_year <= 2006)

cat("\n=== Mechanism check sample ===\n")
cat("N rows:", nrow(mech), " N cardio:", n_distinct(mech$npi),
    " N HRRs:", n_distinct(mech$hrr_med_school), "\n")

m_teach_only <- feols(mean_resid_cath ~ train_cath_lab_teach |
                        hrr_med_school + hrr_practice + year,
                      data = mech, weights = ~n_nstemi,
                      cluster = ~hrr_med_school)
m_both_meas  <- feols(mean_resid_cath ~ train_cath_lab + train_cath_lab_teach |
                        hrr_med_school + hrr_practice + year,
                      data = mech, weights = ~n_nstemi,
                      cluster = ~hrr_med_school)
m_all_only   <- feols(mean_resid_cath ~ train_cath_lab |
                        hrr_med_school + hrr_practice + year,
                      data = mech, weights = ~n_nstemi,
                      cluster = ~hrr_med_school)

cat("\n--- All-hospital measure only (within-origin) ---\n")
print(summary(m_all_only))
cat("\n--- Teaching-hospital measure only (within-origin) ---\n")
print(summary(m_teach_only))
cat("\n--- Head-to-head (within-origin) ---\n")
print(summary(m_both_meas))

mech_all   <- mc_row(m_all_only,   "train_cath_lab")
mech_tch   <- mc_row(m_teach_only, "train_cath_lab_teach")
mech_a_h2h <- mc_row(m_both_meas,  "train_cath_lab")
mech_t_h2h <- mc_row(m_both_meas,  "train_cath_lab_teach")

body_mech <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`,
  "All-hospital cath share, training HRR",
    fmt_e(mech_all$est, mech_all$p),     " ",                         fmt_e(mech_a_h2h$est, mech_a_h2h$p),
  "",
    fmt_s(mech_all$se),                   " ",                         fmt_s(mech_a_h2h$se),
  "Teaching-hospital cath share, training HRR",
    " ",                                   fmt_e(mech_tch$est, mech_tch$p),     fmt_e(mech_t_h2h$est, mech_t_h2h$p),
  "",
    " ",                                   fmt_s(mech_tch$se),                   fmt_s(mech_t_h2h$se)
)

footer_mech <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`,
  "Med school HRR FE",  "Yes", "Yes", "Yes",
  "Practice HRR FE",    "Yes", "Yes", "Yes",
  "Year FE",            "Yes", "Yes", "Yes",
  "Observations",
    format(nobs(m_all_only),   big.mark = ","),
    format(nobs(m_teach_only), big.mark = ","),
    format(nobs(m_both_meas),  big.mark = ",")
)

table_mech <- bind_rows(body_mech, footer_mech)
kable(table_mech,
      format    = "latex",
      booktabs  = TRUE,
      linesep   = "",
      escape    = FALSE,
      align     = c("l", rep("c", 3)),
      col.names = c("",
                    "All hospitals",
                    "Teaching only",
                    "Head-to-head")) %>%
  row_spec(4, extra_latex_after = "\\midrule") %>%
  save_kable("results/tables/aha-mechanism-teaching.tex")

cat("\n=== Wrote results/tables/aha-mechanism-teaching.tex ===\n")


# 13. Cohort robustness: medical-school x decade FE -----------------------

# The within-origin specification absorbs fixed medical-school features but
# does not absorb decade-level cohort shifts. To probe whether cath-lab
# rollout timing is correlated with decade-level shifts in unobservables,
# we add medical-school x graduation-decade fixed effects. Identification
# now comes from cardiologists who attended the same medical school in the
# same decade but in different years within that decade.

cohort_data <- clean2 %>%
  mutate(grad_decade = floor(grad_year / 10) * 10L,
         school_decade = paste0(hrr_med_school, "_", grad_decade))

cat("\n=== Cohort-robustness sample ===\n")
cat("N rows:", nrow(cohort_data), " N cardio:", n_distinct(cohort_data$npi),
    " N school-decades:", n_distinct(cohort_data$school_decade), "\n")

m_orig_dec <- feols(mean_resid_cath ~ train_cath_lab + intensity_dest_loo |
                      school_decade + hrr_practice + year,
                    data = cohort_data, weights = ~n_nstemi,
                    cluster = ~hrr_med_school)
m_orig_dec_train <- feols(mean_resid_cath ~ train_cath_lab |
                            school_decade + hrr_practice + year,
                          data = cohort_data, weights = ~n_nstemi,
                          cluster = ~hrr_med_school)

cat("\n--- Med school x decade FE (training + destination) ---\n")
print(summary(m_orig_dec))
cat("\n--- Med school x decade FE (training only) ---\n")
print(summary(m_orig_dec_train))

# Side-by-side with the headline within-origin spec
hl_train  <- mc_row(b1,         "train_cath_lab")
dec_train <- mc_row(m_orig_dec,       "train_cath_lab")
dec_only  <- mc_row(m_orig_dec_train, "train_cath_lab")

body_coh <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`,
  "Training-HRR cath lab share",
    fmt_e(hl_train$est, hl_train$p),
    fmt_e(dec_only$est, dec_only$p),
    fmt_e(dec_train$est, dec_train$p),
  "",
    fmt_s(hl_train$se),
    fmt_s(dec_only$se),
    fmt_s(dec_train$se)
)

footer_coh <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`,
  "Med school HRR FE",        "Yes", "No",  "No",
  "Med school HRR x Decade FE", "No",  "Yes", "Yes",
  "Practice HRR FE",          "Yes", "Yes", "Yes",
  "Year FE",                  "Yes", "Yes", "Yes",
  "Current cath culture",     "Yes", "No",  "Yes",
  "Observations",
    format(nobs(b1),         big.mark = ","),
    format(nobs(m_orig_dec_train), big.mark = ","),
    format(nobs(m_orig_dec),       big.mark = ",")
)

table_coh <- bind_rows(body_coh, footer_coh)
kable(table_coh,
      format    = "latex",
      booktabs  = TRUE,
      linesep   = "",
      escape    = FALSE,
      align     = c("l", rep("c", 3)),
      col.names = c("",
                    "Headline",
                    "Decade FE",
                    "Decade FE + dest.")) %>%
  row_spec(2, extra_latex_after = "\\midrule") %>%
  save_kable("results/tables/aha-cohort-robust.tex")

cat("\n=== Wrote results/tables/aha-cohort-robust.tex ===\n")
