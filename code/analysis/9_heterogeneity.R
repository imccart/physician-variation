# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-05-07
## Description:   Heterogeneity in training imprint by current peer
##                environment. Within-physician test: for cardiologists
##                we observe at multiple destinations, does the training-
##                cath share interact with the current-destination peer
##                cath rate? NPI FE absorbs main effects; the interaction
##                tells us whether training and destination are complements,
##                substitutes, or independent.
##
##                Spec:
##                  y_it = alpha_i + gamma_t
##                       + delta * (tau_i * intensity_dest_loo_it)
##                       + epsilon_it
##
##                  delta > 0: complements (training + destination compound)
##                  delta < 0: training persists despite countervailing
##                            destination (substitutes)
##                  delta = 0: training independent of destination


# 1. Build merged panel -------------------------------------------------

analysis  <- read_csv("data/output/analysis_panel.csv",
                      col_types = cols(npi = col_character(),
                                       year = col_integer(),
                                       .default = col_guess()))
aha_hosp  <- read_csv("data/input/aha_hospital.csv", show_col_types = FALSE,
                      col_types = cols(HRRCODE = col_integer(),
                                       year = col_integer(),
                                       CCLABHOS = col_character(),
                                       .default = col_guess()))
cardio_xw <- read_csv("data/input/cardio-school-to-nih.csv",
                      show_col_types = FALSE,
                      col_types = cols(.default = col_character()))

aha_hrr <- aha_hosp %>%
  filter(!is.na(HRRCODE), !is.na(year)) %>%
  mutate(has_cath = as.integer(CCLABHOS == "1")) %>%
  group_by(HRRCODE, year) %>%
  summarize(cath_lab_share = mean(has_cath, na.rm = TRUE), .groups = "drop") %>%
  rename(hrr = HRRCODE) %>%
  mutate(cath_lab_share = if_else(is.nan(cath_lab_share), NA_real_,
                                  cath_lab_share))

panel <- analysis %>%
  left_join(cardio_xw %>% select(cardio_name, canonical_school),
            by = c("med_school" = "cardio_name")) %>%
  mutate(med_school_start = grad_year - 3L,
         aha_match_year   = pmin(pmax(med_school_start, 1980L), 2003L)) %>%
  left_join(aha_hrr %>% rename(train_cath_lab = cath_lab_share),
            by = c("hrr_med_school" = "hrr",
                   "aha_match_year"  = "year"))

clean <- panel %>%
  filter(!is.na(train_cath_lab),
         !is.na(intensity_dest_loo),
         !is.nan(intensity_dest_loo),
         !is.na(mean_resid_cath),
         grad_year >= 1983, grad_year <= 2006)

cat("Heterogeneity sample:\n")
cat("  rows:        ", nrow(clean), "\n")
cat("  cardiologists:", n_distinct(clean$npi), "\n\n")


# 2. Within-physician interaction (NPI FE) ------------------------------

# Main interaction spec. NPI FE absorbs the fixed training cath share
# (one per physician) and any other time-invariant physician traits.
# The interaction term picks up how the training-imprint manifestation
# varies with the time-varying current destination intensity.
m_inter <- feols(mean_resid_cath ~ intensity_dest_loo : train_cath_lab |
                   npi + year,
                 data = clean, weights = ~n_nstemi,
                 cluster = ~npi)

# Also include intensity_dest_loo main effect for context
m_inter2 <- feols(mean_resid_cath ~ intensity_dest_loo +
                                    intensity_dest_loo:train_cath_lab |
                    npi + year,
                  data = clean, weights = ~n_nstemi,
                  cluster = ~npi)

cat("=== Within-physician imprint x destination interaction ===\n")
cat("\n--- Spec A: interaction only ---\n")
print(summary(m_inter))
cat("\n--- Spec B: dest main effect + interaction ---\n")
print(summary(m_inter2))


# 3. Restricted to mid-career movers ------------------------------------

# Movers are the cleanest within-physician variation in current
# destination. Restrict to cardiologists with >=2 distinct hrr_practice
# values in panel.
movers_npi <- clean %>%
  group_by(npi) %>%
  filter(n_distinct(hrr_practice) >= 2) %>%
  ungroup()

cat("\n=== Mover subsample (>=2 distinct practice HRRs) ===\n")
cat("  rows:        ", nrow(movers_npi), "\n")
cat("  cardiologists:", n_distinct(movers_npi$npi), "\n")

m_inter_mov <- feols(mean_resid_cath ~ intensity_dest_loo +
                                       intensity_dest_loo:train_cath_lab |
                       npi + year,
                     data = movers_npi, weights = ~n_nstemi,
                     cluster = ~npi)
cat("\n--- Movers-only: dest + interaction ---\n")
print(summary(m_inter_mov))


# 4. Stratified by training cath share -- visualization aid ------------

# Bin training cath share into terciles. Within each tercile, recover
# the slope on intensity_dest_loo (NPI + year FE, within tercile sample).
tert <- clean %>%
  mutate(train_tercile = ntile(train_cath_lab, 3))

m_low  <- feols(mean_resid_cath ~ intensity_dest_loo | npi + year,
                data = tert %>% filter(train_tercile == 1),
                weights = ~n_nstemi, cluster = ~npi)
m_mid  <- feols(mean_resid_cath ~ intensity_dest_loo | npi + year,
                data = tert %>% filter(train_tercile == 2),
                weights = ~n_nstemi, cluster = ~npi)
m_high <- feols(mean_resid_cath ~ intensity_dest_loo | npi + year,
                data = tert %>% filter(train_tercile == 3),
                weights = ~n_nstemi, cluster = ~npi)

cat("\n=== Destination response by training tercile ===\n")
cat("\n--- Low training cath share ---\n");  print(summary(m_low))
cat("\n--- Mid training cath share ---\n");  print(summary(m_mid))
cat("\n--- High training cath share ---\n"); print(summary(m_high))


# 5. Build a side-by-side table -----------------------------------------

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

main_full <- mc_row(m_inter2, "intensity_dest_loo")
int_full  <- mc_row(m_inter2, "intensity_dest_loo:train_cath_lab")
main_mov  <- mc_row(m_inter_mov, "intensity_dest_loo")
int_mov   <- mc_row(m_inter_mov, "intensity_dest_loo:train_cath_lab")
low_b     <- mc_row(m_low,  "intensity_dest_loo")
mid_b     <- mc_row(m_mid,  "intensity_dest_loo")
high_b    <- mc_row(m_high, "intensity_dest_loo")

body_het <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`, ~`(5)`,
  "Destination intensity",
    fmt_e(main_full$est, main_full$p),
    fmt_e(main_mov$est,  main_mov$p),
    fmt_e(low_b$est,     low_b$p),
    fmt_e(mid_b$est,     mid_b$p),
    fmt_e(high_b$est,    high_b$p),
  "",
    fmt_s(main_full$se),
    fmt_s(main_mov$se),
    fmt_s(low_b$se),
    fmt_s(mid_b$se),
    fmt_s(high_b$se),
  "$\\times$ Training cath share",
    fmt_e(int_full$est, int_full$p),
    fmt_e(int_mov$est,  int_mov$p),
    " ", " ", " ",
  "",
    fmt_s(int_full$se),
    fmt_s(int_mov$se),
    " ", " ", " "
)

footer_het <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`, ~`(5)`,
  "Physician FE", "Yes", "Yes", "Yes", "Yes", "Yes",
  "Year FE",      "Yes", "Yes", "Yes", "Yes", "Yes",
  "Sample",       "Full", "Movers",
                  "Low train", "Mid train", "High train",
  "Observations",
    format(nobs(m_inter2), big.mark = ","),
    format(nobs(m_inter_mov), big.mark = ","),
    format(nobs(m_low),  big.mark = ","),
    format(nobs(m_mid),  big.mark = ","),
    format(nobs(m_high), big.mark = ",")
)

table_het <- bind_rows(body_het, footer_het)

kable(table_het,
      format    = "latex",
      booktabs  = TRUE,
      linesep   = "",
      escape    = FALSE,
      align     = c("l", rep("c", 5)),
      col.names = c("",
                    "Full",
                    "Movers",
                    "Low",
                    "Mid",
                    "High")) %>%
  row_spec(4, extra_latex_after = "\\midrule") %>%
  save_kable("results/tables/heterogeneity-train-x-dest.tex")

cat("\n=== Wrote results/tables/heterogeneity-train-x-dest.tex ===\n")
