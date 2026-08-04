# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-08-04
## Description:   Permutation (randomization-inference) test on the within-
##                origin training imprint. Reassigns each cardiologist the
##                training-period cath lab availability that a DIFFERENT
##                graduation cohort from the SAME medical school HRR actually
##                faced, holding all else fixed, and re-estimates the imprint
##                coefficient 1,000 times. If the design is informative, the
##                true cohort-to-exposure assignment should beat the reshuffle.
##
##                Precondition (reported below): the reshuffle only has
##                leverage if training exposure varies within HRR across
##                cohorts. We decompose the variance to confirm this.
##
##                Reconstructs train_cath_lab from source rather than
##                depending on 7_aha_training.R's in-memory objects.
##
##                Outputs:
##                  results/figures/perm-null.png
##                  results/permutation-summary.csv

set.seed(20260804)

# 1. Build train_cath_lab (same construction as 7_aha_training.R) ----------

analysis <- read_csv("data/output/analysis_panel.csv",
                     col_types = cols(npi = col_character(),
                                      year = col_integer(),
                                      .default = col_guess()))

aha_hosp <- read_csv("data/input/aha_hospital.csv", show_col_types = FALSE,
                     col_types = cols(HRRCODE = col_integer(), year = col_integer(),
                                      CCLABHOS = col_character(),
                                      .default = col_guess()))

aha_hrr <- aha_hosp %>%
  filter(!is.na(HRRCODE), !is.na(year)) %>%
  mutate(has_cath_lab = as.integer(CCLABHOS == "1")) %>%
  group_by(HRRCODE, year) %>%
  summarize(cath_lab_share = mean(has_cath_lab, na.rm = TRUE), .groups = "drop") %>%
  rename(hrr = HRRCODE) %>%
  mutate(cath_lab_share = if_else(is.nan(cath_lab_share), NA_real_, cath_lab_share))

phys_train <- analysis %>%
  filter(!is.na(grad_year), !is.na(hrr_med_school)) %>%
  distinct(npi, hrr_med_school, grad_year) %>%
  mutate(med_school_start = grad_year - 3,
         aha_match_year   = pmin(pmax(med_school_start, 1980L), 2003L))

training_intensity <- phys_train %>%
  left_join(aha_hrr %>% select(hrr, year, cath_lab_share),
            by = c("hrr_med_school" = "hrr", "aha_match_year" = "year"))

panel <- analysis %>%
  left_join(training_intensity %>%
              select(npi, hrr_med_school, train_cath_lab = cath_lab_share),
            by = c("npi", "hrr_med_school"))

# Within-origin estimation sample. Matches the canonical baseline in
# 5_selection.R (m_aha_base, cohort-robust.tex col 1): grad 1983-2006,
# non-missing training exposure, outcome, and destination peer measure, plus
# non-missing gender/specialty (the IPW join restricts to these). This yields
# the headline within-origin coefficient of 0.058 on N = 10,729.
clean <- panel %>%
  filter(!is.na(train_cath_lab), !is.na(mean_resid_cath),
         !is.na(intensity_dest_loo), !is.nan(intensity_dest_loo),
         !is.na(gender), !is.na(specialty),
         grad_year >= 1983, grad_year <= 2006)

cat("\n=== Permutation test: within-origin estimation sample ===\n")
cat("rows (cardiologist-years):", nrow(clean), "\n")
cat("cardiologists:            ", n_distinct(clean$npi), "\n")
cat("med-school HRRs:          ", n_distinct(clean$hrr_med_school), "\n")


# 2. Precondition: within- vs between-HRR variation in exposure ------------

phys <- clean %>%
  distinct(npi, hrr_med_school, train_cath_lab) %>%
  group_by(hrr_med_school) %>%
  mutate(hrr_mean = mean(train_cath_lab)) %>%
  ungroup()

v_within  <- mean((phys$train_cath_lab - phys$hrr_mean)^2)
v_between <- mean((phys$hrr_mean - mean(phys$train_cath_lab))^2)
within_share <- v_within / (v_within + v_between)

cat("\n=== Exposure variance decomposition (physician level) ===\n")
cat("within-HRR SD:  ", sprintf("%.4f", sqrt(v_within)), "\n")
cat("between-HRR SD: ", sprintf("%.4f", sqrt(v_between)), "\n")
cat("within share:   ", sprintf("%.3f", within_share), "\n")


# 3. True within-origin coefficient ---------------------------------------

m_true <- feols(mean_resid_cath ~ train_cath_lab |
                  hrr_med_school + hrr_practice + year,
                data = clean, weights = ~n_nstemi, cluster = ~hrr_med_school)
beta_true <- unname(coef(m_true)["train_cath_lab"])
cat("\n=== True within-origin beta_train ===\n")
print(summary(m_true))


# 4. Permutation loop -----------------------------------------------------

phys_key <- clean %>% distinct(npi, hrr_med_school, train_cath_lab)

n_perm <- 1000
betas  <- numeric(n_perm)

for (b in seq_len(n_perm)) {
  shuffled <- phys_key %>%
    group_by(hrr_med_school) %>%
    mutate(train_perm = sample(train_cath_lab)) %>%
    ungroup() %>%
    select(npi, train_perm)

  d <- clean %>% left_join(shuffled, by = "npi")
  m <- feols(mean_resid_cath ~ train_perm |
               hrr_med_school + hrr_practice + year,
             data = d, weights = ~n_nstemi)
  betas[b] <- unname(coef(m)["train_perm"])
}

p_two   <- mean(abs(betas) >= abs(beta_true))
p_right <- mean(betas >= beta_true)
band_lo <- unname(quantile(betas, 0.025))
band_hi <- unname(quantile(betas, 0.975))

cat("\n=== Permutation results (", n_perm, " draws) ===\n", sep = "")
cat("true beta_train:      ", sprintf("%.4f", beta_true), "\n")
cat("placebo mean:         ", sprintf("%.4f", mean(betas)), "\n")
cat("placebo SD:           ", sprintf("%.4f", sd(betas)), "\n")
cat("placebo 2.5/97.5 pct: ", sprintf("%.4f / %.4f", band_lo, band_hi), "\n")
cat("RI p (two-sided):     ", sprintf("%.4f", p_two), "\n")
cat("RI p (right tail):    ", sprintf("%.4f", p_right), "\n")


# 5. Outputs --------------------------------------------------------------

summary_out <- tibble(
  statistic = c("beta_true", "placebo_mean", "placebo_sd",
                "placebo_p025", "placebo_p975",
                "ri_p_two_sided", "ri_p_right_tail",
                "within_hrr_sd", "between_hrr_sd", "within_share",
                "n_perm", "n_obs", "n_cardio", "n_hrr"),
  value = c(beta_true, mean(betas), sd(betas),
            band_lo, band_hi,
            p_two, p_right,
            sqrt(v_within), sqrt(v_between), within_share,
            n_perm, nrow(clean), n_distinct(clean$npi),
            n_distinct(clean$hrr_med_school))
)
write_csv(summary_out, "results/permutation-summary.csv")

perm_df <- tibble(beta = betas)

p <- ggplot(perm_df, aes(x = beta)) +
  geom_histogram(bins = 40, fill = "grey70", color = "white", boundary = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = beta_true, color = "firebrick", linewidth = 1) +
  annotate("text", x = beta_true, y = Inf,
           label = sprintf("Actual estimate = %.3f", beta_true),
           hjust = 1.05, vjust = 1.8, color = "firebrick", size = 4) +
  labs(x = "Training coefficient under permuted cohort assignment",
       y = "Count") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())

ggsave("results/figures/perm-null.png", p, width = 6.5, height = 4, dpi = 300)

cat("\nWrote results/figures/perm-null.png and results/permutation-summary.csv\n")
