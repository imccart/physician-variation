# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-05-06 (rewrote 2026-05-07)
## Description:   Year-matched NIH-funding rank gradient. Replaces the
##                earlier hand-curated USNWR top-25 stub with year-by-year
##                NIH-funding rankings built from NIH ExPORTER bulk files
##                (1985-2025), validated against BRIMR (Spearman 0.92-0.99
##                across years).
##
##                For each cardiologist, we look up the NIH rank tier of
##                their medical school in the year their med school began
##                (grad_year - 3). This year-matches the rank to their
##                actual training period rather than using a contemporary
##                proxy.
##
##                Pipeline inputs:
##                  data/input/med-school-nih.csv  (school x fiscal_year)
##                  data/input/cardio-school-to-nih.csv  (panel name -> NIH name)


# 1. Load and join --------------------------------------------------------

analysis <- read_csv("data/output/analysis_panel.csv",
                     col_types = cols(npi = col_character(),
                                      year = col_integer(),
                                      .default = col_guess()))

nih_panel <- read_csv("data/input/med-school-nih.csv",
                      col_types = cols(med_school = col_character(),
                                       fiscal_year = col_integer(),
                                       total_funding = col_double(),
                                       n_awards = col_integer(),
                                       rank = col_integer(),
                                       rank_tier = col_character()))

cardio_xw <- read_csv("data/input/cardio-school-to-nih.csv",
                      col_types = cols(.default = col_character()))

# Join the panel's `med_school` to the NIH canonical name via the crosswalk
analysis <- analysis %>%
  left_join(cardio_xw %>% select(cardio_name, canonical_school),
            by = c("med_school" = "cardio_name"))

cat("Panel rows:", nrow(analysis), "\n")
cat("Rows with canonical_school matched:",
    sum(!is.na(analysis$canonical_school)), "\n")

# Year-match: training period start = grad_year - 3 (start of med school)
# Clamp to NIH coverage [1985, 2025].
analysis <- analysis %>%
  mutate(med_school_start = grad_year - 3L,
         nih_match_year = pmin(pmax(med_school_start, 1985L), 2025L))

# Bring training-year rank tier onto each cardiologist
ranked <- analysis %>%
  left_join(nih_panel %>%
              select(canonical_school = med_school,
                     nih_match_year = fiscal_year,
                     nih_rank = rank,
                     nih_tier = rank_tier),
            by = c("canonical_school", "nih_match_year"))

cat("\nNIH rank coverage among matched cardiologists:\n")
ranked %>%
  filter(!is.na(canonical_school)) %>%
  summarize(matched_to_nih = sum(!is.na(nih_rank)),
            unmatched      = sum(is.na(nih_rank))) %>%
  print()

# Translate NIH tier into our analysis tiers
ranked <- ranked %>%
  mutate(
    nih_top10  = as.integer(nih_tier == "01_top10"),
    nih_top25  = as.integer(nih_tier %in% c("01_top10", "02_top11_25")),
    nih_top50  = as.integer(nih_tier %in% c("01_top10", "02_top11_25", "03_top26_50")),
    nih_top100 = as.integer(nih_tier %in% c("01_top10", "02_top11_25",
                                            "03_top26_50", "04_top51_100"))
  )

cat("\nDistribution of NIH tier among matched cardiologist-years:\n")
print(ranked %>% filter(!is.na(nih_tier)) %>% count(nih_tier))


# 2. Join training-period AHA cath lab share onto the panel ---------------

# Replicates the AHA matching from 8_aha_training.R so that the rank table
# can include a horse race against the training cath lab share.
aha_hosp <- read_csv("data/input/aha_hospital.csv", show_col_types = FALSE,
                     col_types = cols(HRRCODE = col_integer(),
                                      year = col_integer(),
                                      CCLABHOS = col_character(),
                                      .default = col_guess()))
aha_hrr <- aha_hosp %>%
  filter(!is.na(HRRCODE), !is.na(year)) %>%
  mutate(has_cath_lab = as.integer(CCLABHOS == "1")) %>%
  group_by(HRRCODE, year) %>%
  summarize(cath_lab_share = mean(has_cath_lab, na.rm = TRUE),
            .groups = "drop") %>%
  rename(hrr = HRRCODE) %>%
  mutate(cath_lab_share = if_else(is.nan(cath_lab_share),
                                  NA_real_, cath_lab_share))

phys_train <- ranked %>%
  filter(!is.na(grad_year), !is.na(hrr_med_school)) %>%
  distinct(npi, hrr_med_school, grad_year) %>%
  mutate(aha_match_year = pmin(pmax(grad_year - 3L, 1980L), 2003L)) %>%
  left_join(aha_hrr, by = c("hrr_med_school" = "hrr",
                            "aha_match_year"  = "year")) %>%
  select(npi, hrr_med_school, train_cath_lab = cath_lab_share)

ranked <- ranked %>% left_join(phys_train, by = c("npi", "hrr_med_school"))


# 3. Main rank-gradient regressions ---------------------------------------

# Drop "OTHER" (non-US / unknown) schools and rows with no NIH match.
# Set the reference category for tier indicators to "05_unranked" (below
# top 100) so coefficients are interpreted as the additional cath rate for
# each tier relative to unranked schools.
reg_data <- ranked %>%
  filter(!is.na(med_school), med_school != "OTHER",
         !is.na(nih_tier),
         !is.na(mean_resid_cath),
         grad_year >= 1983, grad_year <= 2006) %>%
  mutate(nih_tier = factor(nih_tier,
                           levels = c("05_unranked", "04_top51_100",
                                      "03_top26_50", "02_top11_25",
                                      "01_top10")),
         years_exp = year - grad_year,
         female    = as.integer(gender == "F"))

reg_data_cath <- reg_data %>% filter(!is.na(train_cath_lab))
reg_data_full <- reg_data_cath %>%
  filter(!is.na(hospital_based_share), !is.na(log_tin_volume))

cat("\nRegression samples:\n")
cat("  base:        ", nrow(reg_data),      "rows\n")
cat("  + cath:      ", nrow(reg_data_cath), "rows\n")
cat("  + full ctrl: ", nrow(reg_data_full), "rows\n\n")

# All specs use origin + destination FE (matching Table 3 Panel B preferred
# specification). Rank is identified within medical-school HRR across schools
# and cohorts; training cath lab share is identified within HRR across
# graduation cohorts.

# Panel A: -log(rank) ----------------------------------------------------
a1 <- feols(mean_resid_cath ~ I(-log(nih_rank)) |
              hrr_med_school + hrr_practice + year,
            data = reg_data, weights = ~n_nstemi,
            cluster = ~hrr_med_school)
a2 <- feols(mean_resid_cath ~ I(-log(nih_rank)) + train_cath_lab |
              hrr_med_school + hrr_practice + year,
            data = reg_data_cath, weights = ~n_nstemi,
            cluster = ~hrr_med_school)
a3 <- feols(mean_resid_cath ~ I(-log(nih_rank)) + train_cath_lab +
              female + years_exp + hospital_based_share + log_tin_volume |
              hrr_med_school + hrr_practice + year,
            data = reg_data_full, weights = ~n_nstemi,
            cluster = ~hrr_med_school)

# Panel B: Tier indicators -----------------------------------------------
b1 <- feols(mean_resid_cath ~ nih_tier |
              hrr_med_school + hrr_practice + year,
            data = reg_data, weights = ~n_nstemi,
            cluster = ~hrr_med_school)
b2 <- feols(mean_resid_cath ~ nih_tier + train_cath_lab |
              hrr_med_school + hrr_practice + year,
            data = reg_data_cath, weights = ~n_nstemi,
            cluster = ~hrr_med_school)
b3 <- feols(mean_resid_cath ~ nih_tier + train_cath_lab +
              female + years_exp + hospital_based_share + log_tin_volume |
              hrr_med_school + hrr_practice + year,
            data = reg_data_full, weights = ~n_nstemi,
            cluster = ~hrr_med_school)

cat("=== Panel A1: -log(rank) only ===\n");                       print(summary(a1))
cat("\n=== Panel A2: + cath lab ===\n");                          print(summary(a2))
cat("\n=== Panel A3: + cath lab + physician + practice ===\n");   print(summary(a3))
cat("\n=== Panel B1: tier indicators only ===\n");                print(summary(b1))
cat("\n=== Panel B2: + cath lab ===\n");                          print(summary(b2))
cat("\n=== Panel B3: + cath lab + physician + practice ===\n");   print(summary(b3))


# 4. Export 2-panel x 3-col table ----------------------------------------

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

# Panel A coefficients
a_lr  <- list(mc_row(a1, "I(-log(nih_rank))"),
              mc_row(a2, "I(-log(nih_rank))"),
              mc_row(a3, "I(-log(nih_rank))"))
a_cl  <- list(mc_row(a1, "train_cath_lab"),
              mc_row(a2, "train_cath_lab"),
              mc_row(a3, "train_cath_lab"))

# Panel B coefficients (one row per tier)
tier_terms <- c("nih_tier04_top51_100", "nih_tier03_top26_50",
                "nih_tier02_top11_25",  "nih_tier01_top10")
tier_labels <- c("Top 51-100", "Top 26-50", "Top 11-25", "Top 10")
b_tiers <- lapply(tier_terms, function(tt) {
  list(mc_row(b1, tt), mc_row(b2, tt), mc_row(b3, tt))
})
b_cl <- list(mc_row(b1, "train_cath_lab"),
             mc_row(b2, "train_cath_lab"),
             mc_row(b3, "train_cath_lab"))

# Row builders
row_3col <- function(label, rs) {
  paste0(label, " & ",
         paste(sapply(rs, function(x) fmt_e(x$est, x$p)), collapse = " & "),
         " \\\\\n",
         " & ",
         paste(sapply(rs, function(x) fmt_s(x$se)), collapse = " & "),
         " \\\\\n")
}

panel_a <- paste0(
  row_3col("$-\\log(\\text{NIH rank})$", a_lr),
  row_3col("Training cath lab share",    a_cl)
)

panel_b <- paste0(
  paste(mapply(row_3col, tier_labels, b_tiers,
               SIMPLIFY = TRUE), collapse = ""),
  row_3col("Training cath lab share", b_cl)
)

obs_row <- function(models) {
  paste0("Observations & ",
         paste(sapply(models, function(m) format(nobs(m), big.mark = ",")),
               collapse = " & "),
         " \\\\\n")
}

bottom_section <- paste0(
  "Physician characteristics & No & No & Yes \\\\\n",
  "Practice characteristics & No & No & Yes \\\\\n",
  "Med school HRR FE & Yes & Yes & Yes \\\\\n",
  "Practice HRR FE & Yes & Yes & Yes \\\\\n",
  "Year FE & Yes & Yes & Yes \\\\\n"
)

tbl <- paste0(
  "\\begin{tabular}{lccc}\n",
  "\\toprule\n",
  " & (1) & (2) & (3) \\\\\n",
  "\\midrule\n",
  "\\multicolumn{4}{l}{\\textit{Panel A. $-\\log$(NIH rank)}} \\\\\n",
  panel_a,
  "\\midrule\n",
  "\\multicolumn{4}{l}{\\textit{Panel B. NIH rank tier indicators (ref.\\ = unranked)}} \\\\\n",
  panel_b,
  "\\midrule\n",
  bottom_section,
  obs_row(list(a1, a2, a3)),
  "\\bottomrule\n",
  "\\end{tabular}\n"
)

writeLines(tbl, "results/tables/rank.tex")
cat("\n=== Wrote results/tables/rank.tex ===\n")
