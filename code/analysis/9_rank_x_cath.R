# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-05-07
## Description:   Rank x training-cath cross-tab. Decomposes the imprint
##                into prestige (NIH rank) and supply-side capacity
##                (AHA cath-lab share) dimensions.
##
##                Two questions:
##                  - Does cath-lab exposure during training have bite
##                    even at top-NIH-rank schools? (i.e., the supply-side
##                    channel survives controlling for prestige)
##                  - Conversely, does NIH rank predict cath rate among
##                    cardiologists from cath-rich training environments?
##                    (i.e., the prestige channel survives controlling
##                    for procedural capacity)


# 1. Load and merge -------------------------------------------------------

analysis <- read_csv("data/output/analysis_panel.csv",
                     col_types = cols(npi = col_character(),
                                      year = col_integer(),
                                      .default = col_guess()))

aha_hosp  <- read_csv("data/input/aha_hospital.csv", show_col_types = FALSE,
                      col_types = cols(HRRCODE = col_integer(),
                                       year = col_integer(),
                                       CCLABHOS = col_character(),
                                       .default = col_guess()))
nih_panel <- read_csv("data/input/med-school-nih.csv", show_col_types = FALSE,
                      col_types = cols(med_school = col_character(),
                                       fiscal_year = col_integer(),
                                       .default = col_guess()))
cardio_xw <- read_csv("data/input/cardio-school-to-nih.csv",
                      show_col_types = FALSE,
                      col_types = cols(.default = col_character()))

# AHA HRR-year cath lab share (replicates the build inside 8_aha_training.R)
aha_hrr <- aha_hosp %>%
  filter(!is.na(HRRCODE), !is.na(year)) %>%
  mutate(has_cath = as.integer(CCLABHOS == "1")) %>%
  group_by(HRRCODE, year) %>%
  summarize(cath_lab_share = mean(has_cath, na.rm = TRUE), .groups = "drop") %>%
  rename(hrr = HRRCODE) %>%
  mutate(cath_lab_share = if_else(is.nan(cath_lab_share), NA_real_,
                                  cath_lab_share))


# 2. Year-match training-period cath share + rank to each cardiologist ----

panel <- analysis %>%
  left_join(cardio_xw %>% select(cardio_name, canonical_school),
            by = c("med_school" = "cardio_name")) %>%
  mutate(med_school_start = grad_year - 3L,
         aha_match_year = pmin(pmax(med_school_start, 1980L), 2003L),
         nih_match_year = pmin(pmax(med_school_start, 1985L), 2025L)) %>%
  left_join(aha_hrr %>% rename(train_cath_lab = cath_lab_share),
            by = c("hrr_med_school" = "hrr",
                   "aha_match_year"  = "year")) %>%
  left_join(nih_panel %>% select(canonical_school = med_school,
                                 nih_match_year   = fiscal_year,
                                 nih_rank         = rank,
                                 nih_tier         = rank_tier),
            by = c("canonical_school", "nih_match_year"))

# Restrict to cardiologists with both AHA + NIH matches and clean grad-year
clean <- panel %>%
  filter(!is.na(train_cath_lab), !is.na(nih_tier),
         !is.na(mean_resid_cath),
         grad_year >= 1983, grad_year <= 2006)

cat("Sample with both training-cath and NIH-rank:\n")
cat("  rows:        ", nrow(clean), "\n")
cat("  cardiologists:", n_distinct(clean$npi), "\n")

# Tier 2x2: high vs low rank, high vs low cath
clean <- clean %>%
  mutate(
    rank_top25 = as.integer(nih_tier %in% c("01_top10", "02_top11_25")),
    cath_high  = as.integer(train_cath_lab >=
                            median(train_cath_lab, na.rm = TRUE)),
    rank_label = if_else(rank_top25 == 1, "Top 25", "Below Top 25"),
    cath_label = if_else(cath_high == 1, "High cath", "Low cath"),
    cell       = paste(rank_label, cath_label, sep = " / ")
  )

cat("\nCell sizes:\n")
print(clean %>% count(rank_label, cath_label))


# 3. Mean residualized cath rate by cell ----------------------------------

cell_means <- clean %>%
  group_by(rank_label, cath_label) %>%
  summarize(n_obs       = n(),
            n_cardio    = n_distinct(npi),
            mean_cath   = weighted.mean(mean_resid_cath, n_nstemi, na.rm = TRUE),
            .groups = "drop")

cat("\nWeighted mean residualized cath rate by cell:\n")
print(cell_means)


# 4. Joint regression with both regressors --------------------------------

# Are training-cath and rank independent dimensions, or correlated?
cat("\nCorrelation of train_cath_lab with NIH rank-top25 dummy:\n")
print(cor(clean$train_cath_lab, clean$rank_top25))

# Joint model: do both matter?
m_joint <- feols(mean_resid_cath ~ train_cath_lab + rank_top25 |
                   hrr_practice + year,
                 data = clean, weights = ~n_nstemi,
                 cluster = ~hrr_med_school)
cat("\n=== Joint: training cath + Top-25 ===\n")
print(summary(m_joint))

# Interaction model: does rank moderate the cath effect?
m_inter <- feols(mean_resid_cath ~ train_cath_lab * rank_top25 |
                   hrr_practice + year,
                 data = clean, weights = ~n_nstemi,
                 cluster = ~hrr_med_school)
cat("\n=== Interaction: training cath x Top-25 ===\n")
print(summary(m_inter))


# 5. Stratified cath effect: within each rank tier ----------------------

# Add the same physician + practice controls used in Tables 3 and 4 so the
# stratified results progress through the same set of specifications.
clean_strat <- clean %>%
  mutate(years_exp = year - grad_year,
         female    = as.integer(gender == "F"))
clean_strat_prac <- clean_strat %>%
  filter(!is.na(hospital_based_share), !is.na(log_tin_volume))

# Top 25 stratum -------------------------------------------------------
m_top25_a1 <- feols(mean_resid_cath ~ train_cath_lab |
                      hrr_practice + year,
                    data = clean_strat %>% filter(rank_top25 == 1),
                    weights = ~n_nstemi, cluster = ~hrr_med_school)
m_top25_a2 <- feols(mean_resid_cath ~ train_cath_lab + female + years_exp |
                      hrr_practice + year,
                    data = clean_strat %>% filter(rank_top25 == 1),
                    weights = ~n_nstemi, cluster = ~hrr_med_school)
m_top25_a3 <- feols(mean_resid_cath ~ train_cath_lab + female + years_exp +
                      hospital_based_share + log_tin_volume |
                      hrr_practice + year,
                    data = clean_strat_prac %>% filter(rank_top25 == 1),
                    weights = ~n_nstemi, cluster = ~hrr_med_school)

# Below Top 25 stratum --------------------------------------------------
m_below_a1 <- feols(mean_resid_cath ~ train_cath_lab |
                      hrr_practice + year,
                    data = clean_strat %>% filter(rank_top25 == 0),
                    weights = ~n_nstemi, cluster = ~hrr_med_school)
m_below_a2 <- feols(mean_resid_cath ~ train_cath_lab + female + years_exp |
                      hrr_practice + year,
                    data = clean_strat %>% filter(rank_top25 == 0),
                    weights = ~n_nstemi, cluster = ~hrr_med_school)
m_below_a3 <- feols(mean_resid_cath ~ train_cath_lab + female + years_exp +
                      hospital_based_share + log_tin_volume |
                      hrr_practice + year,
                    data = clean_strat_prac %>% filter(rank_top25 == 0),
                    weights = ~n_nstemi, cluster = ~hrr_med_school)

# Aliases for backward compatibility (later cell table uses m_top25/m_below)
m_top25 <- m_top25_a1
m_below <- m_below_a1

cat("\n=== Cath-lab effect among Top-25 schools (baseline) ===\n");      print(summary(m_top25_a1))
cat("\n=== Cath-lab effect among Top-25 schools (+ phys) ===\n");        print(summary(m_top25_a2))
cat("\n=== Cath-lab effect among Top-25 schools (+ practice) ===\n");    print(summary(m_top25_a3))
cat("\n=== Cath-lab effect among Below Top-25 (baseline) ===\n");        print(summary(m_below_a1))
cat("\n=== Cath-lab effect among Below Top-25 (+ phys) ===\n");          print(summary(m_below_a2))
cat("\n=== Cath-lab effect among Below Top-25 (+ practice) ===\n");      print(summary(m_below_a3))


# 6. 2x2 cell-effects table -----------------------------------------------

# Run a regression with the cell as a 4-level factor; interpret cell means
# (relative to the "Below Top 25 / Low cath" reference).
clean <- clean %>%
  mutate(cell = factor(cell,
                       levels = c("Below Top 25 / Low cath",
                                  "Below Top 25 / High cath",
                                  "Top 25 / Low cath",
                                  "Top 25 / High cath")))
m_cells <- feols(mean_resid_cath ~ cell | hrr_practice + year,
                 data = clean, weights = ~n_nstemi,
                 cluster = ~hrr_med_school)
cat("\n=== Cell effects (ref = Below Top 25 / Low cath) ===\n")
print(summary(m_cells))


# 7. Save tables ----------------------------------------------------------

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

# Stratified cath-effect table: 2 panels x 3 cols (baseline / + phys / + practice)
top_rows   <- list(mc_row(m_top25_a1, "train_cath_lab"),
                   mc_row(m_top25_a2, "train_cath_lab"),
                   mc_row(m_top25_a3, "train_cath_lab"))
below_rows <- list(mc_row(m_below_a1, "train_cath_lab"),
                   mc_row(m_below_a2, "train_cath_lab"),
                   mc_row(m_below_a3, "train_cath_lab"))

row_3col <- function(label, rs) {
  paste0(label, " & ",
         paste(sapply(rs, function(x) fmt_e(x$est, x$p)), collapse = " & "),
         " \\\\\n",
         " & ",
         paste(sapply(rs, function(x) fmt_s(x$se)), collapse = " & "),
         " \\\\\n")
}
obs_row <- function(models) {
  paste0("Observations & ",
         paste(sapply(models, function(m) format(nobs(m), big.mark = ",")),
               collapse = " & "),
         " \\\\\n")
}

bottom_section <- paste0(
  "Physician characteristics & No & Yes & Yes \\\\\n",
  "Practice characteristics & No & No & Yes \\\\\n",
  "Practice HRR FE & Yes & Yes & Yes \\\\\n",
  "Year FE & Yes & Yes & Yes \\\\\n"
)

tbl_strat <- paste0(
  "\\begin{tabular}{lccc}\n",
  "\\toprule\n",
  " & (1) & (2) & (3) \\\\\n",
  "\\midrule\n",
  "\\multicolumn{4}{l}{\\textit{Panel A. Top 25 schools}} \\\\\n",
  row_3col("Cath lab share, training HRR", top_rows),
  "\\midrule\n",
  "\\multicolumn{4}{l}{\\textit{Panel B. Below Top 25 schools}} \\\\\n",
  row_3col("Cath lab share, training HRR", below_rows),
  "\\midrule\n",
  bottom_section,
  obs_row(list(m_below_a1, m_below_a2, m_below_a3)),
  "\\bottomrule\n",
  "\\end{tabular}\n"
)
writeLines(tbl_strat, "results/tables/rank-x-cath-stratified.tex")

# Cell-effects table (relative to ref cell)
c2 <- mc_row(m_cells, "cellBelow Top 25 / High cath")
c3 <- mc_row(m_cells, "cellTop 25 / Low cath")
c4 <- mc_row(m_cells, "cellTop 25 / High cath")
body_cells <- tribble(
  ~term, ~est, ~se,
  "Below Top 25 / High cath",  fmt_e(c2$est, c2$p), fmt_s(c2$se),
  "Top 25 / Low cath",         fmt_e(c3$est, c3$p), fmt_s(c3$se),
  "Top 25 / High cath",        fmt_e(c4$est, c4$p), fmt_s(c4$se)
)
footer_cells <- tribble(
  ~term, ~est, ~se,
  "Practice HRR FE", "Yes", "",
  "Year FE",         "Yes", "",
  "Observations",    format(nobs(m_cells), big.mark = ","), ""
)
kable(bind_rows(body_cells, footer_cells),
      format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
      align = c("l", "c", "c"),
      col.names = c("Cell (vs.\\ Below Top 25 / Low cath)",
                    "$\\hat{\\beta}$", "(SE)")) %>%
  row_spec(3, extra_latex_after = "\\midrule") %>%
  save_kable("results/tables/rank-x-cath-cells.tex")

cat("\n=== Wrote results/tables/rank-x-cath-stratified.tex ===\n")
cat("=== Wrote results/tables/rank-x-cath-cells.tex ===\n")
