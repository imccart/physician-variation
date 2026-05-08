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


# 2. Main rank-gradient regressions --------------------------------------

# Drop "OTHER" (non-US / unknown) schools and rows with no NIH match.
reg_data <- ranked %>%
  filter(!is.na(med_school), med_school != "OTHER",
         !is.na(nih_tier),
         !is.na(mean_resid_cath))

cat("\nRegression sample:", nrow(reg_data), "rows,",
    n_distinct(reg_data$npi), "cardiologists\n\n")

# Continuous: NIH rank (1 = top, larger = lower). Lower is better.
# Use -log(rank) so positive coefficient means top schools have higher cath
m_logrank <- feols(mean_resid_cath ~ I(-log(nih_rank)) |
                     hrr_practice + year,
                   data = reg_data, weights = ~n_nstemi,
                   cluster = ~hrr_med_school)

# Tier indicators (relative to "below top 100")
m_tier <- feols(mean_resid_cath ~ nih_tier | hrr_practice + year,
                data = reg_data, weights = ~n_nstemi,
                cluster = ~hrr_med_school)

# Binary top 25
m_t25  <- feols(mean_resid_cath ~ nih_top25 | hrr_practice + year,
                data = reg_data, weights = ~n_nstemi,
                cluster = ~hrr_med_school)

# Binary top 10
m_t10  <- feols(mean_resid_cath ~ nih_top10 | hrr_practice + year,
                data = reg_data, weights = ~n_nstemi,
                cluster = ~hrr_med_school)

cat("=== A: -log(rank) (positive => higher-ranked schools cath more) ===\n")
print(summary(m_logrank))
cat("\n=== B: NIH tier indicators (ref = lowest tier) ===\n")
print(summary(m_tier))
cat("\n=== C: binary Top 25 ===\n")
print(summary(m_t25))
cat("\n=== D: binary Top 10 ===\n")
print(summary(m_t10))


# 3. Movers-only sensitivity ---------------------------------------------

mov <- reg_data %>% filter(mover == 1)
m_t25_mov <- feols(mean_resid_cath ~ nih_top25 | hrr_practice + year,
                   data = mov, weights = ~n_nstemi,
                   cluster = ~hrr_med_school)
m_t10_mov <- feols(mean_resid_cath ~ nih_top10 | hrr_practice + year,
                   data = mov, weights = ~n_nstemi,
                   cluster = ~hrr_med_school)
cat("\n=== Movers-only Top 25 ===\n");  print(summary(m_t25_mov))
cat("\n=== Movers-only Top 10 ===\n");  print(summary(m_t10_mov))


# 4. Export side-by-side table -------------------------------------------

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

lr   <- mc_row(m_logrank, "I(-log(nih_rank))")
t10v <- mc_row(m_tier,    "nih_tier01_top10")
t25v <- mc_row(m_tier,    "nih_tier02_top25")
t50v <- mc_row(m_tier,    "nih_tier03_top50")
t100v<- mc_row(m_tier,    "nih_tier04_top100")
b25  <- mc_row(m_t25,     "nih_top25")
b10  <- mc_row(m_t10,     "nih_top10")
b25m <- mc_row(m_t25_mov, "nih_top25")
b10m <- mc_row(m_t10_mov, "nih_top10")

body_rk <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`,
  "$-\\log(\\text{NIH rank})$",
    fmt_e(lr$est, lr$p), " ", " ", " ",
  "",
    fmt_s(lr$se), " ", " ", " ",
  "Top 10",
    " ", fmt_e(t10v$est, t10v$p), fmt_e(b10$est, b10$p), fmt_e(b10m$est, b10m$p),
  "",
    " ", fmt_s(t10v$se), fmt_s(b10$se), fmt_s(b10m$se),
  "Top 11-25",
    " ", fmt_e(t25v$est, t25v$p), " ", " ",
  "",
    " ", fmt_s(t25v$se), " ", " ",
  "Top 26-50",
    " ", fmt_e(t50v$est, t50v$p), " ", " ",
  "",
    " ", fmt_s(t50v$se), " ", " ",
  "Top 51-100",
    " ", fmt_e(t100v$est, t100v$p), " ", " ",
  "",
    " ", fmt_s(t100v$se), " ", " ",
  "Top 25 (binary)",
    " ", " ", fmt_e(b25$est, b25$p), fmt_e(b25m$est, b25m$p),
  "",
    " ", " ", fmt_s(b25$se), fmt_s(b25m$se)
)

footer_rk <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`,
  "Practice HRR FE", "Yes", "Yes", "Yes", "Yes",
  "Year FE",         "Yes", "Yes", "Yes", "Yes",
  "Sample",          "Full", "Full", "Full", "Movers",
  "Observations",
    format(nobs(m_logrank), big.mark = ","),
    format(nobs(m_tier),    big.mark = ","),
    format(nobs(m_t25),     big.mark = ","),
    format(nobs(m_t25_mov), big.mark = ",")
)

table_rk <- bind_rows(body_rk, footer_rk)

kable(table_rk,
      format    = "latex",
      booktabs  = TRUE,
      linesep   = "",
      escape    = FALSE,
      align     = c("l", rep("c", 4)),
      col.names = c("",
                    "$-\\log$ rank",
                    "Tiers",
                    "Top 10/25",
                    "Movers")) %>%
  row_spec(12, extra_latex_after = "\\midrule") %>%
  save_kable("results/tables/rank.tex")

cat("\n=== Wrote results/tables/rank.tex ===\n")
