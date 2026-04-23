# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-04-06
## Description:   Mover design regressions. Three specifications:
##                (1) Level of med school HRR intensity
##                (2) Semi-parametric with cubic spline
##                (3) Change in intensity (destination LOO - med school)

source("code/0-setup.R")
library(fixest)
library(splines)
library(broom)
library(kableExtra)


# 1. Load data ------------------------------------------------------------

analysis <- read_csv("data/output/analysis_panel.csv",
                     col_types = cols(npi = col_character(),
                                      year = col_integer(),
                                      .default = col_guess()))

# Restrict to movers with non-missing intensity measures
movers <- analysis %>%
  filter(mover == 1,
         !is.na(intensity_med_school),
         !is.na(intensity_dest_loo),
         !is.nan(intensity_dest_loo))

cat("Mover cardiologist-years:", nrow(movers), "\n")
cat("Unique mover cardiologists:", n_distinct(movers$npi), "\n")


# 2. Specification 1: Level of med school HRR intensity -------------------

# Regress cardiologist intensity on med school HRR intensity,
# controlling for destination HRR FE and year FE
m1 <- feols(mean_resid_cath ~ intensity_med_school | hrr_practice + year,
            data = movers, weights = ~n_nstemi,
            cluster = ~hrr_med_school)

summary(m1)


# 3. Specification 2: Semi-parametric (cubic spline) ----------------------

# Replace linear med school intensity with natural cubic spline
# to allow nonlinear relationship
movers <- movers %>%
  mutate(spline_basis = ns(intensity_med_school, df = 4))

m2 <- feols(mean_resid_cath ~ spline_basis | hrr_practice + year,
            data = movers, weights = ~n_nstemi,
            cluster = ~hrr_med_school)

summary(m2)

# Joint F-test on spline terms
wald(m2, "spline")


# 4. Specification 3: Change in intensity ---------------------------------

# Regress cardiologist intensity on the change in environment intensity
# (destination LOO minus med school HRR). Controls for destination HRR FE
# absorb the level of destination intensity.
m3 <- feols(mean_resid_cath ~ intensity_change | hrr_practice + year,
            data = movers, weights = ~n_nstemi,
            cluster = ~hrr_med_school)

summary(m3)


# 5. Robustness: add graduation cohort FE --------------------------------

m4 <- feols(mean_resid_cath ~ intensity_med_school | hrr_practice + year + grad_year,
            data = movers, weights = ~n_nstemi,
            cluster = ~hrr_med_school)

m5 <- feols(mean_resid_cath ~ intensity_change | hrr_practice + year + grad_year,
            data = movers, weights = ~n_nstemi,
            cluster = ~hrr_med_school)


# 6. Full sample: stayers + movers ----------------------------------------

full <- analysis %>%
  filter(!is.na(intensity_med_school),
         !is.na(intensity_dest_loo),
         !is.nan(intensity_dest_loo))

m6 <- feols(mean_resid_cath ~ intensity_med_school | hrr_practice + year,
            data = full, weights = ~n_nstemi,
            cluster = ~hrr_med_school)


# 7. Export results -------------------------------------------------------

# Hand-built kable() tables -- modelsummary backend options (kableExtra /
# factory_latex) proved unreliable in past work, so we extract coefficients
# and SEs directly and format in a tibble.

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

# Pull (est, se, p) for a given term out of a feols model
coef_row <- function(model, term) {
  td <- tidy(model)
  hit <- td %>% filter(term == !!term)
  if (nrow(hit) == 0) {
    list(est = NA_real_, se = NA_real_, p = NA_real_)
  } else {
    list(est = hit$estimate[1], se = hit$std.error[1], p = hit$p.value[1])
  }
}


## 7a. Main table: Level and Change specs, w/ and w/o cohort FE ----

models_main <- list(m1, m3, m4, m5)
col_names_main <- c("Level", "Change", "Level + Cohort", "Change + Cohort")

level_rows <- map(models_main, coef_row, term = "intensity_med_school")
change_rows <- map(models_main, coef_row, term = "intensity_change")

body_main <- tribble(
  ~term,
    ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`,
  "Med school HRR intensity",
    fmt_est(level_rows[[1]]$est, level_rows[[1]]$p),
    fmt_est(level_rows[[2]]$est, level_rows[[2]]$p),
    fmt_est(level_rows[[3]]$est, level_rows[[3]]$p),
    fmt_est(level_rows[[4]]$est, level_rows[[4]]$p),
  "",
    fmt_se(level_rows[[1]]$se),
    fmt_se(level_rows[[2]]$se),
    fmt_se(level_rows[[3]]$se),
    fmt_se(level_rows[[4]]$se),
  "$\\Delta$ intensity (dest $-$ med school)",
    fmt_est(change_rows[[1]]$est, change_rows[[1]]$p),
    fmt_est(change_rows[[2]]$est, change_rows[[2]]$p),
    fmt_est(change_rows[[3]]$est, change_rows[[3]]$p),
    fmt_est(change_rows[[4]]$est, change_rows[[4]]$p),
  "",
    fmt_se(change_rows[[1]]$se),
    fmt_se(change_rows[[2]]$se),
    fmt_se(change_rows[[3]]$se),
    fmt_se(change_rows[[4]]$se)
)

footer_main <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`,
  "Practice HRR FE", "Yes", "Yes", "Yes", "Yes",
  "Year FE",         "Yes", "Yes", "Yes", "Yes",
  "Grad year FE",    "No",  "No",  "Yes", "Yes",
  "Observations",
    format(nobs(m1), big.mark = ","),
    format(nobs(m3), big.mark = ","),
    format(nobs(m4), big.mark = ","),
    format(nobs(m5), big.mark = ","),
  "$R^2$",
    sprintf("%.3f", r2(m1, "r2")),
    sprintf("%.3f", r2(m3, "r2")),
    sprintf("%.3f", r2(m4, "r2")),
    sprintf("%.3f", r2(m5, "r2"))
)

table_main <- bind_rows(body_main, footer_main)

kable(table_main,
      format    = "latex",
      booktabs  = TRUE,
      linesep   = "",
      escape    = FALSE,
      align     = c("l", rep("c", 4)),
      col.names = c("", col_names_main)) %>%
  row_spec(4, extra_latex_after = "\\addlinespace") %>%
  row_spec(7, extra_latex_after = "\\midrule") %>%
  save_kable("results/tables/main-regressions.tex")


## 7b. Comparison table: movers vs. full sample --------------------

full_rows <- map(list(m1, m6), coef_row, term = "intensity_med_school")

body_full <- tribble(
  ~term, ~`(1)`, ~`(2)`,
  "Med school HRR intensity",
    fmt_est(full_rows[[1]]$est, full_rows[[1]]$p),
    fmt_est(full_rows[[2]]$est, full_rows[[2]]$p),
  "",
    fmt_se(full_rows[[1]]$se),
    fmt_se(full_rows[[2]]$se)
)

footer_full <- tribble(
  ~term, ~`(1)`, ~`(2)`,
  "Practice HRR FE", "Yes", "Yes",
  "Year FE",         "Yes", "Yes",
  "Observations",
    format(nobs(m1), big.mark = ","),
    format(nobs(m6), big.mark = ","),
  "$R^2$",
    sprintf("%.3f", r2(m1, "r2")),
    sprintf("%.3f", r2(m6, "r2"))
)

table_full <- bind_rows(body_full, footer_full)

kable(table_full,
      format    = "latex",
      booktabs  = TRUE,
      linesep   = "",
      escape    = FALSE,
      align     = c("l", rep("c", 2)),
      col.names = c("", "Movers", "Full Sample")) %>%
  row_spec(2, extra_latex_after = "\\addlinespace") %>%
  row_spec(4, extra_latex_after = "\\midrule") %>%
  save_kable("results/tables/movers-vs-full.tex")
