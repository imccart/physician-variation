# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-11-09
## Description:   Specialty composition as a function of training-period
##                cath-lab availability. Collapses the clinician roster to
##                medical-school-HRR by graduation-cohort cells and regresses
##                the share of graduates with a cardiology primary specialty
##                on the cath-lab share in the medical-school HRR, with
##                medical-school HRR and cohort fixed effects, weighted by
##                cell size and clustered on the medical-school HRR. The
##                cath-lab share is lagged one to four years before
##                matriculation, so the labs were in place before the cohort
##                arrived. Same cohort window as the headline imprint
##                (graduation years 1983-2006).
##
##                Inputs:
##                  data/output/physician_roster.csv
##                  data/output/aha_hrr_year.csv
##
##                Outputs:
##                  results/tables/specialty-composition-lag.tex
##                  results/tables/specialty-composition-lag.csv
##                  results/tables/specialty-roster-counts.csv

# 1. Cells -----------------------------------------------------------------

roster <- read_csv("data/output/physician_roster.csv",
                   col_types = cols(npi = col_character(),
                                    grad_year = col_integer(),
                                    hrr_med_school = col_integer(),
                                    .default = col_guess()),
                   show_col_types = FALSE) %>%
  filter(grad_year >= 1983, grad_year <= 2006, !is.na(train_cath_lab))

aha_hrr <- read_csv("data/output/aha_hrr_year.csv",
                    col_types = cols(hrr = col_integer(), year = col_integer(),
                                     .default = col_double()),
                    show_col_types = FALSE) %>%
  select(hrr, year, cath_lab_share)

# train_cath_lab is constant within a cell: the match year is a function of
# the graduation year and the HRR is the cell's own.
cells <- roster %>%
  group_by(hrr_med_school, grad_year) %>%
  summarize(train_cath_lab = first(train_cath_lab),
            n_grads        = n(),
            n_cardio       = sum(is_cardiologist),
            .groups = "drop") %>%
  mutate(share_cardio = n_cardio / n_grads)

coef_row <- function(model, term) {
  td <- tidy(model) %>% filter(term == !!term)
  list(est = td$estimate[1], se = td$std.error[1], p = td$p.value[1])
}
fmt_est <- function(x, p) {
  s <- sprintf("%.3f", x)
  if (p < 0.01) s <- paste0(s, "***")
  else if (p < 0.05) s <- paste0(s, "**")
  else if (p < 0.10) s <- paste0(s, "*")
  s
}
fmt_se <- function(x) paste0("(", sprintf("%.3f", x), ")")


# 2. Roster counts quoted in the appendix -----------------------------------

write_csv(tibble(n_graduates     = sum(cells$n_grads),
                 n_cardiologists = sum(cells$n_cardio),
                 share_cardio    = weighted.mean(cells$share_cardio, cells$n_grads),
                 n_hrr           = n_distinct(cells$hrr_med_school),
                 n_cells         = nrow(cells)),
          "results/tables/specialty-roster-counts.csv")


# 3. Lagged cath-lab share --------------------------------------------------

# The lag is measured from the matriculation year, grad_year - 4 for a fall
# start in a four-year program, so k = 1 is the year before the cohort
# arrived. A cell enters only when the lagged year is inside the AHA
# coverage (1980 on), so a lag of k years uses graduation cohorts 1984 + k
# to 2006.
lags <- c(1L, 2L, 3L, 4L)

lag_fit <- function(k) {
  d <- cells %>%
    mutate(lag_year = grad_year - 4L - k) %>%
    inner_join(aha_hrr %>% rename(train_cath_lab_lag = cath_lab_share),
               by = c("hrr_med_school" = "hrr", "lag_year" = "year"),
               relationship = "many-to-one")
  m <- feols(share_cardio ~ train_cath_lab_lag | hrr_med_school + grad_year,
             data = d, weights = ~n_grads, cluster = ~hrr_med_school)
  r <- coef_row(m, "train_cath_lab_lag")
  tibble(lag = k, beta = r$est, se = r$se, p = r$p,
         outcome_mean = weighted.mean(d$share_cardio, d$n_grads),
         n_cells = nobs(m), n_graduates = sum(d$n_grads),
         cohorts = paste0(min(d$grad_year), "--", max(d$grad_year)))
}

lag_out <- bind_rows(map(lags, lag_fit))
print(lag_out)

tbl_lag <- paste0(
  "\\begin{tabular}{lcccc}\n",
  "\\toprule\n",
  " & \\multicolumn{4}{c}{Cath lab share $k$ years before matriculation} \\\\\n",
  "\\cmidrule(lr){2-5}\n",
  " & ",
    paste0("$k=", lag_out$lag, "$", collapse = " & "), " \\\\\n",
  "\\midrule\n",
  "Training-HRR cath lab share & ",
    paste(map2_chr(lag_out$beta, lag_out$p, fmt_est), collapse = " & "), " \\\\\n",
  " & ",
    paste(map_chr(lag_out$se, fmt_se), collapse = " & "), " \\\\\n",
  "\\midrule\n",
  "Outcome mean & ",
    paste(sprintf("%.3f", lag_out$outcome_mean), collapse = " & "), " \\\\\n",
  "Med school HRR FE & Yes & Yes & Yes & Yes \\\\\n",
  "Cohort FE & Yes & Yes & Yes & Yes \\\\\n",
  "Observations & ",
    paste(format(lag_out$n_cells, big.mark = ","), collapse = " & "), " \\\\\n",
  "\\bottomrule\n",
  "\\end{tabular}\n"
)
writeLines(tbl_lag, "results/tables/specialty-composition-lag.tex")
write_csv(lag_out, "results/tables/specialty-composition-lag.csv")
