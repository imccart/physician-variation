# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-05-05
## Description:   Robustness checks for the mover design, ported and adapted
##                from Shirley Cai's PUF/surgeons pipeline. Five specs:
##                  (1) Drop med school HRR intensity main effect (delta only)
##                  (2) Drop destination HRR FE (level + delta)
##                  (3) Scale by destination LOO (level)
##                  (4) Delta-intensity quartiles (heterogeneity)
##                  (5) Pos vs neg delta (asymmetry)

# 1. Load data ------------------------------------------------------------

analysis <- read_csv("data/output/analysis_panel.csv",
                     col_types = cols(npi = col_character(),
                                      year = col_integer(),
                                      .default = col_guess()))

movers <- analysis %>%
  filter(mover == 1,
         !is.na(intensity_med_school),
         !is.na(intensity_dest_loo),
         !is.nan(intensity_dest_loo))

mean_y <- weighted.mean(movers$mean_resid_cath, movers$n_nstemi, na.rm = TRUE)
mean_d <- weighted.mean(movers$intensity_change, movers$n_nstemi, na.rm = TRUE)


# Helpers ----------------------------------------------------------------

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
coef_row <- function(model, term) {
  td <- tidy(model)
  hit <- td %>% filter(term == !!term)
  if (nrow(hit) == 0) {
    list(est = NA_real_, se = NA_real_, p = NA_real_)
  } else {
    list(est = hit$estimate[1], se = hit$std.error[1], p = hit$p.value[1])
  }
}


# 2. Spec 1: Drop med school HRR intensity (delta only) ------------------

r1 <- feols(mean_resid_cath ~ intensity_change | hrr_practice + year,
            data = movers, weights = ~n_nstemi,
            cluster = ~hrr_med_school)


# 3. Spec 2: Drop destination HRR FE -------------------------------------

r2_lev <- feols(mean_resid_cath ~ intensity_med_school | year,
                data = movers, weights = ~n_nstemi,
                cluster = ~hrr_med_school)

r2_del <- feols(mean_resid_cath ~ intensity_change + intensity_med_school | year,
                data = movers, weights = ~n_nstemi,
                cluster = ~hrr_med_school)


# 4. (Dropped) "Scaled" spec ---------------------------------------------
#
# Shirley's PUF/surgeons pipeline included a spec dividing own intensity by
# destination intensity (own / dest_loo). That is interpretable when the
# outcome is a raw cath rate in [0,1]. Our outcome is the residualized cath
# rate from a patient-level LPM, which is centered near zero and roughly
# half negative -- the ratio is unstable, the > 0 filter would drop ~half
# the sample, and the spec doesn't carry a meaningful interpretation in our
# setting. Spec dropped.


# 5. Spec 4: Delta-intensity quartiles -----------------------------------

movers_q <- movers %>% mutate(delta_q = ntile(intensity_change, 4))

r4_q1 <- feols(mean_resid_cath ~ intensity_change | hrr_practice + year,
               data = movers_q %>% filter(delta_q == 1),
               weights = ~n_nstemi, cluster = ~hrr_med_school)
r4_q2 <- feols(mean_resid_cath ~ intensity_change | hrr_practice + year,
               data = movers_q %>% filter(delta_q == 2),
               weights = ~n_nstemi, cluster = ~hrr_med_school)
r4_q3 <- feols(mean_resid_cath ~ intensity_change | hrr_practice + year,
               data = movers_q %>% filter(delta_q == 3),
               weights = ~n_nstemi, cluster = ~hrr_med_school)
r4_q4 <- feols(mean_resid_cath ~ intensity_change | hrr_practice + year,
               data = movers_q %>% filter(delta_q == 4),
               weights = ~n_nstemi, cluster = ~hrr_med_school)


# 6. Spec 5: Positive vs negative delta ----------------------------------

movers_s <- movers %>%
  mutate(delta_pos = case_when(intensity_change >  0 ~ TRUE,
                               intensity_change <  0 ~ FALSE,
                               TRUE                  ~ NA))

r5_neg <- feols(mean_resid_cath ~ intensity_change | hrr_practice + year,
                data = movers_s %>% filter(delta_pos == FALSE),
                weights = ~n_nstemi, cluster = ~hrr_med_school)
r5_pos <- feols(mean_resid_cath ~ intensity_change | hrr_practice + year,
                data = movers_s %>% filter(delta_pos == TRUE),
                weights = ~n_nstemi, cluster = ~hrr_med_school)


# 7. Tables --------------------------------------------------------------

## 7a. Specs 1 + 2: drop FE / drop main effect ---------------------------

models_a <- list(r1, r2_lev, r2_del)
col_a    <- c("Drop med school", "Drop HRR FE (level)", "Drop HRR FE (delta)")

lev_a <- map(models_a, coef_row, term = "intensity_med_school")
del_a <- map(models_a, coef_row, term = "intensity_change")

body_a <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`,
  "Med school HRR intensity",
    fmt_est(lev_a[[1]]$est, lev_a[[1]]$p),
    fmt_est(lev_a[[2]]$est, lev_a[[2]]$p),
    fmt_est(lev_a[[3]]$est, lev_a[[3]]$p),
  "",
    fmt_se(lev_a[[1]]$se), fmt_se(lev_a[[2]]$se), fmt_se(lev_a[[3]]$se),
  "$\\Delta$ intensity",
    fmt_est(del_a[[1]]$est, del_a[[1]]$p),
    fmt_est(del_a[[2]]$est, del_a[[2]]$p),
    fmt_est(del_a[[3]]$est, del_a[[3]]$p),
  "",
    fmt_se(del_a[[1]]$se), fmt_se(del_a[[2]]$se), fmt_se(del_a[[3]]$se)
)

footer_a <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`,
  "Practice HRR FE", "Yes", "No",  "No",
  "Year FE",         "Yes", "Yes", "Yes",
  "Observations",
    format(nobs(r1),      big.mark = ","),
    format(nobs(r2_lev),  big.mark = ","),
    format(nobs(r2_del),  big.mark = ","),
  "$R^2$",
    sprintf("%.3f", r2(r1,     "r2")),
    sprintf("%.3f", r2(r2_lev, "r2")),
    sprintf("%.3f", r2(r2_del, "r2")),
  "Mean cath intensity",
    sprintf("%.3f", mean_y),
    sprintf("%.3f", mean_y),
    sprintf("%.3f", mean_y)
)

table_a <- bind_rows(body_a, footer_a)

kable(table_a,
      format    = "latex",
      booktabs  = TRUE,
      linesep   = "",
      escape    = FALSE,
      align     = c("l", rep("c", 3)),
      col.names = c("", col_a)) %>%
  row_spec(4, extra_latex_after = "\\addlinespace") %>%
  row_spec(7, extra_latex_after = "\\midrule") %>%
  save_kable("results/tables/robust-fe.tex")


## 7c. Spec 4: Delta-intensity quartiles ---------------------------------

models_q <- list(r4_q1, r4_q2, r4_q3, r4_q4)
del_q    <- map(models_q, coef_row, term = "intensity_change")

mean_y_q <- movers_q %>%
  group_by(delta_q) %>%
  summarise(m = weighted.mean(mean_resid_cath, n_nstemi, na.rm = TRUE)) %>%
  pull(m)
mean_d_q <- movers_q %>%
  group_by(delta_q) %>%
  summarise(m = weighted.mean(intensity_change, n_nstemi, na.rm = TRUE)) %>%
  pull(m)

body_q <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`,
  "$\\Delta$ intensity",
    fmt_est(del_q[[1]]$est, del_q[[1]]$p),
    fmt_est(del_q[[2]]$est, del_q[[2]]$p),
    fmt_est(del_q[[3]]$est, del_q[[3]]$p),
    fmt_est(del_q[[4]]$est, del_q[[4]]$p),
  "",
    fmt_se(del_q[[1]]$se), fmt_se(del_q[[2]]$se),
    fmt_se(del_q[[3]]$se), fmt_se(del_q[[4]]$se)
)

footer_q <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`,
  "Practice HRR FE", "Yes", "Yes", "Yes", "Yes",
  "Year FE",         "Yes", "Yes", "Yes", "Yes",
  "Observations",
    format(nobs(r4_q1), big.mark = ","),
    format(nobs(r4_q2), big.mark = ","),
    format(nobs(r4_q3), big.mark = ","),
    format(nobs(r4_q4), big.mark = ","),
  "Mean cath intensity",
    sprintf("%.3f", mean_y_q[1]),
    sprintf("%.3f", mean_y_q[2]),
    sprintf("%.3f", mean_y_q[3]),
    sprintf("%.3f", mean_y_q[4]),
  "Mean $\\Delta$",
    sprintf("%.3f", mean_d_q[1]),
    sprintf("%.3f", mean_d_q[2]),
    sprintf("%.3f", mean_d_q[3]),
    sprintf("%.3f", mean_d_q[4])
)

kable(bind_rows(body_q, footer_q),
      format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
      align = c("l", rep("c", 4)),
      col.names = c("", "Q1", "Q2", "Q3", "Q4")) %>%
  row_spec(2, extra_latex_after = "\\addlinespace") %>%
  row_spec(4, extra_latex_after = "\\midrule") %>%
  save_kable("results/tables/robust-quartiles.tex")


## 7d. Spec 5: Pos vs neg delta ------------------------------------------

del_s <- map(list(r5_neg, r5_pos), coef_row, term = "intensity_change")
mean_y_s <- movers_s %>%
  filter(!is.na(delta_pos)) %>%
  group_by(delta_pos) %>%
  summarise(m = weighted.mean(mean_resid_cath, n_nstemi, na.rm = TRUE)) %>%
  arrange(delta_pos) %>%
  pull(m)
mean_d_s <- movers_s %>%
  filter(!is.na(delta_pos)) %>%
  group_by(delta_pos) %>%
  summarise(m = weighted.mean(intensity_change, n_nstemi, na.rm = TRUE)) %>%
  arrange(delta_pos) %>%
  pull(m)

body_s <- tribble(
  ~term, ~`(1)`, ~`(2)`,
  "$\\Delta$ intensity",
    fmt_est(del_s[[1]]$est, del_s[[1]]$p),
    fmt_est(del_s[[2]]$est, del_s[[2]]$p),
  "",
    fmt_se(del_s[[1]]$se), fmt_se(del_s[[2]]$se)
)

footer_s <- tribble(
  ~term, ~`(1)`, ~`(2)`,
  "Practice HRR FE", "Yes", "Yes",
  "Year FE",         "Yes", "Yes",
  "Observations",
    format(nobs(r5_neg), big.mark = ","),
    format(nobs(r5_pos), big.mark = ","),
  "Mean cath intensity",
    sprintf("%.3f", mean_y_s[1]),
    sprintf("%.3f", mean_y_s[2]),
  "Mean $\\Delta$",
    sprintf("%.3f", mean_d_s[1]),
    sprintf("%.3f", mean_d_s[2])
)

kable(bind_rows(body_s, footer_s),
      format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
      align = c("l", rep("c", 2)),
      col.names = c("", "$\\Delta < 0$", "$\\Delta > 0$")) %>%
  row_spec(2, extra_latex_after = "\\addlinespace") %>%
  row_spec(4, extra_latex_after = "\\midrule") %>%
  save_kable("results/tables/robust-pos-neg.tex")
