# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-09-29
## Description:   Does the training imprint shift the treatment threshold?
##                Patient-level version of the within-origin imprint
##                regression (Table 3 Panel B col 1), alone and with
##                graduation-year and admitting-hospital fixed effects,
##                then the imprint by
##                quintile of baseline one-year mortality risk, separately
##                for caths followed by revascularization and diagnostic-
##                only caths, and for 30- and 365-day mortality, each
##                also with admitting-hospital fixed effects. Then balance
##                of patient risk on training exposure, the cath models
##                without patients who die within 2 days of admission,
##                and tests of equal imprints across risk quintiles.
##
##                Inputs (VRDC R container):
##                  data/input/nstemi_threshold.csv      (SAS step 8 export)
##                  data/input/nstemi_death.csv          (SAS step 9 export)
##                  data/input/cardiologist_exposure.csv (uploaded; built by
##                                                        code/data-build/5_exposure_upload.R)
##
##                Outputs (container results/, for manual export):
##                  results/threshold-coefs.csv
##                  results/threshold-quintiles.csv
##                  results/threshold-tests.csv

# 1. Inputs ----------------------------------------------------------------

thr <- read_csv("data/input/nstemi_threshold.csv",
                col_types = cols(BENE_ID = col_character(),
                                 PRVDR_NUM = col_character(),
                                 NPI_Gatekeeper = col_character(),
                                 .default = col_guess()),
                show_col_types = FALSE) %>%
  rename_with(tolower) %>%
  rename(npi = npi_gatekeeper, year = ami_year)

expo <- read_csv("data/input/cardiologist_exposure.csv",
                 col_types = cols(npi = col_character(),
                                  year = col_integer(),
                                  .default = col_guess()),
                 show_col_types = FALSE)

death <- read_csv("data/input/nstemi_death.csv",
                  col_types = cols(BENE_ID = col_character(),
                                   Days_To_Death = col_double()),
                  show_col_types = FALSE) %>%
  rename_with(tolower)

# The exposure file holds the cardiologist-years of the paper's analytic
# sample, so the inner join applies the same sample restriction here.
d <- thr %>%
  left_join(death, by = "bene_id", relationship = "one-to-one") %>%
  inner_join(expo %>%
               select(npi, year, hrr_practice, hrr_med_school, grad_year,
                      train_cath_lab, intensity_dest_loo),
             by = c("npi", "year"), relationship = "many-to-one") %>%
  mutate(risk_q5 = factor(risk_q5),
         d_q1 = as.integer(risk_q5 == "1"),
         d_q2 = as.integer(risk_q5 == "2"),
         d_q3 = as.integer(risk_q5 == "3"),
         d_q4 = as.integer(risk_q5 == "4"),
         d_q5 = as.integer(risk_q5 == "5"),
         # same window as the cath outcome (procedure within 2 days)
         d_death_d2 = as.integer(!is.na(days_to_death) & days_to_death <= 2),
         pred_death_c = pred_death_365 - mean(pred_death_365),
         # age in decades; the raw square runs into the thousands and leaves the
         # clustered VCOV ill-conditioned
         age = age / 10, age_sq = age_sq / 100)

setFixest_fml(..ctrl = ~ age + age_sq + d_female + d_black + d_hisp + d_asian +
                d_race_missing + d_sex_missing + d_dual + dual_elgbl_mons +
                chf + car + val + pcd + pvd + htn_uncx + htn_cx + par + neu + cpd +
                diab_uncx + diab_cx + thy_hypo + renal_fl + liver + pud + aids +
                lymph + mets + tumor + rheu + coag + obese + wgt_loss + fluid +
                anemia_blood + anemia_def + alcohol + drug + psychoses + depression)


# 2. Patient-level replication of the within-origin imprint ---------------

m_base <- feols(d_cath_d2 ~ train_cath_lab + intensity_dest_loo + ..ctrl |
                  hrr_med_school + hrr_practice + year,
                data = d, cluster = ~hrr_med_school)
print(summary(m_base))

m_base_gy <- feols(d_cath_d2 ~ train_cath_lab + intensity_dest_loo + ..ctrl |
                     hrr_med_school + hrr_practice + year + grad_year,
                   data = d, cluster = ~hrr_med_school)
print(summary(m_base_gy))

m_base_hosp <- feols(d_cath_d2 ~ train_cath_lab + intensity_dest_loo + ..ctrl |
                       hrr_med_school + hrr_practice + year + prvdr_num,
                     data = d, cluster = ~hrr_med_school)
print(summary(m_base_hosp))

m_base_both <- feols(d_cath_d2 ~ train_cath_lab + intensity_dest_loo + ..ctrl |
                       hrr_med_school + hrr_practice + year + grad_year + prvdr_num,
                     data = d, cluster = ~hrr_med_school)
print(summary(m_base_both))


# 3. Imprint by baseline risk quintile ------------------------------------

m_q <- feols(d_cath_d2 ~ i(risk_q5, train_cath_lab) + intensity_dest_loo + ..ctrl |
               hrr_med_school + hrr_practice + year + risk_q5,
             data = d, cluster = ~hrr_med_school)
print(summary(m_q))

m_lin <- feols(d_cath_d2 ~ train_cath_lab + train_cath_lab:pred_death_c +
                 intensity_dest_loo + ..ctrl |
                 hrr_med_school + hrr_practice + year,
               data = d, cluster = ~hrr_med_school)
print(summary(m_lin))


# 4. Diagnostic-only versus revascularized caths --------------------------

m_dx <- feols(d_cath_d2_dxonly ~ i(risk_q5, train_cath_lab) + intensity_dest_loo + ..ctrl |
                hrr_med_school + hrr_practice + year + risk_q5,
              data = d, cluster = ~hrr_med_school)
print(summary(m_dx))

m_rv <- feols(d_cath_d2_revasc ~ i(risk_q5, train_cath_lab) + intensity_dest_loo + ..ctrl |
                hrr_med_school + hrr_practice + year + risk_q5,
              data = d, cluster = ~hrr_med_school)
print(summary(m_rv))


# 5. Mortality by risk quintile -------------------------------------------

m_d30 <- feols(d_death_30 ~ i(risk_q5, train_cath_lab) + intensity_dest_loo + ..ctrl |
                 hrr_med_school + hrr_practice + year + risk_q5,
               data = d, cluster = ~hrr_med_school)
print(summary(m_d30))

m_d365 <- feols(d_death_365 ~ i(risk_q5, train_cath_lab) + intensity_dest_loo + ..ctrl |
                  hrr_med_school + hrr_practice + year + risk_q5,
                data = d %>% filter(fu_365 == 1), cluster = ~hrr_med_school)
print(summary(m_d365))


# 6. Quintile models with admitting-hospital fixed effects ----------------

m_q_h <- feols(d_cath_d2 ~ i(risk_q5, train_cath_lab) + intensity_dest_loo + ..ctrl |
                 hrr_med_school + hrr_practice + year + risk_q5 + prvdr_num,
               data = d, cluster = ~hrr_med_school)
print(summary(m_q_h))

m_dx_h <- feols(d_cath_d2_dxonly ~ i(risk_q5, train_cath_lab) + intensity_dest_loo + ..ctrl |
                  hrr_med_school + hrr_practice + year + risk_q5 + prvdr_num,
                data = d, cluster = ~hrr_med_school)
print(summary(m_dx_h))

m_rv_h <- feols(d_cath_d2_revasc ~ i(risk_q5, train_cath_lab) + intensity_dest_loo + ..ctrl |
                  hrr_med_school + hrr_practice + year + risk_q5 + prvdr_num,
                data = d, cluster = ~hrr_med_school)
print(summary(m_rv_h))

m_d30_h <- feols(d_death_30 ~ i(risk_q5, train_cath_lab) + intensity_dest_loo + ..ctrl |
                   hrr_med_school + hrr_practice + year + risk_q5 + prvdr_num,
                 data = d, cluster = ~hrr_med_school)
print(summary(m_d30_h))

m_d365_h <- feols(d_death_365 ~ i(risk_q5, train_cath_lab) + intensity_dest_loo + ..ctrl |
                    hrr_med_school + hrr_practice + year + risk_q5 + prvdr_num,
                  data = d %>% filter(fu_365 == 1), cluster = ~hrr_med_school)
print(summary(m_d365_h))


# 7. Balance of patient risk on training exposure -------------------------

# No patient controls: predicted risk and its quintiles are built from them.
m_bal_pred <- feols(pred_death_365 ~ train_cath_lab + intensity_dest_loo |
                      hrr_med_school + hrr_practice + year,
                    data = d, cluster = ~hrr_med_school)
print(summary(m_bal_pred))

m_bal_q1 <- feols(d_q1 ~ train_cath_lab + intensity_dest_loo |
                    hrr_med_school + hrr_practice + year,
                  data = d, cluster = ~hrr_med_school)
m_bal_q2 <- feols(d_q2 ~ train_cath_lab + intensity_dest_loo |
                    hrr_med_school + hrr_practice + year,
                  data = d, cluster = ~hrr_med_school)
m_bal_q3 <- feols(d_q3 ~ train_cath_lab + intensity_dest_loo |
                    hrr_med_school + hrr_practice + year,
                  data = d, cluster = ~hrr_med_school)
m_bal_q4 <- feols(d_q4 ~ train_cath_lab + intensity_dest_loo |
                    hrr_med_school + hrr_practice + year,
                  data = d, cluster = ~hrr_med_school)
m_bal_q5 <- feols(d_q5 ~ train_cath_lab + intensity_dest_loo |
                    hrr_med_school + hrr_practice + year,
                  data = d, cluster = ~hrr_med_school)
print(etable(m_bal_q1, m_bal_q2, m_bal_q3, m_bal_q4, m_bal_q5))


# 8. Cath models without deaths within 2 days -----------------------------

# A patient who dies inside the cath window cannot be cathed, so these
# versions drop those patients.
m_base_ex <- feols(d_cath_d2 ~ train_cath_lab + intensity_dest_loo + ..ctrl |
                     hrr_med_school + hrr_practice + year,
                   data = d %>% filter(d_death_d2 == 0), cluster = ~hrr_med_school)
print(summary(m_base_ex))

m_q_ex <- feols(d_cath_d2 ~ i(risk_q5, train_cath_lab) + intensity_dest_loo + ..ctrl |
                  hrr_med_school + hrr_practice + year + risk_q5,
                data = d %>% filter(d_death_d2 == 0), cluster = ~hrr_med_school)
print(summary(m_q_ex))

m_death_d2 <- feols(d_death_d2 ~ train_cath_lab + intensity_dest_loo + ..ctrl |
                      hrr_med_school + hrr_practice + year,
                    data = d, cluster = ~hrr_med_school)
print(summary(m_death_d2))


# 9. Equality of the imprint across risk quintiles ------------------------

# Wald tests from each quintile model's clustered VCOV: each of Q2-Q5
# against Q1, and all five equal (F with the model's t degrees of freedom).
quintile_tests <- function(m) {
  b <- coef(m)[str_detect(names(coef(m)), "^risk_q5::")]
  V <- vcov(m)[names(b), names(b)]
  R <- cbind(-1, diag(4))
  rb <- as.numeric(R %*% b)
  rvr <- R %*% V %*% t(R)
  df2 <- degrees_freedom(m, "t")
  se <- sqrt(diag(rvr))
  f_all <- as.numeric(t(rb) %*% solve(rvr) %*% rb) / 4
  p <- c(2 * pt(-abs(rb / se), df2), pf(f_all, 4, df2, lower.tail = FALSE))
  tibble(test = c(paste0("q", 2:5, "_minus_q1"), "all_equal"),
         estimate = c(rb, NA),
         std.error = c(se, NA),
         statistic = c(rb / se, f_all),
         df1 = c(rep(1, 4), 4),
         df2 = df2,
         p.value = p)
}


# 10. Outputs -------------------------------------------------------------

models <- list(base = m_base, base_gradyear = m_base_gy,
               base_hospital = m_base_hosp, base_both = m_base_both,
               quintile = m_q, linear = m_lin,
               dxonly = m_dx, revasc = m_rv,
               death30 = m_d30, death365 = m_d365,
               quintile_hospital = m_q_h, dxonly_hospital = m_dx_h,
               revasc_hospital = m_rv_h, death30_hospital = m_d30_h,
               death365_hospital = m_d365_h,
               balance_pred_death = m_bal_pred,
               balance_q1 = m_bal_q1, balance_q2 = m_bal_q2,
               balance_q3 = m_bal_q3, balance_q4 = m_bal_q4,
               balance_q5 = m_bal_q5,
               base_excl_death_d2 = m_base_ex,
               quintile_excl_death_d2 = m_q_ex,
               death_d2 = m_death_d2)

# fixest_data() returns the rows each model was fit on, including the
# filtered samples, so the counts match the estimation sample.
coefs <- imap_dfr(models, function(m, nm) {
  est <- fixest_data(m, sample = "estimation")
  tidy(m, conf.int = TRUE) %>%
    filter(str_detect(term, "train_cath_lab")) %>%
    mutate(model = nm,
           n_obs = nobs(m),
           n_cardiologists = n_distinct(est$npi),
           n_clusters = n_distinct(est$hrr_med_school)) %>%
    select(model, term, estimate, std.error, conf.low, conf.high, p.value,
           n_obs, n_cardiologists, n_clusters)
})
write_csv(coefs, "results/threshold-coefs.csv")

quintiles <- d %>%
  group_by(risk_q5) %>%
  summarize(n = n(),
            mean_pred_death = mean(pred_death_365),
            cath_d2 = mean(d_cath_d2),
            cath_d2_dxonly = mean(d_cath_d2_dxonly),
            cath_d2_revasc = mean(d_cath_d2_revasc),
            death_d2 = mean(d_death_d2),
            death_30 = mean(d_death_30),
            death_365 = mean(d_death_365[fu_365 == 1]),
            .groups = "drop")
write_csv(quintiles, "results/threshold-quintiles.csv")

quintile_models <- list(quintile = m_q, dxonly = m_dx, revasc = m_rv,
                        death30 = m_d30, death365 = m_d365,
                        quintile_hospital = m_q_h, dxonly_hospital = m_dx_h,
                        revasc_hospital = m_rv_h, death30_hospital = m_d30_h,
                        death365_hospital = m_d365_h,
                        quintile_excl_death_d2 = m_q_ex)

tests <- imap_dfr(quintile_models, function(m, nm) {
  quintile_tests(m) %>%
    mutate(model = nm, .before = 1)
})
write_csv(tests, "results/threshold-tests.csv")
