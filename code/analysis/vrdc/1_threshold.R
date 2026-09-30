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
##                only caths, and for 30- and 365-day mortality.
##
##                Inputs (VRDC R container):
##                  data/input/nstemi_threshold.csv      (SAS step 8 export)
##                  data/input/cardiologist_exposure.csv (uploaded; built by
##                                                        code/data-build/5_exposure_upload.R)
##
##                Outputs (container results/, for manual export):
##                  results/threshold-coefs.csv
##                  results/threshold-quintiles.csv

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

# The exposure file holds the cardiologist-years of the paper's analytic
# sample, so the inner join applies the same sample restriction here.
d <- thr %>%
  inner_join(expo %>%
               select(npi, year, hrr_practice, hrr_med_school, grad_year,
                      train_cath_lab, intensity_dest_loo),
             by = c("npi", "year")) %>%
  mutate(risk_q5 = factor(risk_q5),
         pred_death_c = pred_death_365 - mean(pred_death_365))

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


# 6. Outputs --------------------------------------------------------------

models <- list(base = m_base, base_gradyear = m_base_gy,
               base_hospital = m_base_hosp, base_both = m_base_both,
               quintile = m_q, linear = m_lin,
               dxonly = m_dx, revasc = m_rv,
               death30 = m_d30, death365 = m_d365)

coefs <- imap_dfr(models, function(m, nm) {
  tidy(m, conf.int = TRUE) %>%
    filter(str_detect(term, "train_cath_lab")) %>%
    mutate(model = nm,
           n_obs = nobs(m),
           n_cardiologists = n_distinct(d$npi[obs(m)]),
           n_clusters = n_distinct(d$hrr_med_school[obs(m)])) %>%
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
            death_30 = mean(d_death_30),
            death_365 = mean(d_death_365[fu_365 == 1]),
            .groups = "drop")
write_csv(quintiles, "results/threshold-quintiles.csv")
