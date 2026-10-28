# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-10-19
## Description:   Figure and appendix tables for the patient-level threshold
##                analysis, built from the three files exported from the
##                VRDC R container (code/analysis/vrdc/1_threshold.R):
##                  results/threshold-coefs.csv
##                  results/threshold-quintiles.csv
##                  results/threshold-tests.csv
##
##                Figure: the effect of training exposure on cath within
##                2 days by quintile of baseline one-year mortality risk.
##
##                Tables: the patient-level effect (Panel A the average
##                effect with patient controls, Panel B by quintile with
##                risk-quintile fixed effects) at baseline, with
##                admitting-hospital fixed effects, and without patients
##                who die within 2 days; 30-day and one-year mortality,
##                average and by quintile; and stay counts, predicted risk,
##                cath rates and mortality by quintile.


# 1. Load -----------------------------------------------------------------

coefs <- read_csv("results/threshold-coefs.csv", show_col_types = FALSE) %>%
  mutate(quintile = as.integer(str_extract(term, "(?<=risk_q5::)\\d")))

quintiles <- read_csv("results/threshold-quintiles.csv", show_col_types = FALSE)

tests <- read_csv("results/threshold-tests.csv", show_col_types = FALSE)


# 2. Figure: imprint by risk quintile -------------------------------------

p_risk_all <- ggplot(coefs %>% filter(model == "quintile"),
                     aes(x = quintile, y = estimate)) +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  color = "gray15", linewidth = 0.7) +
  scale_x_continuous(breaks = 1:5,
                     labels = c("1\n(lowest)", "2", "3", "4", "5\n(highest)")) +
  labs(x = "Quintile of baseline one-year mortality risk",
       y = "Effect of training-HRR cath lab share\non Pr(cath within 2 days)") +
  theme_minimal()

ggsave("results/figures/patient-risk-quintiles.png", p_risk_all,
       width = 7, height = 4.5, dpi = 300)


# 3. Formatting -----------------------------------------------------------

fmt_e <- function(x, p, digits = 3) {
  s <- str_replace(sprintf(paste0("%.", digits, "f"), x), "^-", "$-$")
  if (p < 0.01) s <- paste0(s, "***")
  else if (p < 0.05) s <- paste0(s, "**")
  else if (p < 0.10) s <- paste0(s, "*")
  s
}
fmt_s <- function(x, digits = 3) paste0("(", sprintf(paste0("%.", digits, "f"), x), ")")

cell_row <- function(label, est, se, p, digits = 3) {
  paste0(label, " & ", paste(mapply(fmt_e, est, p, digits), collapse = " & "), " \\\\\n",
         " & ", paste(sapply(se, fmt_s, digits), collapse = " & "), " \\\\\n")
}


# 4. Table: patient-level effect, average and by quintile -----------------

panel_a_models <- c("base", "base_hospital", "base_excl_death_d2")
panel_b_models <- c("quintile", "quintile_hospital", "quintile_excl_death_d2")

row_a <- coefs %>%
  filter(model %in% panel_a_models) %>%
  arrange(match(model, panel_a_models))

rows_b <- coefs %>%
  filter(model %in% panel_b_models) %>%
  arrange(quintile, match(model, panel_b_models))

p_equal <- tests %>%
  filter(model %in% panel_b_models, test == "all_equal") %>%
  arrange(match(model, panel_b_models)) %>%
  pull(p.value)

n_obs_b <- rows_b %>% filter(quintile == 1) %>% pull(n_obs)
n_card_b <- rows_b %>% filter(quintile == 1) %>% pull(n_cardiologists)

quintile_rows <- paste(sapply(1:5, function(q) {
  r <- rows_b %>% filter(quintile == q)
  cell_row(paste0("Risk quintile ", q), pull(r, estimate), pull(r, std.error), pull(r, p.value))
}), collapse = "")

tbl <- paste0(
  "\\begin{tabular}{lccc}\n",
  "\\toprule\n",
  " & (1) & (2) & (3) \\\\\n",
  "\\midrule\n",
  "\\multicolumn{4}{l}{\\textit{Panel A. All patients}} \\\\\n",
  cell_row("Training-HRR cath lab share", pull(row_a, estimate), pull(row_a, std.error),
           pull(row_a, p.value)),
  "Observations & ", paste(format(pull(row_a, n_obs), big.mark = ","), collapse = " & "), " \\\\\n",
  "\\midrule\n",
  "\\multicolumn{4}{l}{\\textit{Panel B. By quintile of baseline mortality risk}} \\\\\n",
  quintile_rows,
  "Equal effects across quintiles ($p$-value) & ",
  paste(ifelse(p_equal < 0.001, "$<$0.001", sprintf("%.3f", p_equal)), collapse = " & "), " \\\\\n",
  "Observations & ", paste(format(n_obs_b, big.mark = ","), collapse = " & "), " \\\\\n",
  "\\midrule\n",
  "Medical-school HRR, practice HRR, year FE & Yes & Yes & Yes \\\\\n",
  "Admitting hospital FE & No & Yes & No \\\\\n",
  "Sample & All & All & Survived 2 days \\\\\n",
  "Cardiologists & ", paste(format(n_card_b, big.mark = ","), collapse = " & "), " \\\\\n",
  "\\bottomrule\n",
  "\\end{tabular}\n"
)
writeLines(tbl, "results/tables/patient-risk.tex")


# 5. Table: mortality, average and by quintile -----------------------------

mort_a <- c("death30_pooled", "death365_pooled")
mort_b <- c("death30", "death365")

row_mort_a <- coefs %>%
  filter(model %in% mort_a) %>%
  arrange(match(model, mort_a))

rows_mort_b <- paste(sapply(1:5, function(q) {
  r <- coefs %>%
    filter(model %in% mort_b, quintile == q) %>%
    arrange(match(model, mort_b))
  cell_row(paste0("Risk quintile ", q), pull(r, estimate), pull(r, std.error), pull(r, p.value))
}), collapse = "")

p_equal_mort <- tests %>%
  filter(model %in% mort_b, test == "all_equal") %>%
  arrange(match(model, mort_b)) %>%
  pull(p.value)

n_obs_mort_b <- coefs %>%
  filter(model %in% mort_b, quintile == 1) %>%
  arrange(match(model, mort_b)) %>%
  pull(n_obs)
n_card_mort <- coefs %>%
  filter(model %in% mort_b, quintile == 1) %>%
  arrange(match(model, mort_b)) %>%
  pull(n_cardiologists)

tbl_mort <- paste0(
  "\\begin{tabular}{lcc}\n",
  "\\toprule\n",
  " & 30-day mortality & One-year mortality \\\\\n",
  " & (1) & (2) \\\\\n",
  "\\midrule\n",
  "\\multicolumn{3}{l}{\\textit{Panel A. Average effect}} \\\\\n",
  cell_row("Training-HRR cath lab share", pull(row_mort_a, estimate), pull(row_mort_a, std.error),
           pull(row_mort_a, p.value)),
  "Observations & ", paste(format(pull(row_mort_a, n_obs), big.mark = ","), collapse = " & "), " \\\\\n",
  "\\midrule\n",
  "\\multicolumn{3}{l}{\\textit{Panel B. By quintile of baseline mortality risk}} \\\\\n",
  rows_mort_b,
  "Equal effects across quintiles ($p$-value) & ",
  paste(ifelse(p_equal_mort < 0.001, "$<$0.001", sprintf("%.3f", p_equal_mort)), collapse = " & "), " \\\\\n",
  "Observations & ", paste(format(n_obs_mort_b, big.mark = ","), collapse = " & "), " \\\\\n",
  "\\midrule\n",
  "Medical-school HRR, practice HRR, year FE & Yes & Yes \\\\\n",
  "Cardiologists & ", paste(format(n_card_mort, big.mark = ","), collapse = " & "), " \\\\\n",
  "\\bottomrule\n",
  "\\end{tabular}\n"
)
writeLines(tbl_mort, "results/tables/patient-mortality.tex")


# 6. Quintile descriptives ------------------------------------------------

quintiles_out <- quintiles %>%
  transmute(risk_q5, n,
            mean_pred_death = round(mean_pred_death, 3),
            cath_d2 = round(cath_d2, 3),
            death_30 = round(death_30, 3),
            death_365 = round(death_365, 3))
write_csv(quintiles_out, "results/tables/patient-risk-quintiles.csv")

rows_desc <- quintiles_out %>%
  mutate(row = paste0(risk_q5, " & ", format(n, big.mark = ","), " & ",
                      sprintf("%.3f", mean_pred_death), " & ",
                      sprintf("%.3f", cath_d2), " & ",
                      sprintf("%.3f", death_30), " & ",
                      sprintf("%.3f", death_365), " \\\\\n")) %>%
  pull(row) %>%
  paste(collapse = "")

tbl_desc <- paste0(
  "\\begin{tabular}{lccccc}\n",
  "\\toprule\n",
  " & & & & \\multicolumn{2}{c}{Mortality} \\\\\n",
  "\\cmidrule(lr){5-6}\n",
  "Risk quintile & Inpatient stays & \\shortstack{Predicted \\\\ one-year \\\\ mortality} & ",
  "\\shortstack{Cath within \\\\ 2 days} & 30-day & One-year \\\\\n",
  "\\midrule\n",
  rows_desc,
  "\\bottomrule\n",
  "\\end{tabular}\n"
)
writeLines(tbl_desc, "results/tables/patient-risk-quintiles.tex")
