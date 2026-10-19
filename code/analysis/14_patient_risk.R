# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-10-19
## Description:   Figure and appendix table for the patient-level threshold
##                analysis, built from the three files exported from the
##                VRDC R container (code/analysis/vrdc/1_threshold.R):
##                  results/threshold-coefs.csv
##                  results/threshold-quintiles.csv
##                  results/threshold-tests.csv
##
##                Figure: the training imprint on cath within 2 days by
##                quintile of baseline one-year mortality risk, for all
##                caths, diagnostic-only caths, and caths followed by
##                revascularization within 90 days.
##
##                Table: Panel A is the patient-level replication of the
##                headline (patient controls), Panel B the imprint by risk
##                quintile (risk-quintile fixed effects), in three columns:
##                baseline, with admitting-hospital fixed effects, and
##                without patients who die within 2 days of admission. The
##                equality test across quintiles is reported under Panel B.


# 1. Load -----------------------------------------------------------------

coefs <- read_csv("results/threshold-coefs.csv", show_col_types = FALSE) %>%
  mutate(quintile = as.integer(str_extract(term, "(?<=risk_q5::)\\d")))

quintiles <- read_csv("results/threshold-quintiles.csv", show_col_types = FALSE)

tests <- read_csv("results/threshold-tests.csv", show_col_types = FALSE)


# 2. Figure: imprint by risk quintile -------------------------------------

fig_data <- coefs %>%
  filter(model %in% c("quintile", "dxonly", "revasc")) %>%
  mutate(outcome = factor(model,
                          levels = c("quintile", "dxonly", "revasc"),
                          labels = c("All caths", "Diagnostic only",
                                     "Followed by revascularization")))

p_risk <- ggplot(fig_data,
                 aes(x = quintile, y = estimate, color = outcome, shape = outcome)) +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = 0.5), linewidth = 0.7) +
  scale_color_manual(values = c("All caths" = "gray15",
                                "Diagnostic only" = "gray45",
                                "Followed by revascularization" = "gray70")) +
  scale_shape_manual(values = c("All caths" = 16,
                                "Diagnostic only" = 17,
                                "Followed by revascularization" = 15)) +
  scale_x_continuous(breaks = 1:5,
                     labels = c("1\n(lowest)", "2", "3", "4", "5\n(highest)")) +
  labs(x = "Quintile of baseline one-year mortality risk",
       y = "Effect of training-HRR cath lab share\non Pr(cath within 2 days)",
       color = NULL, shape = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("results/figures/patient-risk-quintiles.png", p_risk,
       width = 7, height = 4.5, dpi = 300)


# 3. Table: headline and quintiles in three specifications ----------------

fmt_e <- function(x, p) {
  s <- sprintf("%.3f", x)
  if (p < 0.01) s <- paste0(s, "***")
  else if (p < 0.05) s <- paste0(s, "**")
  else if (p < 0.10) s <- paste0(s, "*")
  s
}
fmt_s <- function(x) paste0("(", sprintf("%.3f", x), ")")

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
  arrange(match(model, panel_b_models))

n_obs_b <- rows_b %>% filter(quintile == 1) %>% pull(n_obs)
n_card_b <- rows_b %>% filter(quintile == 1) %>% pull(n_cardiologists)

cell_row <- function(label, est, se, p) {
  paste0(label, " & ", paste(mapply(fmt_e, est, p), collapse = " & "), " \\\\\n",
         " & ", paste(sapply(se, fmt_s), collapse = " & "), " \\\\\n")
}

quintile_rows <- paste(sapply(1:5, function(q) {
  r <- rows_b %>% filter(quintile == q)
  cell_row(paste0("Risk quintile ", q), r$estimate, r$std.error, r$p.value)
}), collapse = "")

tbl <- paste0(
  "\\begin{tabular}{lccc}\n",
  "\\toprule\n",
  " & (1) & (2) & (3) \\\\\n",
  "\\midrule\n",
  "\\multicolumn{4}{l}{\\textit{Panel A. All patients}} \\\\\n",
  cell_row("Training-HRR cath lab share", row_a$estimate, row_a$std.error, row_a$p.value),
  "Observations & ", paste(format(row_a$n_obs, big.mark = ","), collapse = " & "), " \\\\\n",
  "\\midrule\n",
  "\\multicolumn{4}{l}{\\textit{Panel B. By quintile of baseline mortality risk}} \\\\\n",
  quintile_rows,
  "Equal effects across quintiles ($p$-value) & ",
  paste(sprintf("%.3f", p_equal$p.value), collapse = " & "), " \\\\\n",
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


# 4. Quintile descriptives ------------------------------------------------

quintiles %>%
  transmute(risk_q5, n,
            mean_pred_death = round(mean_pred_death, 3),
            cath_d2 = round(cath_d2, 3),
            cath_d2_dxonly = round(cath_d2_dxonly, 3),
            cath_d2_revasc = round(cath_d2_revasc, 3),
            death_30 = round(death_30, 3),
            death_365 = round(death_365, 3)) %>%
  write_csv("results/tables/patient-risk-quintiles.csv")
