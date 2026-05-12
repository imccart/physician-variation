# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-05-07
## Description:   Dynamic calibration combining the causal training imprint
##                (beta_train, identified via within-origin x cohort
##                variation) and the within-physician destination effect
##                (beta_dest). Computes the steady-state amplification
##                from destination feedback and traces the transition path
##                of a one-time training-cath shock through cohort turnover.
##
##                Coefficients come from 8_aha_training.R section 10
##                (tightened identification specs).


# 1. Coefficients from the tightened-ID specs ---------------------------

beta_train <- 0.058   # from origin-FE + destination-FE + year-FE spec
beta_dest  <- 0.350   # pooled pre/post from event study (12_event_study.R)
amp        <- 1 / (1 - beta_dest)
long_run   <- beta_train * amp

cat("=== Dynamic calibration ===\n")
cat(sprintf("Training imprint  (within-origin x cohort):   beta_train = %.3f\n", beta_train))
cat(sprintf("Destination effect (within-physician):        beta_dest  = %.3f\n", beta_dest))
cat(sprintf("Steady-state amplification  1/(1-beta_dest):  %.3f\n", amp))
cat(sprintf("Long-run training elasticity beta_train * amp: %.3f\n", long_run))
cat(sprintf("Direct effect:                                 %.3f\n", beta_train))
cat(sprintf("Indirect (destination feedback):               %.3f\n", long_run - beta_train))
cat(sprintf("Feedback amplification:                        %.1f%%\n",
            100 * (long_run - beta_train) / beta_train))


# 2. Cohort transition path ---------------------------------------------

# Assume career length ~30 years -> 1/30 of practitioners turn over per
# year. Trace the cath-rate response to a 10pp training-cath-share shock.
horizon       <- 60
share_replace <- 1 / 20   # 5% per year, ~20-year career
shock_train   <- 0.10

path <- tibble(year = 0:horizon,
               cath_change   = NA_real_,
               share_treated = NA_real_)
path$share_treated[1] <- 0
path$cath_change[1]   <- 0
for (t in 2:nrow(path)) {
  path$share_treated[t] <- min(1, path$share_treated[t-1] + share_replace)
  direct  <- beta_train * shock_train * path$share_treated[t]
  feedback <- beta_dest  * path$cath_change[t-1]
  path$cath_change[t] <- direct + feedback
}

cat("\nDynamic transition path (10pp training-cath shock):\n")
print(path %>%
        filter(year %in% c(0, 5, 10, 20, 30, 45, 60)) %>%
        mutate(cath_change_pp = round(cath_change * 100, 4),
               share_treated  = round(share_treated, 3)))


# 3. Plot ---------------------------------------------------------------

p_dyn <- ggplot(path, aes(x = year, y = cath_change * 100)) +
  geom_line(linewidth = 1, color = "gray25") +
  geom_hline(yintercept = beta_train * shock_train * 100,
             linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = long_run * shock_train * 100,
             linetype = "dotted", color = "gray40") +
  annotate("text", x = horizon * 0.95, y = beta_train * shock_train * 100,
           label = "Direct (one-cohort) effect",
           hjust = 1, vjust = -0.5, color = "gray20", size = 3) +
  annotate("text", x = horizon * 0.95, y = long_run * shock_train * 100,
           label = "Steady-state (with feedback)",
           hjust = 1, vjust = -0.5, color = "gray20", size = 3) +
  labs(x = "Years since training reform",
       y = "Cath rate change (pp)") +
  theme_minimal()

ggsave("results/figures/dynamic-calibration.png", p_dyn,
       width = 7, height = 4.5, dpi = 300)

write_csv(path, "results/tables/dynamic-path.csv")

cat("\nWrote results/figures/dynamic-calibration.png\n")
cat("Wrote results/tables/dynamic-path.csv\n")


# 4. Place-based variance reduction transition path ---------------------

# Under training equalization, the training+peer component of place-based
# variance dissipates as cohorts turn over (mechanical, ~1/k per year)
# and the peer mean homogenizes (compounding via beta_dest). Other place
# factors persist. The fraction of place-based variance attributable to
# training+peer is s (we don't identify it; literature suggests s in
# roughly 0.3-0.7 range, anchored on Cutler/Badinski/FGW).
#
# At time t, the share of place-based variance still present:
#   1 - s * (1 - residual(t))
# where residual(t) is the share of training-driven heterogeneity still
# in the system. residual(t) declines toward 0 as cohorts homogenize.

# residual(t) for our equalization counterfactual: the share of cohorts
# still pre-shock at time t = (1 - share_treated[t])
# Plus the peer-mean share that hasn't yet equilibrated.
# Simple version: residual(t) = 1 - share_treated[t] + (peer adaptation lag).
# We use the decay implied by cath_change[t] / cath_change_steady_state
# as residual(t), since cath_change traces the path to equilibrium.

ss_value <- long_run * shock_train
path <- path %>% mutate(progress = cath_change / ss_value)

var_panel <- tibble(
  year   = path$year,
  lo     = 0.3 * path$progress,
  hi     = 0.7 * path$progress,
  anchor = 0.5 * path$progress
)

p_var <- ggplot(var_panel, aes(x = year)) +
  geom_ribbon(aes(ymin = lo, ymax = hi),
              fill = "gray75", alpha = 0.55) +
  geom_line(aes(y = anchor), color = "gray25", linewidth = 1.1) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 1)) +
  labs(x = "Years since training equalization",
       y = "Reduction in place-based cross-HRR variance") +
  theme_minimal()

ggsave("results/figures/place-variance-path.png", p_var,
       width = 7.5, height = 4.5, dpi = 300)
write_csv(var_panel, "results/tables/place-variance-path.csv")
cat("\nWrote results/figures/place-variance-path.png\n")
cat("Wrote results/tables/place-variance-path.csv\n")
