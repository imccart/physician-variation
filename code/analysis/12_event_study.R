# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-05-07
## Description:   Molitor-style event study around mid-career moves.
##                For cardiologists who change practice HRR within the
##                2008-2018 panel, trace the trajectory of cath rate in
##                event time relative to the move. Tests parallel trends
##                pre-move and quantifies the post-move adaptation.
##
##                Specification:
##                  y_it = alpha_i + gamma_t
##                       + sum_tau beta_tau * 1[event_time = tau] * Delta_dest_i
##                       + epsilon_it
##
##                where Delta_dest_i = (LOO destination intensity in
##                year t* in destination HRR_B)
##                                   - (LOO destination intensity in
##                                      year t*-1 in origin HRR_A)
##                t* = move year for physician i.
##                beta_tau traces the share of Delta_dest reflected in
##                physician i's cath rate at event time tau.

# 1. Build merged panel -------------------------------------------------

analysis  <- read_csv("data/output/analysis_panel.csv",
                      col_types = cols(npi = col_character(),
                                       year = col_integer(),
                                       .default = col_guess()))

panel <- analysis %>%
  filter(!is.na(hrr_practice),
         !is.na(intensity_dest_loo),
         !is.nan(intensity_dest_loo),
         !is.na(mean_resid_cath))

cat("Starting panel:", nrow(panel), "rows,",
    n_distinct(panel$npi), "cardiologists\n")


# 2. Identify mid-career movers + move year -----------------------------

# A "mover" here = cardiologist with >=2 distinct hrr_practice values.
# move_year = first year hrr_practice differs from the cardiologist's
# initial hrr_practice in our panel.
mov_info <- panel %>%
  arrange(npi, year) %>%
  group_by(npi) %>%
  mutate(initial_hrr = first(hrr_practice),
         is_post     = hrr_practice != initial_hrr) %>%
  summarize(n_distinct_hrr = n_distinct(hrr_practice),
            n_years        = n(),
            move_year      = if (any(is_post)) min(year[is_post]) else NA_integer_,
            initial_hrr    = first(initial_hrr),
            destination_hrr = if (any(is_post)) hrr_practice[which(is_post)[1]] else NA_integer_,
            .groups = "drop") %>%
  filter(n_distinct_hrr >= 2, !is.na(move_year))

cat("Mid-career movers identified:", nrow(mov_info), "\n")
cat("  with move year in 2010-2017 (>=2 pre/post window):",
    sum(mov_info$move_year >= 2010 & mov_info$move_year <= 2017), "\n")


# 3. Compute Delta_dest (jump in peer intensity at move) ----------------

# For each mover, get LOO destination intensity in (origin HRR, year before
# move) and (destination HRR, move year).
delta_calc <- mov_info %>%
  rowwise() %>%
  mutate(
    pre_loo  = mean(panel$intensity_dest_loo[
                      panel$hrr_practice == initial_hrr &
                      panel$year         == move_year - 1L], na.rm = TRUE),
    post_loo = mean(panel$intensity_dest_loo[
                      panel$hrr_practice == destination_hrr &
                      panel$year         == move_year], na.rm = TRUE),
    delta_dest = post_loo - pre_loo
  ) %>%
  ungroup() %>%
  filter(!is.na(delta_dest), is.finite(delta_dest))

cat("\nMovers with computable Delta_dest:", nrow(delta_calc), "\n")
cat("Distribution of Delta_dest:\n");  print(summary(delta_calc$delta_dest))


# 4. Build event-study panel --------------------------------------------

# Bring move_year and Delta_dest onto the panel
es_panel <- panel %>%
  inner_join(delta_calc %>% select(npi, move_year, delta_dest),
             by = "npi") %>%
  mutate(event_time = year - move_year)

# Restrict to event-time window. With ~10-year panel, most movers have
# at most 4-5 years on either side of the move.
window <- -3:5
es_panel <- es_panel %>% filter(event_time %in% window)

cat("\nEvent-study sample:\n")
cat("  rows:        ", nrow(es_panel), "\n")
cat("  cardiologists:", n_distinct(es_panel$npi), "\n")
cat("  by event time:\n"); print(table(es_panel$event_time))


# 5. Event-study regression --------------------------------------------

# Reference: event_time = -1 (year before move). Each event-time-x-Delta
# interaction recovers the share of Delta_dest reflected in cath rate at
# that event time, relative to t=-1.
es_panel <- es_panel %>%
  mutate(et = factor(event_time, levels = window))

m_es <- feols(mean_resid_cath ~ i(et, delta_dest, ref = "-1") |
                npi + year,
              data = es_panel, weights = ~n_nstemi,
              cluster = ~npi)

cat("\n=== Event-study regression ===\n")
print(summary(m_es))


# 6. Extract coefficients and plot --------------------------------------

td <- tidy(m_es)

es_coef <- td %>%
  filter(grepl("^et::", term)) %>%
  mutate(event_time = as.integer(gsub("et::([-]?\\d+):.*", "\\1", term))) %>%
  bind_rows(tibble(term = "et::-1:delta_dest", estimate = 0,
                   std.error = 0, statistic = 0, p.value = 1,
                   event_time = -1L)) %>%
  arrange(event_time) %>%
  mutate(lo95 = estimate - 1.96 * std.error,
         hi95 = estimate + 1.96 * std.error)

cat("\nEvent-time coefficients (share of Delta_dest reflected in own cath):\n")
print(es_coef %>% select(event_time, estimate, std.error, p.value))

p_es <- ggplot(es_coef, aes(x = event_time, y = estimate)) +
  geom_ribbon(aes(ymin = lo95, ymax = hi95), alpha = 0.25, fill = "gray70") +
  geom_line(color = "gray25", linewidth = 1) +
  geom_point(size = 2.5, color = "gray25") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "gray40") +
  scale_x_continuous(breaks = window) +
  labs(x = "Event time (years from move)",
       y = expression(hat(beta)[tau])) +
  theme_minimal()

ggsave("results/figures/event-study.png", p_es,
       width = 7, height = 4.5, dpi = 300)
cat("\nWrote results/figures/event-study.png\n")


# 7. Pooled post-move adaptation rate -----------------------------------

# Single coefficient: pre-period (event_time < 0) vs post-period (>= 0).
es_panel <- es_panel %>% mutate(post = as.integer(event_time >= 0))

m_pooled <- feols(mean_resid_cath ~ delta_dest:post | npi + year,
                  data = es_panel, weights = ~n_nstemi,
                  cluster = ~npi)
cat("\n=== Pooled pre/post ===\n")
print(summary(m_pooled))

write_csv(es_coef, "results/tables/event-study-coefs.csv")
cat("\nWrote results/tables/event-study-coefs.csv\n")


# 8. Asymmetric direction-of-move ----------------------------------------

# Split movers by sign of Delta_dest:
#   up_mover  : moved to higher-cath environment (Delta > 0)
#   down_mover: moved to lower-cath environment  (Delta < 0)
#
# For each subsample, run the same event-study regression. beta_tau is
# the share of Delta_dest reflected in own cath at event time tau.
# Asymmetric malleability => beta_tau larger for up-movers (low-trained
# adapt) than down-movers (high-trained resist).
# Regression-to-mean / ceiling effects => roughly symmetric beta_tau.

es_panel <- es_panel %>% mutate(direction = if_else(delta_dest > 0, "up", "down"))

cat("\n=== Asymmetric direction-of-move ===\n")
cat("Up-movers (Delta > 0):  ", n_distinct(es_panel$npi[es_panel$direction == "up"]),   "cardiologists\n")
cat("Down-movers (Delta < 0):", n_distinct(es_panel$npi[es_panel$direction == "down"]), "cardiologists\n")

m_es_up   <- feols(mean_resid_cath ~ i(et, delta_dest, ref = "-1") |
                     npi + year,
                   data = es_panel %>% filter(direction == "up"),
                   weights = ~n_nstemi, cluster = ~npi)
m_es_down <- feols(mean_resid_cath ~ i(et, delta_dest, ref = "-1") |
                     npi + year,
                   data = es_panel %>% filter(direction == "down"),
                   weights = ~n_nstemi, cluster = ~npi)

cat("\n--- Up-movers: event-study coefficients ---\n");   print(summary(m_es_up))
cat("\n--- Down-movers: event-study coefficients ---\n"); print(summary(m_es_down))

# Pooled pre/post within each direction
m_pool_up   <- feols(mean_resid_cath ~ delta_dest:post | npi + year,
                     data = es_panel %>% filter(direction == "up"),
                     weights = ~n_nstemi, cluster = ~npi)
m_pool_down <- feols(mean_resid_cath ~ delta_dest:post | npi + year,
                     data = es_panel %>% filter(direction == "down"),
                     weights = ~n_nstemi, cluster = ~npi)

cat("\n--- Pooled pre/post: up-movers ---\n");    print(summary(m_pool_up))
cat("\n--- Pooled pre/post: down-movers ---\n");  print(summary(m_pool_down))


# Combined plot: trajectories for up vs down movers
extract_curve <- function(model, label) {
  td <- tidy(model)
  td %>%
    filter(grepl("^et::", term)) %>%
    mutate(event_time = as.integer(gsub("et::([-]?\\d+):.*", "\\1", term)),
           direction = label) %>%
    bind_rows(tibble(term = "et::-1:delta_dest", estimate = 0,
                     std.error = 0, statistic = 0, p.value = 1,
                     event_time = -1L, direction = label)) %>%
    arrange(event_time) %>%
    mutate(lo95 = estimate - 1.96 * std.error,
           hi95 = estimate + 1.96 * std.error)
}

curves <- bind_rows(
  extract_curve(m_es_up,   "Up-mover (Δ > 0)"),
  extract_curve(m_es_down, "Down-mover (Δ < 0)")
)

write_csv(curves, "results/tables/event-study-coefs-by-direction.csv")
cat("\nWrote results/tables/event-study-coefs-by-direction.csv\n")

p_es_dir <- ggplot(curves, aes(x = event_time, y = estimate,
                                color = direction, fill = direction,
                                linetype = direction, shape = direction)) +
  geom_ribbon(aes(ymin = lo95, ymax = hi95), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "gray40") +
  scale_x_continuous(breaks = window) +
  scale_color_manual(values = c("Up-mover (Δ > 0)"   = "gray20",
                                "Down-mover (Δ < 0)" = "gray60")) +
  scale_fill_manual( values = c("Up-mover (Δ > 0)"   = "gray20",
                                "Down-mover (Δ < 0)" = "gray60")) +
  scale_linetype_manual(values = c("Up-mover (Δ > 0)"   = "solid",
                                   "Down-mover (Δ < 0)" = "dashed")) +
  scale_shape_manual(values = c("Up-mover (Δ > 0)"   = 16,
                                "Down-mover (Δ < 0)" = 17)) +
  labs(x = "Event time (years from move)",
       y = expression(hat(beta)[tau]),
       color = NULL, fill = NULL, linetype = NULL, shape = NULL) +
  theme_minimal()

ggsave("results/figures/event-study-by-direction.png", p_es_dir,
       width = 7.5, height = 4.5, dpi = 300)
cat("\nWrote results/figures/event-study-by-direction.png\n")


# 9. Two-peer-set test: deviation from peers at origin vs destination ----

# Each mid-career mover gives us TWO peer comparisons for the same
# physician: pre-move at origin, post-move at destination. If training
# imprint is durable, the cardiologist's deviation from peers should
# track their training cath share -- in BOTH environments. If they fully
# adapt to current peers, the deviation should be ~0 regardless of
# training.
#
# We need each mover's training cath share. Bring in via the same
# crosswalk used in 8_aha_training.R.

aha_hosp_for_dev  <- read_csv("data/input/aha_hospital.csv",
                              show_col_types = FALSE,
                              col_types = cols(HRRCODE = col_integer(),
                                               year = col_integer(),
                                               CCLABHOS = col_character(),
                                               .default = col_guess()))
aha_hrr_for_dev <- aha_hosp_for_dev %>%
  filter(!is.na(HRRCODE), !is.na(year)) %>%
  mutate(has_cath = as.integer(CCLABHOS == "1")) %>%
  group_by(HRRCODE, year) %>%
  summarize(cath_lab_share = mean(has_cath, na.rm = TRUE), .groups = "drop") %>%
  rename(hrr = HRRCODE) %>%
  mutate(cath_lab_share = if_else(is.nan(cath_lab_share), NA_real_,
                                  cath_lab_share))

# Bring training cath share onto each mover-year, year-matched as before
es_panel_dev <- es_panel %>%
  mutate(med_school_start = grad_year - 3L,
         aha_match_year   = pmin(pmax(med_school_start, 1980L), 2003L)) %>%
  left_join(aha_hrr_for_dev %>% rename(train_cath_lab = cath_lab_share),
            by = c("hrr_med_school" = "hrr",
                   "aha_match_year"  = "year"))

# Compute per-physician origin and destination averages
phase_panel <- es_panel_dev %>%
  filter(!is.na(train_cath_lab)) %>%
  mutate(phase = case_when(
    event_time <= -1            ~ "origin",
    event_time >= 2             ~ "destination",   # long-run post-move
    TRUE                        ~ NA_character_
  )) %>%
  filter(!is.na(phase))

mover_dev <- phase_panel %>%
  group_by(npi, phase) %>%
  summarize(
    own_cath       = weighted.mean(mean_resid_cath,    n_nstemi, na.rm = TRUE),
    peer_cath      = weighted.mean(intensity_dest_loo, n_nstemi, na.rm = TRUE),
    deviation      = own_cath - peer_cath,
    train_cath_lab = first(train_cath_lab),
    n_years        = n(),
    .groups = "drop"
  )

cat("\n=== Two-peer-set deviation analysis ===\n")
cat("Physicians with both phases:",
    mover_dev %>% group_by(npi) %>% filter(n_distinct(phase) == 2) %>%
      pull(npi) %>% n_distinct(), "\n")

# Two-phase movers only (so we can compare within-physician)
two_phase <- mover_dev %>%
  group_by(npi) %>%
  filter(n_distinct(phase) == 2) %>%
  ungroup()

# Regression: deviation_phase on training cath share, separately by phase
m_dev_orig <- feols(deviation ~ train_cath_lab,
                    data = two_phase %>% filter(phase == "origin"))
m_dev_dest <- feols(deviation ~ train_cath_lab,
                    data = two_phase %>% filter(phase == "destination"))

cat("\n--- Deviation at origin ~ training cath share ---\n")
print(summary(m_dev_orig))
cat("\n--- Deviation at destination ~ training cath share ---\n")
print(summary(m_dev_dest))

# Within-physician diff: does the same physician's deviation change
# between origin and destination?
two_phase_wide <- two_phase %>%
  select(npi, phase, deviation, train_cath_lab) %>%
  pivot_wider(names_from = phase, values_from = deviation) %>%
  mutate(diff = destination - origin)

cat("\n--- Within-physician change in deviation (destination - origin) ~ training ---\n")
m_diff <- feols(diff ~ train_cath_lab, data = two_phase_wide)
print(summary(m_diff))

# Plot: deviation vs training cath share, separately by phase
p_dev <- ggplot(two_phase, aes(x = train_cath_lab, y = deviation,
                                color = phase, fill = phase,
                                linetype = phase, shape = phase)) +
  geom_smooth(method = "lm", alpha = 0.2) +
  geom_point(alpha = 0.3, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("origin" = "gray20",
                                "destination" = "gray60")) +
  scale_fill_manual( values = c("origin" = "gray20",
                                "destination" = "gray60")) +
  scale_linetype_manual(values = c("origin" = "solid",
                                   "destination" = "dashed")) +
  scale_shape_manual(values = c("origin" = 16, "destination" = 17)) +
  labs(x = "Training-period cath-lab share",
       y = "Deviation from current peer mean",
       color = NULL, fill = NULL, linetype = NULL, shape = NULL) +
  theme_minimal()

ggsave("results/figures/two-peer-deviation.png", p_dev,
       width = 7.5, height = 4.5, dpi = 300)
cat("\nWrote results/figures/two-peer-deviation.png\n")
