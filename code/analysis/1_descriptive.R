# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-04-06
## Description:   Descriptive statistics and figures for mover design.

# 1. Load data ------------------------------------------------------------

analysis <- read_csv("data/output/analysis_panel.csv",
                     col_types = cols(npi = col_character(),
                                      year = col_integer(),
                                      .default = col_guess()))


# 2. Summary statistics table ---------------------------------------------

# Cardiologist-level (collapse panel to cardiologist means)
cardio_level <- analysis %>%
  filter(!is.na(mover)) %>%
  group_by(npi) %>%
  summarize(
    mover = first(mover),
    mean_volume = mean(n_nstemi),
    mean_resid = mean(mean_resid_cath),
    n_years = n(),
    grad_year = first(grad_year),
    gender = first(gender),
    .groups = "drop"
  )

sumstats <- cardio_level %>%
  group_by(mover) %>%
  summarize(
    N = n(),
    Years_Active = mean(n_years),
    Volume = mean(mean_volume),
    Resid_Cath = mean(mean_resid),
    Grad_Year = mean(grad_year, na.rm = TRUE),
    Pct_Female = mean(gender == "F", na.rm = TRUE),
    .groups = "drop"
  )

print(sumstats)
write_csv(sumstats, "results/tables/summary-stats.csv")


# 3. Scatterplot: med school intensity vs practice intensity ---------------

scatter_data <- analysis %>%
  filter(mover == 1, !is.na(intensity_med_school), !is.na(intensity_dest_loo))

p1 <- ggplot(scatter_data, aes(x = intensity_med_school, y = mean_resid_cath)) +
  geom_point(alpha = 0.1, size = 0.5) +
  geom_smooth(method = "lm", color = "firebrick", se = TRUE) +
  labs(x = "Medical School HRR Intensity",
       y = "Cardiologist Residualized Cath Rate",
       title = "Training environment predicts practice intensity") +
  theme_minimal()

ggsave("results/figures/scatter-med-school-vs-practice.png", p1,
       width = 8, height = 6, dpi = 300)


# 4. Binscatter: med school intensity vs practice intensity ----------------

binscatter_data <- scatter_data %>%
  mutate(bin = ntile(intensity_med_school, 20)) %>%
  group_by(bin) %>%
  summarize(
    x = mean(intensity_med_school),
    y = mean(mean_resid_cath),
    .groups = "drop"
  )

p2 <- ggplot(binscatter_data, aes(x = x, y = y)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick") +
  labs(x = "Medical School HRR Intensity (ventile means)",
       y = "Mean Residualized Cath Rate",
       title = "Training environment predicts practice intensity (binscatter)") +
  theme_minimal()

ggsave("results/figures/binscatter-med-school-vs-practice.png", p2,
       width = 8, height = 6, dpi = 300)


# 5. Distribution of intensity change for movers --------------------------

p3 <- analysis %>%
  filter(mover == 1, !is.na(intensity_change)) %>%
  ggplot(aes(x = intensity_change)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Change in Intensity (Destination LOO - Med School)",
       y = "Count",
       title = "Distribution of intensity change for movers") +
  theme_minimal()

ggsave("results/figures/hist-intensity-change.png", p3,
       width = 8, height = 6, dpi = 300)


# 6. Panel volume over time -----------------------------------------------

p4 <- analysis %>%
  group_by(year) %>%
  summarize(
    n_cardiologists = n_distinct(npi),
    n_episodes = sum(n_nstemi),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = year, y = n_cardiologists)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(x = "Year", y = "Number of Cardiologists",
       title = "Panel size over time") +
  theme_minimal()

ggsave("results/figures/panel-size-by-year.png", p4,
       width = 8, height = 6, dpi = 300)
