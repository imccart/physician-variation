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


# 2b. Full summary statistics table (cardiologist sample) ----------------

# Cardiologist-level (collapse panel to one row per NPI, first observation
# for time-invariant fields, mean for time-varying)
cardio_summ <- analysis %>%
  arrange(npi, year) %>%
  group_by(npi) %>%
  summarize(
    grad_year             = first(grad_year),
    gender                = first(gender),
    specialty             = first(specialty),
    intensity_med_school  = first(intensity_med_school),
    n_years               = n(),
    .groups = "drop"
  )

# Helper: format mean (sd) [min, max] for a numeric column
fmt_summ <- function(x, digits = 3) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(c(mean = " ", sd = " ", min = " ", max = " ", n = "0"))
  tibble(
    mean = sprintf(paste0("%.", digits, "f"), mean(x)),
    sd   = sprintf(paste0("%.", digits, "f"), sd(x)),
    min  = sprintf(paste0("%.", digits, "f"), min(x)),
    max  = sprintf(paste0("%.", digits, "f"), max(x)),
    n    = format(length(x), big.mark = ",")
  )
}

# Cardiologist-year level rows
cy_rows <- list(
  list(label = "Residualized cath rate",
       vals  = fmt_summ(analysis$mean_resid_cath, 3)),
  list(label = "NSTEMI volume per year",
       vals  = fmt_summ(analysis$n_nstemi, 1)),
  list(label = "Med school HRR intensity",
       vals  = fmt_summ(analysis$intensity_med_school, 3)),
  list(label = "Destination HRR LOO intensity",
       vals  = fmt_summ(analysis$intensity_dest_loo, 3)),
  list(label = "$\\Delta$ intensity (dest $-$ med school)",
       vals  = fmt_summ(analysis$intensity_change, 3))
)

# Cardiologist-level rows (one row per NPI)
c_rows <- list(
  list(label = "Graduation year",
       vals  = fmt_summ(cardio_summ$grad_year, 1)),
  list(label = "Years observed in panel",
       vals  = fmt_summ(cardio_summ$n_years, 1))
)

# Cardiologist-level binary/share rows
g_pct  <- mean(cardio_summ$gender == "F", na.rm = TRUE)
sp_gen <- mean(cardio_summ$specialty == "Cardiology", na.rm = TRUE)
sp_ic  <- mean(cardio_summ$specialty == "Interventional Cardiology", na.rm = TRUE)
sp_ep  <- mean(cardio_summ$specialty == "Clinical Cardiac Electrophysiology", na.rm = TRUE)
sp_hf  <- mean(cardio_summ$specialty == "Advanced Heart Failure and Transplant Cardiology", na.rm = TRUE)
n_card <- format(nrow(cardio_summ), big.mark = ",")

share_row <- function(label, share) {
  tibble(label = label,
         mean  = sprintf("%.3f", share),
         sd    = " ", min = " ", max = " ",
         n     = n_card)
}

# Build table body
build_section <- function(rows) {
  map_dfr(rows, function(r) tibble(label = r$label,
                                   mean = r$vals$mean,
                                   sd   = r$vals$sd,
                                   min  = r$vals$min,
                                   max  = r$vals$max,
                                   n    = r$vals$n))
}

cy_section <- build_section(cy_rows)
c_section  <- build_section(c_rows)
share_section <- bind_rows(
  share_row("Female",                    g_pct),
  share_row("General Cardiology",        sp_gen),
  share_row("Interventional Cardiology", sp_ic),
  share_row("Electrophysiology",         sp_ep),
  share_row("Adv.\\ Heart Failure",      sp_hf)
)

summ_table <- bind_rows(cy_section, c_section, share_section)
n_cy <- nrow(cy_section)
n_c  <- nrow(c_section)
n_sh <- nrow(share_section)

kable(summ_table,
      format    = "latex",
      booktabs  = TRUE,
      linesep   = "",
      escape    = FALSE,
      align     = c("l", rep("c", 5)),
      col.names = c("", "Mean", "SD", "Min", "Max", "N")) %>%
  pack_rows("Cardiologist-year level",       1,           n_cy,             italic = TRUE, bold = FALSE) %>%
  pack_rows("Cardiologist level (per NPI)",  n_cy + 1,    n_cy + n_c + n_sh, italic = TRUE, bold = FALSE) %>%
  save_kable("results/tables/summary-stats.tex")


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


# 7. Balance table: movers vs stayers ------------------------------------

# Collapse panel to one row per cardiologist (first-observation values for
# pre-treatment characteristics, mean of time-varying ones)
cardio_balance <- analysis %>%
  filter(!is.na(mover)) %>%
  arrange(npi, year) %>%
  group_by(npi) %>%
  summarize(
    mover                = first(mover),
    grad_year            = first(grad_year),
    gender               = first(gender),
    specialty            = first(specialty),
    intensity_med_school = first(intensity_med_school),
    n_years              = n(),
    mean_volume          = mean(n_nstemi),
    first_year           = min(year),
    .groups = "drop"
  )

balance_means <- cardio_balance %>%
  group_by(mover) %>%
  summarize(
    n           = n(),
    grad_year   = mean(grad_year, na.rm = TRUE),
    pct_female  = mean(gender == "F", na.rm = TRUE),
    pct_general = mean(specialty == "Cardiology", na.rm = TRUE),
    pct_ic      = mean(specialty == "Interventional Cardiology", na.rm = TRUE),
    pct_ep      = mean(specialty == "Clinical Cardiac Electrophysiology", na.rm = TRUE),
    pct_hf      = mean(specialty == "Advanced Heart Failure and Transplant Cardiology", na.rm = TRUE),
    train_int   = mean(intensity_med_school, na.rm = TRUE),
    n_years     = mean(n_years),
    volume      = mean(mean_volume),
    first_year  = mean(first_year),
    .groups = "drop"
  )

# Two-sample tests against stayer mean
test_diff <- function(var, binary = FALSE) {
  x <- cardio_balance[[var]]
  g <- cardio_balance$mover
  if (binary) {
    res <- t.test(as.numeric(x[g == 1]), as.numeric(x[g == 0]))
  } else {
    res <- t.test(x[g == 1], x[g == 0])
  }
  list(diff = unname(res$estimate[1] - res$estimate[2]),
       p    = res$p.value)
}

vars <- list(
  list(label = "Graduation year",      var = "grad_year",   binary = FALSE),
  list(label = "Female (\\%)",         var = "gender",      binary = TRUE),
  list(label = "General Cardiology (\\%)",         var = "specialty", binary = "general"),
  list(label = "Interventional Cardiology (\\%)",  var = "specialty", binary = "ic"),
  list(label = "Electrophysiology (\\%)",          var = "specialty", binary = "ep"),
  list(label = "Adv.\\ Heart Failure (\\%)",       var = "specialty", binary = "hf"),
  list(label = "Med school HRR intensity",         var = "intensity_med_school", binary = FALSE),
  list(label = "Years in panel",                   var = "n_years", binary = FALSE),
  list(label = "Mean NSTEMI volume per year",      var = "mean_volume", binary = FALSE),
  list(label = "First observation year",           var = "first_year", binary = FALSE)
)

# Build mover/stayer/diff/p columns
balance_rows <- map(vars, function(v) {
  if (isTRUE(v$binary == "general")) {
    x <- as.numeric(cardio_balance$specialty == "Cardiology")
  } else if (isTRUE(v$binary == "ic")) {
    x <- as.numeric(cardio_balance$specialty == "Interventional Cardiology")
  } else if (isTRUE(v$binary == "ep")) {
    x <- as.numeric(cardio_balance$specialty == "Clinical Cardiac Electrophysiology")
  } else if (isTRUE(v$binary == "hf")) {
    x <- as.numeric(cardio_balance$specialty == "Advanced Heart Failure and Transplant Cardiology")
  } else if (isTRUE(v$binary)) {
    x <- as.numeric(cardio_balance[[v$var]] == "F")
  } else {
    x <- cardio_balance[[v$var]]
  }
  g <- cardio_balance$mover
  m_mover  <- mean(x[g == 1], na.rm = TRUE)
  m_stayer <- mean(x[g == 0], na.rm = TRUE)
  res <- t.test(x[g == 1], x[g == 0])
  is_pct <- grepl("\\\\%", v$label)
  scale <- if (is_pct) 100 else 1
  digits <- if (grepl("year", v$label, ignore.case = TRUE)) 1 else if (is_pct) 1 else 3
  fmt <- function(z) sprintf(paste0("%.", digits, "f"), z * scale)
  tibble(Variable = v$label,
         Movers   = fmt(m_mover),
         Stayers  = fmt(m_stayer),
         Diff     = fmt(m_mover - m_stayer),
         `p-value` = sprintf("%.3f", res$p.value))
})

balance_body <- bind_rows(balance_rows)

balance_footer <- tibble(
  Variable = "Cardiologists",
  Movers   = format(sum(cardio_balance$mover == 1), big.mark = ","),
  Stayers  = format(sum(cardio_balance$mover == 0), big.mark = ","),
  Diff     = "",
  `p-value` = ""
)

balance_table <- bind_rows(balance_body, balance_footer)

kable(balance_table,
      format    = "latex",
      booktabs  = TRUE,
      linesep   = "",
      escape    = FALSE,
      align     = c("l", rep("c", 4)),
      col.names = c("", "Movers", "Stayers", "Diff.", "$p$-value")) %>%
  row_spec(nrow(balance_table) - 1, extra_latex_after = "\\midrule") %>%
  save_kable("results/tables/balance.tex")

write_csv(balance_table, "results/tables/balance.csv")


# 8. Origin-destination matrix: "movers go everywhere" --------------------

# Per-cardiologist (first obs) origin and destination HRRs for movers
od_data <- analysis %>%
  filter(mover == 1, !is.na(hrr_med_school), !is.na(hrr_practice)) %>%
  arrange(npi, year) %>%
  group_by(npi) %>%
  summarize(origin = first(hrr_med_school),
            dest   = first(hrr_practice),
            .groups = "drop")

# Destination dispersion per origin: how concentrated are destinations?
# Herfindahl on destination shares within each origin HRR.
od_dispersion <- od_data %>%
  group_by(origin) %>%
  summarize(
    n_movers      = n(),
    n_dest_unique = n_distinct(dest),
    hhi_dest      = sum((table(dest) / n())^2),
    .groups = "drop"
  ) %>%
  filter(n_movers >= 5)  # avoid tiny-origin noise

cat("\nOrigin-destination dispersion (origins with >=5 movers):\n")
cat("  Origins:                ", nrow(od_dispersion), "\n")
cat("  Median destinations/origin: ", median(od_dispersion$n_dest_unique), "\n")
cat("  Median HHI:              ", round(median(od_dispersion$hhi_dest), 3), "\n")
cat("  (HHI=1 -> all to one dest; HHI~1/n_dest -> evenly spread)\n")

# Save the per-origin summary
write_csv(od_dispersion, "results/tables/origin-dispersion.csv")

# Heatmap of mover counts (limit to top origins/destinations to be readable)
top_origins <- od_data %>% count(origin) %>% slice_max(n, n = 25) %>% pull(origin)
top_dests   <- od_data %>% count(dest)   %>% slice_max(n, n = 25) %>% pull(dest)

od_counts <- od_data %>%
  filter(origin %in% top_origins, dest %in% top_dests) %>%
  count(origin, dest) %>%
  mutate(origin = factor(origin, levels = top_origins),
         dest   = factor(dest,   levels = top_dests))

p_od <- ggplot(od_counts, aes(x = dest, y = origin, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#F0F0F0", high = "#1F2D5C", trans = "log10") +
  labs(x = "Destination HRR (top 25)",
       y = "Origin HRR (top 25)",
       fill = "Movers",
       title = "Origin-destination flows for cardiologist movers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.text.y = element_text(size = 7))

ggsave("results/figures/od-heatmap.png", p_od,
       width = 8, height = 6, dpi = 300)

# Distribution of HHI across origins (the actual diagnostic)
p_hhi <- ggplot(od_dispersion, aes(x = hhi_dest)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  geom_vline(xintercept = median(od_dispersion$hhi_dest),
             linetype = "dashed", color = "firebrick") +
  labs(x = "Destination HHI within origin HRR",
       y = "Number of origin HRRs",
       title = "Movers do not concentrate on one destination") +
  theme_minimal()

ggsave("results/figures/od-hhi.png", p_hhi,
       width = 8, height = 6, dpi = 300)
