# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-05-07
## Description:   Balance / selection assessment for mid-career movers.
##                Two questions:
##                  1. Are mid-career movers different from non-movers on
##                     observables (training cath share, gender, cohort,
##                     subspecialty, volume, panel years)?
##                  2. Among movers, is there sorting by training intensity
##                     into destination types? (e.g., do high-trained
##                     movers go to even-higher-cath destinations?)

# 1. Build merged panel -------------------------------------------------

analysis  <- read_csv("data/output/analysis_panel.csv",
                      col_types = cols(npi = col_character(),
                                       year = col_integer(),
                                       .default = col_guess()))
aha_hosp  <- read_csv("data/input/aha_hospital.csv", show_col_types = FALSE,
                      col_types = cols(HRRCODE = col_integer(),
                                       year = col_integer(),
                                       CCLABHOS = col_character(),
                                       .default = col_guess()))

# CCLABHOS (cath lab): 1980-2003 -- used as the training-period exposure
# measure when we year-match to the cardiologist's med-school start.
aha_hrr <- aha_hosp %>%
  filter(!is.na(HRRCODE), !is.na(year)) %>%
  mutate(has_cath = as.integer(CCLABHOS == "1")) %>%
  group_by(HRRCODE, year) %>%
  summarize(cath_lab_share = mean(has_cath, na.rm = TRUE),
            .groups = "drop") %>%
  rename(hrr = HRRCODE) %>%
  mutate(cath_lab_share = if_else(is.nan(cath_lab_share), NA_real_,
                                  cath_lab_share))

panel <- analysis %>%
  mutate(med_school_start = grad_year - 3L,
         aha_match_year   = pmin(pmax(med_school_start, 1980L), 2003L)) %>%
  left_join(aha_hrr %>% rename(train_cath_lab = cath_lab_share),
            by = c("hrr_med_school" = "hrr",
                   "aha_match_year"  = "year"))

# Identify mid-career movers: cardiologists with >=2 distinct hrr_practice
# values during 2008-2018 panel. We focus on cardiologists in the clean
# grad-year window (1983-2006) for direct comparability with the AHA
# training imprint analysis.
phys <- panel %>%
  filter(grad_year >= 1983, grad_year <= 2006,
         !is.na(mean_resid_cath),
         !is.na(hrr_practice)) %>%
  group_by(npi) %>%
  summarize(
    is_mover         = as.integer(n_distinct(hrr_practice) >= 2),
    n_distinct_hrr   = n_distinct(hrr_practice),
    n_years_in_panel = n(),
    train_cath_lab   = first(train_cath_lab),
    grad_year        = first(grad_year),
    gender           = first(gender),
    specialty        = first(specialty),
    mean_volume      = mean(n_nstemi),
    initial_hrr      = first(hrr_practice),
    .groups = "drop"
  )

cat("Sample for balance test:\n")
cat("  total cardiologists:", nrow(phys), "\n")
cat("  mid-career movers:  ", sum(phys$is_mover), "\n")
cat("  non-movers:         ", sum(!phys$is_mover), "\n\n")


# 2. Balance: movers vs non-movers --------------------------------------

balance_vars <- list(
  list(label = "Training cath share",         var = "train_cath_lab", binary = FALSE),
  list(label = "Graduation year",             var = "grad_year",      binary = FALSE),
  list(label = "Female (\\%)",                var = "gender",         binary = "F"),
  list(label = "General Cardiology (\\%)",    var = "specialty",
       binary = "Cardiology"),
  list(label = "Interventional (\\%)",        var = "specialty",
       binary = "Interventional Cardiology"),
  list(label = "Electrophysiology (\\%)",     var = "specialty",
       binary = "Clinical Cardiac Electrophysiology"),
  list(label = "Adv.\\ Heart Failure (\\%)",  var = "specialty",
       binary = "Advanced Heart Failure and Transplant Cardiology"),
  list(label = "Years observed in panel",     var = "n_years_in_panel", binary = FALSE),
  list(label = "Mean NSTEMI volume / year",   var = "mean_volume",    binary = FALSE)
)

build_row <- function(v) {
  if (isTRUE(v$binary == FALSE)) {
    x <- phys[[v$var]]
    is_pct <- FALSE
  } else {
    x <- as.numeric(phys[[v$var]] == v$binary)
    is_pct <- TRUE
  }
  res <- t.test(x[phys$is_mover == 1], x[phys$is_mover == 0])
  scale  <- if (is_pct) 100 else 1
  digits <- if (grepl("year", v$label, ignore.case = TRUE)) 1
            else if (is_pct) 1
            else 3
  fmt <- function(z) sprintf(paste0("%.", digits, "f"), z * scale)
  tibble(
    Variable = v$label,
    Movers    = fmt(mean(x[phys$is_mover == 1], na.rm = TRUE)),
    `Non-movers` = fmt(mean(x[phys$is_mover == 0], na.rm = TRUE)),
    Diff      = fmt(mean(x[phys$is_mover == 1], na.rm = TRUE) -
                    mean(x[phys$is_mover == 0], na.rm = TRUE)),
    `p-value` = sprintf("%.3f", res$p.value)
  )
}

balance <- map_dfr(balance_vars, build_row)

footer <- tibble(
  Variable = "Cardiologists",
  Movers    = format(sum(phys$is_mover == 1), big.mark = ","),
  `Non-movers` = format(sum(phys$is_mover == 0), big.mark = ","),
  Diff      = "",
  `p-value` = ""
)

balance_table <- bind_rows(balance, footer)

cat("=== Balance: mid-career movers vs non-movers ===\n")
print(balance_table)

write_csv(balance_table, "results/tables/mover-balance.csv")

# Build LaTeX by hand to avoid kableExtra's phantom blank line between
# `extra_latex_after = "\\midrule"` and the next row.
body_rows_mb <- apply(balance, 1, function(r) {
  paste0(r[1], " & ", r[2], " & ", r[3], " & ", r[4], " & ", r[5], " \\\\\n")
})
footer_row_mb <- paste0(
  "Cardiologists & ",
  format(sum(phys$is_mover == 1), big.mark = ","), " & ",
  format(sum(phys$is_mover == 0), big.mark = ","), " &  &  \\\\\n"
)

tbl_mb <- paste0(
  "\\begin{tabular}{lcccc}\n",
  "\\toprule\n",
  " & Movers & Non-movers & Diff. & $p$-value \\\\\n",
  "\\midrule\n",
  paste(body_rows_mb, collapse = ""),
  "\\midrule\n",
  footer_row_mb,
  "\\bottomrule\n",
  "\\end{tabular}\n"
)
writeLines(tbl_mb, "results/tables/mover-balance.tex")


# 3. Among movers: where do they go relative to training? ---------------

# Build a per-mover record of origin and destination HRR cath share.
mover_panel <- panel %>%
  filter(grad_year >= 1983, grad_year <= 2006,
         !is.na(mean_resid_cath),
         !is.na(hrr_practice),
         !is.na(train_cath_lab)) %>%
  arrange(npi, year) %>%
  group_by(npi) %>%
  mutate(initial_hrr = first(hrr_practice),
         is_post     = hrr_practice != initial_hrr) %>%
  summarize(
    n_distinct_hrr   = n_distinct(hrr_practice),
    train_cath_lab   = first(train_cath_lab),
    initial_hrr      = first(initial_hrr),
    move_year        = if (any(is_post)) min(year[is_post]) else NA_integer_,
    destination_hrr  = if (any(is_post)) hrr_practice[which(is_post)[1]] else NA_integer_,
    .groups = "drop"
  ) %>%
  filter(n_distinct_hrr >= 2, !is.na(move_year))

# Origin and destination cath-culture at the move year, from the within-
# panel residualized peer cath rate (intensity_dest_loo). This is the
# same measure we use as the destination peer environment in the event
# study -- conceptually appropriate here too since the question is what
# cath culture the cardiologist faces at each side of the move.
move_loo <- panel %>%
  filter(grad_year >= 1983, grad_year <= 2006,
         !is.na(intensity_dest_loo), !is.nan(intensity_dest_loo)) %>%
  select(hrr_practice, year, intensity_dest_loo) %>%
  group_by(hrr_practice, year) %>%
  summarize(loo_share = mean(intensity_dest_loo, na.rm = TRUE),
            .groups = "drop")

mover_panel <- mover_panel %>%
  mutate(origin_year = move_year - 1L) %>%
  left_join(move_loo %>% rename(origin_share = loo_share),
            by = c("initial_hrr" = "hrr_practice", "origin_year" = "year")) %>%
  left_join(move_loo %>% rename(dest_share = loo_share),
            by = c("destination_hrr" = "hrr_practice", "move_year" = "year")) %>%
  mutate(delta_dest = dest_share - origin_share)

cat("\n=== Mover destination by training tercile ===\n")
mover_terc <- mover_panel %>%
  filter(!is.na(delta_dest)) %>%
  mutate(train_tercile = ntile(train_cath_lab, 3)) %>%
  group_by(train_tercile) %>%
  summarize(
    n             = n(),
    mean_train    = mean(train_cath_lab,  na.rm = TRUE),
    mean_origin   = mean(origin_share,    na.rm = TRUE),
    mean_dest     = mean(dest_share,      na.rm = TRUE),
    mean_delta    = mean(delta_dest,      na.rm = TRUE),
    pct_up_movers = mean(delta_dest > 0,  na.rm = TRUE),
    .groups = "drop"
  )
print(mover_terc)


# 4. Test for selection on direction by training intensity ---------------

# Are high-trained physicians disproportionately up-movers (going to even
# higher-cath destinations)? If yes, the "selection caveat" is real.
m_select <- feols(as.integer(delta_dest > 0) ~ train_cath_lab,
                  data = mover_panel %>% filter(!is.na(delta_dest)))
cat("\n=== Selection: P(up-mover) ~ training cath share ===\n")
print(summary(m_select))

# Also test: does Delta_dest correlate with training cath share?
m_delta <- feols(delta_dest ~ train_cath_lab,
                 data = mover_panel %>% filter(!is.na(delta_dest)))
cat("\n=== Selection: Delta_dest ~ training cath share ===\n")
print(summary(m_delta))


# 5. Visualization -------------------------------------------------------

p_select <- mover_panel %>%
  filter(!is.na(delta_dest)) %>%
  ggplot(aes(x = train_cath_lab, y = delta_dest)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "firebrick") +
  labs(x = "Training-period cath-lab share",
       y = "Destination cath share - origin cath share",
       title = "Where mid-career movers go, by training intensity",
       subtitle = "Slope > 0 means high-trained movers go to higher-cath destinations.") +
  theme_minimal()

ggsave("results/figures/mover-selection.png", p_select,
       width = 7.5, height = 4.5, dpi = 300)
cat("\nWrote results/figures/mover-selection.png\n")
