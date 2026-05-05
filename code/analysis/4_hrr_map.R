# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-05-05
## Description:   Static HRR choropleth of cardiologist cath intensity.
##                Adapted from Shirley's (interactive leaflet) hrr_map.R.
##                Output: results/figures/hrr-cath-intensity.png

source("code/0-setup.R")
library(sf)


# 1. Load data ------------------------------------------------------------

analysis <- read_csv("data/output/analysis_panel.csv",
                     col_types = cols(npi = col_character(),
                                      year = col_integer(),
                                      .default = col_guess()))

# HRR-year volume-weighted mean residualized cath rate
hrr_year <- analysis %>%
  filter(!is.na(hrr_practice)) %>%
  group_by(hrr_practice, year) %>%
  summarise(intensity = weighted.mean(mean_resid_cath, n_nstemi,
                                      na.rm = TRUE),
            n_cardio  = n_distinct(npi),
            .groups   = "drop")

# Pooled across years (one number per HRR for the static map)
hrr_pool <- analysis %>%
  filter(!is.na(hrr_practice)) %>%
  group_by(hrr_practice) %>%
  summarise(intensity = weighted.mean(mean_resid_cath, n_nstemi,
                                      na.rm = TRUE),
            n_cardio  = n_distinct(npi),
            .groups   = "drop")


# 2. HRR shapefile --------------------------------------------------------

hrr_sf <- st_read("data/input/hrr-shapefile/HRR_Bdry.SHP", quiet = TRUE) %>%
  rename_with(tolower)

# Drop AK + HI for a contiguous-US map
hrr_sf <- hrr_sf %>%
  mutate(state2 = substr(hrrcity, 1, 2)) %>%
  filter(!state2 %in% c("AK", "HI"))


# 3. Join and plot --------------------------------------------------------

map_df <- hrr_sf %>%
  left_join(hrr_pool, by = c("hrrnum" = "hrr_practice"))

p_map <- ggplot(map_df) +
  geom_sf(aes(fill = intensity), color = "gray60", linewidth = 0.1) +
  scale_fill_distiller(palette = "Reds", direction = 1,
                       na.value = "gray90",
                       name = "Mean residualized\ncath rate") +
  coord_sf(crs = 5070) +   # Albers equal area for the lower 48
  theme_void() +
  theme(legend.position = "right")

ggsave("results/figures/hrr-cath-intensity.png", p_map,
       width = 8, height = 5, dpi = 200, bg = "white")

cat("Wrote results/figures/hrr-cath-intensity.png\n")
cat("HRRs with intensity data:",
    sum(!is.na(map_df$intensity)), "of", nrow(map_df), "\n")
