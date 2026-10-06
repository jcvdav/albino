################################################################################
# Site location map (Mexico overview + Yucatan Peninsula inset)
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description: Builds a two-panel map showing the dive/cenote site location:
# a zoomed-out view of Mexico with a bounding box over the Yucatan Peninsula,
# and a zoomed-in panel on the Yucatan Peninsula showing the exact site.
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  sf,
  rnaturalearth,
  ggspatial,
  cowplot,
  tidyverse
)

# Define site location ----------------------------------------------------------
site <- tibble(
  site = "Cenote\nOrquidea",
  lon = -87.33547,
  lat = 20.48147
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Define bounding box for Yucatan overview (used for the small inset) -----------
yucatan_bbox <- st_bbox(c(xmin = -91, xmax = -86.5, ymin = 18, ymax = 22.3),
                         crs = st_crs(4326))

## PROCESSING ##################################################################

# Load basemap data --------------------------------------------------------------
mexico <- ne_states(country = "Mexico", returnclass = "sf")
# Exclude Mexico here; it's drawn separately (at state level) to avoid
# stacking two slightly misaligned Mexico outlines, which produced rendering
# seams/holes along the Baja California coastline.
countries <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name != "Mexico")

# Define shared "modern" basemap palette -----------------------------------------
ocean_color <- "white"
land_color <- "grey40"
border_color <- "white"

# Overview map: Mexico with Yucatan bbox highlighted ------------------------------
map_overview <- ggplot() +
  geom_sf(data = countries, fill = land_color, color = border_color, linewidth = 0.15) +
  geom_sf(data = mexico, fill = land_color, color = border_color, linewidth = 0.15) +
  geom_sf(data = st_as_sfc(yucatan_bbox), fill = NA, color = "red", linewidth = 0.6) +
  coord_sf(xlim = c(-118, -86), ylim = c(14, 33), expand = FALSE) +
  theme_void() +
  theme(panel.background = element_rect(fill = "white", color = NA),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

# Inset map: zoomed in on the site --------------------------------------------
map_inset <- ggplot() +
  geom_sf(data = st_crop(mexico, yucatan_bbox), fill = land_color, color = border_color, linewidth = 0.3) +
  geom_sf(data = site, color = "red", size = 3) +
  geom_sf_text(data = site, aes(label = site),
               hjust = 1.3, vjust = 1, size = 4, color = "black") +
  coord_sf(xlim = c(yucatan_bbox["xmin"], yucatan_bbox["xmax"]),
           ylim = c(yucatan_bbox["ymin"], yucatan_bbox["ymax"]),
           expand = FALSE) +
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                          height = unit(0.8, "cm"), width = unit(0.8, "cm")) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(panel.background = element_rect(fill = ocean_color, color = NA),
        panel.border = element_blank(),
        panel.grid = element_blank())

# Combine with overview as inset in top-left of main panel ------------------------
site_map <- ggdraw() +
  draw_plot(map_inset) +
  draw_plot(map_overview, x = 0.21, y = 0.78, width = 0.2, height = 0.2)

## EXPORT ######################################################################

# Save map -----------------------------------------------------------------------
ggsave(here("data", "site_map.png"), site_map, width = 8.5, height = 6, dpi = 300)
