################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  XML,
  patchwork,
  tidyverse
)

# Define UDFs ------------------------------------------------------------------
process_dive <- function(path){

  dive <- xmlParse(path) %>%
    xmlToList() %>%
    pluck("DiveSamples")

  time <- map_chr(dive, ~pluck(.x, "Time"))
  depth <- map_chr(dive, ~pluck(.x, "Depth"))
  temp <- map_chr(dive, ~pluck(.x, "Temperature"))

  data <- tibble(time = as.numeric(time),
                 depth = as.numeric(depth),
                 temp = as.numeric(temp)) %>%
    #summarise_all(as.numeric) %>%
    mutate(time = time / 60,
           depth = -1 * depth,
           temp = round(temp),
           date = str_extract(path, "[:digit:]{4}-[:digit:]{2}-[:digit:]{2}")) %>%
    rename(time_min = time,
           depth_m = depth,
           temp_c = temp)

  return(data)
}

# Load data --------------------------------------------------------------------
paths <- list.files(here("data", "raw"),
                    pattern = "xml",
                    full.names = T)

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
data <- map_dfr(paths, process_dive) %>%
  mutate(bookmark = ((depth_m == min(depth_m[date == "2024-07-03"])) & (date == "2024-07-03")) |
           ((time_min == 24) & (date == "2024-07-08"))) %>%
  # Number the two bookmarked observations in chronological (date) order
  arrange(date, time_min) %>%
  mutate(obs_num = cumsum(bookmark))

## PLOT ######################################################################

# Define UDF: plot a single dive profile -----------------------------------------
plot_dive_profile <- function(df, color_limits){
  bookmarks <- filter(df, bookmark) %>%
    # Nudge labels away from the line/cloud band: obs #1 shifts right,
    # obs #2 shifts up and left
    mutate(label_hjust = if_else(obs_num == 1, -0.4, 1.2),
           label_vjust = if_else(obs_num == 1, -0.3, -0.5))
  date_label <- {
    d <- as.Date(unique(df$date))
    paste0(format(d, "%B"), " ", as.integer(format(d, "%d")), ", ", format(d, "%Y"))
  }

  ggplot(df, aes(x = time_min, y = depth_m)) +
    # Hydrogen sulfide cloud starts around 19 m
    annotate("rect",
             xmin = -Inf, xmax = Inf, ymin = -20, ymax = -19,
             fill = "grey50", alpha = 0.3) +
    annotate("text",
             x = max(df$time_min), y = -19.5,
             label = "Hydrogen Sulfide Cloud",
             hjust = 1, vjust = 0.5, size = 3, color = "grey30") +
    geom_line(aes(color = temp_c), linewidth = 1) +
    geom_point(data = bookmarks, color = "red", size = 2) +
    geom_text(data = bookmarks,
              aes(label = paste0("Obs. #", obs_num, "\n",
                                  round(depth_m, 1), "\n",
                                  temp_c, "°C"),
                  hjust = label_hjust, vjust = label_vjust),
              color = "red", size = 3, lineheight = 0.9) +
    scale_y_continuous(limits = c(-20, 0)) +
    scale_color_viridis_c(name = "Temp (°C)", limits = color_limits,
                           guide = guide_colorbar(barwidth = unit(10, "cm"),
                                                   barheight = unit(0.4, "cm"))) +
    labs(x = "Time (min)", y = "Depth (m)", title = date_label) +
    theme_bw()
}

# Build one plot per date ---------------------------------------------------------
dive_plots <- data %>%
  group_split(date) %>%
  set_names(map_chr(., ~unique(.x$date))) %>%
  map(plot_dive_profile, color_limits = range(data$temp_c))

# Combine side by side with a shared legend and aligned, de-duplicated y axis -----
combined_dive_plot <- wrap_plots(dive_plots, ncol = 2) +
  plot_layout(guides = "collect", axes = "collect", axis_titles = "collect") &
  #plot_annotation(tag_levels = list("B", "D")) &
  theme(legend.position = "bottom")

combined_dive_plot

ggsave(file.path(here::here("data/dive_profiles.png")), combined_dive_plot, width = 9.5, height = 4.5)





