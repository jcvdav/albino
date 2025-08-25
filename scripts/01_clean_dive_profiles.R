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

  data <- tibble(time,
                 depth,
                 temp) %>%
    summarise_all(as.numeric) %>%
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
           ((time_min == 24) & (date == "2024-07-08")))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS()







