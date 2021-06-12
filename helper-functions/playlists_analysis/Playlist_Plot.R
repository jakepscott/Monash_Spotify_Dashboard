library(tidyverse)
library(here)
library(glue)
library(lubridate)
library(ggiraph)
library(ggtext)
theme_set(theme_minimal(base_size = 12))

playlist_tracks <- read_rds(here("data/playlist_tracks.rds"))

method <- "compare_playlists"
main_variable <- "valence"
comparison_variable <- "Date_Song_Saved"
how_many <- 5
data <- playlist_tracks

if (method=="compare_playlists") {
  data_agg <- aggregate_data(main_variable = main_variable, 
                             comparison_variable = comparison_variable,
                             data=data)
  barplot <- barplot_function(main_variable = main_variable,
                              how_many = how_many,
                              data = data_agg)
  scatterplot <- scatterplot_function(main_variable = main_variable,
                                      comparison_variable = comparison_variable,
                                      data = data_agg)
  
}
