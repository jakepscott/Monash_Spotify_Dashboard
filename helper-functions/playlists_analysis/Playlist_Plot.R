library(tidyverse)
library(here)

playlist_tracks <- read_rds(here("data/playlist_tracks.rds"))

aggregation <- "compare_playlists"
main_variable <- "Track_Release_Date"
comparison_variable <- "minutes"
how_many <- 5
data <- playlist_tracks_features

if (aggregation=="compare_playlists") {
  
}