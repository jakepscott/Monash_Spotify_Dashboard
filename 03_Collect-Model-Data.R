# Load Libs and Data ------------------------------------------------------
library(tidyverse)
library(janitor)
library(here)
library(spotifyr)
library(glue)

# Load in Popular Data ----------------------------------------------------
popular_data_raw <- read_rds(here("data/Popular_Songs_Raw.rds"))
  

# Grab just the id columns, and each distinct track -----------------------
popular_data <- popular_data_raw %>% 
  select("track_id"=Id, track_name=Song, "artist_name"=Artist, "artist_id"=Artist_id, 
         "album_name"=Album, "album_id"=Album_id) %>% 
  distinct(track_id,.keep_all = T)

#Save just the names and IDs
saveRDS(popular_data,here("data/Popular_Songs_IDs.rds"))


# get track features information -----------------------------------------
popular_features <- get_track_audio_features(popular_data$track_id[1])

for (i in popular_ids$track_id) {
  print(i)
  to_bind <- get_track_audio_features(i)
  popular_features <- popular_features %>%
    bind_rows(to_bind)
}

popular_features <- popular_features %>% 
  rename("track_id"=id)
saveRDS(popular_features,here("data/Popular-Songs_Features.rds"))


# Obtain Album information ------------------------------------------------
album_ids <- popular_data %>% 
  distinct(album_id)

popular_albums <- get_albums(album_ids$album_id[1]) %>% 
  as_tibble() %>%
  select("album_id"=id, "album_popularity"=popularity,
         "track_release_date"=release_date,
         release_date_precision)
  
for (i in album_ids$album_id) {
  tryCatch({
    print(i)
    to_bind <- get_albums(i) %>% 
      as_tibble() %>%
      select("album_id"=id, "album_popularity"=popularity,
             "track_release_date"=release_date,
             release_date_precision)
    
    popular_albums <- popular_albums %>%
      bind_rows(to_bind)
  },
  error=function(e){})
}

saveRDS(popular_albums, here("data/Popular-Songs_Albums.rds"))


# Get Popular Artist Information ------------------------------------------
artist_ids <- popular_data %>% 
  distinct(artist_id)

popular_artists_raw <- get_artists(artist_ids$artist_id[1]) %>% 
  as_tibble() %>% 
  head(0)

for (i in artist_ids$artist_id) {
  print(i)
  to_bind <- get_artists(i) %>% 
    as_tibble()
  popular_artists_raw <- popular_artists_raw %>% 
    bind_rows(to_bind)
}


artist_info <- popular_artists_raw %>% 
  clean_names() %>% 
  select("artist_name"=name,
         "artist_followers"=followers_total,
         "artist_popularity"=popularity,
         "artist_id"=id,
         genres)

#This function unlists the genre column, making it a character string of comma separated genres
unlisting_function <- function(x){
  unlisted <- unlist(x)
  if (is.null(unlisted)) {
    return(NA)
  } else if(!is.null(unlisted)){
    return(unlisted %>% knitr::combine_words(sep = ",",and = ""))
  }
}

popular_artists <- popular_artists_raw %>% 
  mutate(genres=map(genres,unlisting_function)) %>% 
  unnest(cols=genres) 

popular_artists <- popular_artists %>% 
  select(genres,"artist_id"=id,"artist_popularity"=popularity, "artist_followers"=followers.total)

saveRDS(popular_artists, here("data/Popular-Songs_Artists.rds"))


# Joining features, album, and artist information -------------------------
#Load in the data
popular_songs_ids <- read_rds(here("data/Popular_Songs_IDs.rds"))
popular_songs_features <- read_rds(here("data/Popular-Songs_Features.rds"))
popular_songs_albums <- read_rds(here("data/Popular-Songs_Albums.rds"))
popular_songs_artists <- read_rds(here("data/Popular-Songs_Artists.rds"))

full_popular_songs <- popular_songs_ids %>% 
  left_join(popular_songs_features, by="track_id") %>% 
  left_join(popular_songs_albums, by = "album_id") %>% 
  left_join(popular_songs_artists, by = "artist_id")

full_popular_songs <- full_popular_songs %>%
  mutate(genres=as.character(genres)) %>% 
  select(-type, -uri, -track_href, -analysis_url)

full_popular_songs <- full_popular_songs %>% 
  mutate(track_release_date = ifelse(release_date_precision=="year", 
                                         glue("{track_release_date}-01-01"),
                                     track_release_date)) %>% 
  mutate(track_release_date=as.Date(track_release_date)) %>% 
  select(-release_date_precision)

saveRDS(full_popular_songs, here("data/Full_Popular_Songs.rds"))
