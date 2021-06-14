library(tidyverse)
library(here)
library(janitor)
library(spotifyr)
source(here("key.R"))

# Load in Popular Data ----------------------------------------------------
popular_data_raw <- read_rds(here("data/Popular_Songs_Raw.rds"))


# Grab just the id columns, and each distinct track -----------------------
popular_data_ids <- popular_data_raw %>% 
  select("track_id"=Id, track_name=Song, "artist_name"=Artist, "artist_id"=Artist_id, 
         "album_name"=Album, "album_id"=Album_id) %>% 
  distinct(track_id,.keep_all = T)

#Save just the names and IDs
saveRDS(popular_data,here("data/Popular_Songs_IDs.rds"))

# Here are the IDs for all the songs I had from old analysis, with missings --------------------------
popular_songs_ids <- read_rds(here("data/Popular_Songs_IDs.rds"))


# Find where album and/or artist id is missing ----------------------------
missing_artist_albums <- popular_songs_ids[!complete.cases(popular_songs_ids),]

#saveRDS(missing_artist_albums, here("data/missing_artist_albums_track_ids.rds"))


missing_artist_albums <- missing_artist_albums %>% 
  select(track_id,track_name, artist_id, artist_name,album_id, album_name)

# Grab artist and album ids for each one I am missing ---------------------
album_artist_ids <- get_tracks("4l0Mvzj72xxOpRrp6h8nHi") %>% 
  as_tibble() %>% 
  select(artists, "track_id"=id, album.id, album.name) %>% 
  head(0)


for(i in missing_artist_albums$track_id){
  print(i)
  to_bind <- get_tracks(i) %>% 
    as_tibble() %>% 
    select(artists, "track_id"=id, album.id, album.name)
  album_artist_ids <- album_artist_ids %>% bind_rows(to_bind)
}

# Grab artist name and id -------------------------------------------------
album_artist_ids <- album_artist_ids %>% 
  mutate(artist_id=map(artists, function(x){x %>% head(1) %>% pull(id)})) %>% 
  unnest(cols=artist_id) %>% 
  mutate(artist_name=map(artists, function(x){x %>% head(1) %>% pull(name)})) %>% 
  unnest(cols=artist_name) %>% 
  select(-artists) 

#Clean names
album_artist_ids <- album_artist_ids %>% 
  clean_names()

saveRDS(album_artist_ids, here("data/missing_artist_album_ids.rds"))


# Replace the missing artist and album IDs with the ones I found ----------
album_artist_ids <- read_rds(here("data/missing_artist_album_ids.rds"))
Popular_Songs_IDs <- read_rds(here('data/Popular_Songs_IDs.rds')) 

Popular_Songs_Ids_Fixed <- Popular_Songs_IDs %>% 
  na.omit() %>%  #Remove missing album and artist ids
  bind_rows(album_artist_ids) %>%  #Add the album and artist ids I found
  distinct(track_id, .keep_all = T) #Rid of duplicates

#Save this corrected IDs dataset
saveRDS(Popular_Songs_Ids_Fixed,here("data/Popular_Songs_Ids_Fixed.rds"))
