# Load Librarie -----------------------------------------------------------
library(spotifyr) # for Spotify API
library(tidyverse) # for tidyverse
library(purrr) # for mapping
library(here) # for here
library(janitor) #For clean_names
library(furrr) # for parallel processing
library(future) # for parallel processing

# Get Access Token --------------------------------------------------------
access_token <- get_spotify_access_token()
#Set my id
my_id <- 'jakerocksalot'


# Get total number of songs -----------------------------------------------
#meta <- get_my_saved_tracks(limit = 1, offset = 0, include_meta_info = T)
#So I have 1238 total "liked" song
#total_songs <- meta$total
total_songs <- 1238

# Let's get all of my liked songs -----------------------------------------
#functional approach https://frie.codes/posts/using-r-to-remove-audiobooks-from-spotify/
sequence <- seq(0,total_songs + 50, 50)

get_chunk <- function(offset){
  cat(glue::glue("{offset} \n"))
  tracks <- get_my_saved_tracks(limit=50, 
                                offset = offset, 
                                include_meta_info = F)
  return(tracks)
}

my_songs_raw <- map_df(sequence,get_chunk)

my_songs_raw <- my_songs %>% 
  as_tibble()

#Save my liked songs
saveRDS(my_songs_raw,here("data/my_songs_raw.rds"))


# Clean my_songs ----------------------------------------------------------
#Select only cols of interest
my_songs <- my_songs_raw %>% 
  select(track.name,track.id, added_at, 
         track.artists, track.explicit, track.popularity,
         track.preview_url, track.album.name, track.album.id, 
         track.album.images, track.album.release_date, track.album.release_date_precision) %>% 
  clean_names()


#Unnest track.artists nested column
my_songs <- my_songs %>% 
  #This goes through each entry of track.artists and takes just the first row
  # This is selecting only the main artist of a given track, not the features
  mutate(artist_name=map(track_artists, function(x){x %>% head(1) %>% pull(name)})) %>% 
  unnest(cols=artist_name) %>% 
  select(-track_artists)

#Unnest track_album_images column, grabbing the 300X300 image
my_songs <- my_songs %>% 
  #This goes through each entry of track_album_images and takes just the second row
  # This is selecting the URL to the 300 by 300 album cover url
  mutate(album_image=map(track_album_images, function(x){x %>% filter(row_number()==2) %>% pull(url)})) %>% 
  unnest(cols=album_image) %>% 
  select(-track_album_images)

#Save the cleaned version of my_songs
saveRDS(my_songs,here("data/my_songs.rds"))


# Getting Track Features -----------------------------------------------
library(tictoc)
get_track_audio_features(my_songs$track_id[1])

tic()
test <- my_songs %>% 
  select(track_id) %>% 
  mutate(test=map(track_id,.f = get_track_audio_features)) 
toc()

test %>% 
  unnest(test)

ids <- my_songs$track_id

full_features <- get_track_audio_features(my_songs$track_id[1]) %>% 
  head(0)

for(i in ids){
  tryCatch({
    Sys.sleep(.01)
    print(i)
    full_features<-bind_rows(full_features,get_track_audio_features(i))},
    error=function(e){})
}

saveRDS(full_features,here("data/full_features.rds"))


# Join Songs and Features -------------------------------------------------
full_data <- my_songs %>% 
  left_join(full_features, by=c("track_id"="id"))

saveRDS(full_data,here("data/full_data.rds"))
