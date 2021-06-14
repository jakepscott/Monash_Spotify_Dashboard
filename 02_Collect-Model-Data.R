# Load Libs and Data ------------------------------------------------------
library(tidyverse)
library(janitor)
library(here)
library(spotifyr)
library(glue)

# Load in Popular Data ----------------------------------------------------
popular_data_ids <- read_rds(here("data/Popular_Songs_Ids_Fixed.rds"))


# get track features information -----------------------------------------
popular_features <- get_track_audio_features(popular_data_ids$track_id[1])

for (i in popular_data_ids$track_id) {
  print(i)
  to_bind <- get_track_audio_features(i)
  popular_features <- popular_features %>%
    bind_rows(to_bind)
}

popular_features <- popular_features %>% 
  rename("track_id"=id) %>% 
  select(-uri,-track_href,-analysis_url)

saveRDS(popular_features,here("data/Popular-Songs_Features.rds"))


# Obtain Album information ------------------------------------------------
album_ids <- popular_data_ids %>% 
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
artist_ids <- popular_data_ids %>% 
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
saveRDS(artist_info, here("data/Raw_Pop_Artist_Info.rds"))


#This function unlists the genre column, making it a character string of comma separated genres
unlisting_function <- function(x){
  unlisted <- unlist(x)
  if (is.null(unlisted)) {
    return(NA)
  } else if(!is.null(unlisted)){
    return(unlisted %>% 
             paste(collapse = ",")
    )
  }
}

popular_artists <- artist_info %>% 
  mutate(genres=map(genres,unlisting_function)) %>% 
  unnest(cols=genres) 


saveRDS(popular_artists, here("data/Popular-Songs_Artists.rds"))


# Joining features, album, and artist information -------------------------
#Load in the data
popular_songs_ids <- read_rds(here("data/Popular_Songs_Ids_Fixed.rds"))
popular_songs_features <- read_rds(here("data/Popular-Songs_Features.rds"))
popular_songs_albums <- read_rds(here("data/Popular-Songs_Albums.rds"))
popular_songs_artists <- read_rds(here("data/Popular-Songs_Artists.rds"))

#Join the data
full_popular_songs <- popular_songs_ids %>% 
  left_join(popular_songs_features, by="track_id") %>% 
  left_join(popular_songs_albums, by = "album_id") %>% 
  left_join(popular_songs_artists %>% select(-artist_name), by = "artist_id")

#Bring in track_names for the 900+ missing track names, using the track_id column
track_names <- read_rds(here("data/Popular_Songs_IDs.rds")) %>% select(track_id,track_name) #This has all track names

full_popular_songs <- full_popular_songs %>% 
  select(-track_name) %>%  #Get rid of this col because it has so many missings
  left_join(track_names) %>%  #bring in the non-missing track names
  na.omit() #remove ~60 obs where there is missing data

#Fix the track release date column
full_popular_songs <- full_popular_songs %>% 
  mutate(track_release_date = ifelse(release_date_precision=="year", 
                                         glue("{track_release_date}-01-01"),
                                     track_release_date)) %>% 
  mutate(track_release_date=as.Date(track_release_date)) %>% 
  select(-release_date_precision)

saveRDS(full_popular_songs, here("data/Full_Popular_Songs.rds"))



# Create output variable, indicator for whether song is in my data --------
my_songs <- read_rds(here("data/full_data.rds"))
full_popular_songs <- read_rds(here("data/Full_Popular_Songs.rds"))

full_popular_songs <- full_popular_songs %>% 
  mutate(liked=(track_id %in% my_songs$track_id))

saveRDS(full_popular_songs, here("data/Full_Popular_Songs.rds"))


full_popular_songs %>% 
  select(track_name, track_id, artist_name, album_name, liked) %>% 
  filter(!is.na(artist_name) & !is.na(album_name)) %>% 
  count(liked)
