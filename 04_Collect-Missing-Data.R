library(tidyverse)
library(here)
library(janitor)

popular_songs_ids <- read_rds(here("data/Popular_Songs_IDs.rds"))


# Find where album and/or artist id is missing ----------------------------
missing_artist_albums <- popular_songs_ids[!complete.cases(popular_songs_ids),]
saveRDS(missing_artist_albums, here("data/missing_artist_albums_track_ids.rds"))


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

album_artist_ids <- album_artist_ids %>% 
  clean_names()

saveRDS(album_artist_ids, here("data/missing_artist_album_ids.rds"))


# Grab artist and album information ---------------------------------------
# Obtain Album information ------------------------------------------------
album_ids <- album_artist_ids %>% 
  distinct(album_id)

missing_albums <- get_albums(album_ids$album_id[1]) %>% 
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
    
    missing_albums <- missing_albums %>%
      bind_rows(to_bind)
  },
  error=function(e){})
}

missing_albums <- missing_albums %>% 
  mutate(track_release_date =ifelse(release_date_precision =="year", 
                                         glue("{track_release_date}-01-01"),
                                    track_release_date)) %>% 
  mutate(track_release_date=as.Date(track_release_date)) %>% 
  select(-release_date_precision)

saveRDS(missing_albums, here("data/Missing_Albums.rds"))


# Get Popular Artist Information ------------------------------------------
artist_ids <- album_artist_ids %>% 
  distinct(artist_id)

missing_artists_raw <- get_artists(artist_ids$artist_id[1]) %>% 
  as_tibble() %>% 
  head(0)

for (i in artist_ids$artist_id) {
  print(i)
  to_bind <- get_artists(i) %>% 
    as_tibble()
  missing_artists_raw <- missing_artists_raw %>% 
    bind_rows(to_bind)
}


artist_info <- missing_artists_raw %>% 
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

artist_info <- artist_info %>% 
  mutate(genres=map(genres,unlisting_function)) %>% 
  unnest(cols=genres) %>% 
  mutate(genres=as.character(genres))

artist_info <- artist_info %>% 
  select(genres,"artist_id"=id,"artist_popularity"=popularity, "artist_followers"=followers.total)

saveRDS(popular_artists, here("data/Missing_Artists.rds"))



# Join these now found artists and albums to full data --------------------
#Load the data gotten above
missing_ids <- read_rds(here("data/missing_artist_album_ids.rds"))
albums <- read_rds(here("data/Missing_Albums.rds"))
artists <- read_rds(here("data/Missing_Artists.rds")) %>% mutate(genres=as.character(genres))

#Join the prev missing data
formerly_missing_data <- missing_ids %>%   
  left_join(albums) %>% 
  left_join(artists)

#Grab feats
Popular_Songs_Features <- read_rds(here('data/Popular-Songs_Features.rds'))
#Join features to the prev missing album and artists
formerly_missing_data_w_feats <- formerly_missing_data %>% 
  left_join(Popular_Songs_Features) %>% 
  select(-uri,-track_href,-analysis_url,-type)

#Adding track name to the prev missing album and artists
track_names <- read_rds(here("data/Popular_Songs_IDs.rds"))
formerly_missing_data_w_feats <- formerly_missing_data_w_feats %>% 
  left_join(track_names %>% select(track_id, track_name))

saveRDS(formerly_missing_data_w_feats,here("data/Full_Formerly_Missing_Data.rds"))

#Add the formerly missing data to the full data
formerly_missing_data_w_feats <- read_rds(here("data/Full_Formerly_Missing_Data.rds"))
Full_Popular_Songs <- read_rds(here('data/Full_Popular_Songs.rds')) 


Full_Popular_Songs_Missings_Fixed <- Full_Popular_Songs %>% 
  na.omit() %>% 
  bind_rows(formerly_missing_data_w_feats)


saveRDS(Full_Popular_Songs_Missings_Fixed,here('data/Full_Popular_Songs_Missings_Fixed.rds'))


# Add "liked" column to corrected data ------------------------------------
Full_Popular_Songs_Missings_Fixed <- read_rds(here('data/Full_Popular_Songs_Missings_Fixed.rds'))
my_songs <- read_rds(here("data/full_data.rds"))

Full_Popular_Songs_Missings_Fixed <- Full_Popular_Songs_Missings_Fixed %>% 
  select(-liked) %>% 
  mutate(liked=(track_id %in% my_songs$track_id))
saveRDS(Full_Popular_Songs_Missings_Fixed,here("data/Full_Popular_Songs_Missings_Fixed.rds"))  
