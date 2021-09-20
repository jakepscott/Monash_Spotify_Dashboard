#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load Libraries -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(spotifyr) # for Spotify API
library(tidyverse) # for tidyverse
library(purrr) # for mapping
library(here) # for here
library(janitor) #For clean_names
library(furrr) # for parallel processing
library(future) # for parallel processing
library(lubridate) # for dates
library(glue) #for bringing strings together
library(zoo)
library(tictoc)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get Access Token --------------------------------------------------------
access_token <- get_spotify_access_token()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set Params --------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Set my id
user_id <- 'jakerocksalot'
number_of_playlists <- 97
include_radios_mixes <- FALSE
playlists <- c("New 30","New 29", "New 28")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Obtain User Playlists ---------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#This is a sequence from 0 to the number of playlists plus 50 by 50. It is used
# This is then used as the offset in the get playlists function. So we get 
# the first 50 playlists, then the next 50, and so on until we have all the 
# playlists
sequence <- seq(0,number_of_playlists + 50, 50)
# This is just the user_id repeated for as many offsets as we have, which 
# will go into map
user_seq <- rep(user_id, length(sequence))

#This is a function that takes as input a user_Id and a user-selected
# offset value. We use this in map, so we run this function for as many 
# offsets as we need to get all playlists
get_select_playlist <- function(user_id, playlist_num){
  get_user_playlists(user_id, limit = 50, offset = playlist_num)
}

#Grab every playlist for given user. Takes about 2 seconds
tic()
playlists_raw <- map2_df(user_seq, sequence, get_select_playlist)
toc()

#Grab just the columns and playlists we want
playlists <- playlists_raw %>% 
  as_tibble() %>% 
  select(name, id, images, tracks.total) %>% 
  #Filter the ratio and mix playlists if the user says so
  filter(!str_detect(str_to_lower(name)," mix| radio")) %>% 
  filter(tracks.total>0) %>% 
  filter(name %in% c("New 30","New 29", "New 28"))

#Unnest the playlist images
playlists <- playlists %>% 
  #This goes through each entry of track_album_images and takes just the second row
  # This is selecting the URL to the 300 by 300 album cover url
  mutate(playlist_image=map(images, function(x){x %>% filter(row_number()==1) %>% pull(url)})) %>% 
  unnest(cols=playlist_image) %>% 
  select(-images)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Grab tracks from playlists -----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#This function takes a given playlist id, grabs the tracks, 
# and attaches the playlist name.

#Note: I say to only get track.id, however, once in a while (happens
# once in my playlists) it will give all columns. This isn't a big deal, they
# are all NAs anyways and I can just choose not to select them. Doesn't 
# changet he speed much
get_tracks_w_playlist <- function(id){
  print(id)
  tryCatch({
    get_playlist_tracks(playlist_id = id, 
                        fields = c("track.name","track.id", "added_at", 
                                   "track.artists", "track.explicit", 
                                   "track.popularity", "track.preview_url", 
                                   "track.album.name", "track.album.id", 
                                   "track.album.images", "track.album.release_date", 
                                   "track.album.release_date_precision")) %>% 
      mutate(playlist_id = id)
  })
}

#Get all tracks by mapping over every playlist id. Takes about 
# 18 seconds for 89 playlists
tracks_raw <- map_df(playlists$id, get_tracks_w_playlist)

#Make into tibble and select just the columns of interest
tracks <- tracks_raw %>% 
  as_tibble() %>% 
  #Some podcasts may be in playlists, and their track names are NA, so 
  # we can remove those
  filter(!is.na(track.name),
         !is.na(track.id)) %>% 
  select("playlist_id", "track.name","track.id", "added_at", 
         "track.artists", "track.explicit", 
         "track.popularity", "track.preview_url", 
         "track.album.name", "track.album.id", 
         "track.album.images", "track.album.release_date", 
         "track.album.release_date_precision") %>% 
  clean_names()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Grab artist name and album image -----------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#These two take about 8 seconds

#Unnest track.artists nested column
tic()
tracks <- tracks %>% 
  #This goes through each entry of track.artists and takes just the first row
  # This is selecting only the main artist of a given track, not the features
  #head() %>% 
  mutate(artist_name=map(track_artists, function(x){x %>% head(1) %>% pull(name)})) %>% 
  unnest(cols=artist_name) %>% 
  select(-track_artists)

#Unnest track_album_images column, grabbing the 300X300 image
tracks <- tracks %>% 
  #This goes through each entry of track_album_images and takes just the second row
  # This is selecting the URL to the 600 by 600 album cover url
  mutate(album_image=map(track_album_images, function(x){x %>% filter(row_number()==1) %>% pull(url)})) %>% 
  unnest(cols=album_image) %>% 
  select(-track_album_images)
toc()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Grab track features -----------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


ids_of_interest <- tracks %>% 
  distinct(track_id)


full_features <- get_track_audio_features(ids_of_interest$track_id[1]) %>% 
  head(0)

tic()
for(i in ids_of_interest$track_id){
  tryCatch({
    Sys.sleep(.01)
    print(i)
    full_features<-bind_rows(full_features,get_track_audio_features(i))},
    error=function(e){})
}
toc()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Join tracks and track features -----------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
full_data <- tracks %>% 
  left_join(full_features, by=c("track_id"="id"))

# Do some minor cleaning --------------------------------------------------
#Clean up duration ms
full_data <- full_data %>% 
  #Get a minutes col
  mutate(minutes=duration_ms/1000/60,
         #get a minutes col that is a character so I can separate at the decimal
         min_char=as.character(minutes)) %>% 
  #Separate at decimal so I can have a whole minutes column and a decimal seconds column
  separate(min_char,into = c("whole_min", "remainder_seconds"),sep = "\\.", remove=F) %>% 
  mutate(remainder_seconds=glue(".{remainder_seconds}"), #add the decimal back to decimal seconds col so I can us as.numeric
         #Convert remainder seconds to numeric
         remainder_seconds=as.numeric(remainder_seconds),
         #Convert decimal remainder seconds to whole seconds
         remainder_seconds=remainder_seconds*60,
         #round
         remainder_seconds=round(remainder_seconds,0)) %>% 
  #create a label that say X minutes and Y seconds
  mutate(duration_label=glue("{whole_min} minutes and {remainder_seconds} seconds")) %>% 
  select(-min_char,-whole_min,-remainder_seconds)

#Create a date column
full_data <- full_data %>% 
  mutate(date=str_remove_all(added_at,"T.*"),
         date=ymd(date),
         year_month=as.yearmon(date))

#Fix the album release date column
full_data <- full_data %>% 
  mutate(track_album_release_date=ifelse(track_album_release_date_precision=="year", 
                                         glue("{track_album_release_date}-01-01"),
                                         track_album_release_date)) %>% 
  mutate(Track_Release_Date=as.Date(track_album_release_date)) %>% 
  select(-track_album_release_date)


#Make "added_at" a date column
full_data <- full_data %>% 
  mutate(Date_Song_Saved=as.Date(added_at)) %>% 
  select(-added_at)

#Make labels for the track release date and added_at columns
full_data <- full_data %>% 
  mutate(Song_Saved_Label = as.character(Date_Song_Saved), 
         Track_Release_Label = as.character(Track_Release_Date))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Join playlist and track data -----------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
playlist_data <- tracks %>% 
  left_join(playlists, by= c("playlist_id" = "id"))
