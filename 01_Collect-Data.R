# Load Librarie -----------------------------------------------------------
library(spotifyr) # for Spotify API
library(tidyverse) # for tidyverse
library(purrr) # for mapping
library(here) # for here
library(janitor) #For clean_names
library(furrr) # for parallel processing
library(future) # for parallel processing
library(lubridate) # for dates
library(glue) #for bringing strings together


# Get Access Token --------------------------------------------------------
access_token <- get_spotify_access_token()
#Set my id
my_id <- 'jakerocksalot'

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Liked Songs -------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

my_songs_raw <- my_songs_raw %>% 
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

saveRDS(full_data,here("data/full_data.rds"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Playlists -------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Get total number of playlists -------------------------------------------
#playlist_metadata <- get_user_playlists(user_id = "jakerocksalot", limit=1, include_meta_info = T) 
#total_playlists <- playlist_metadata$total
total_playlists <- 90 #There are 90 playlists

# Let's get all of my playlists -----------------------------------------
#functional approach https://frie.codes/posts/using-r-to-remove-audiobooks-from-spotify/
sequence <- seq(0,total_playlists + 50, 50)

get_chunk <- function(offset){
  cat(glue::glue("{offset} \n"))
  tracks <- get_user_playlists(user_id = "jakerocksalot",
                               limit=50, 
                               offset = offset, 
                               include_meta_info = F)
  return(tracks)
}

my_playlists_raw <- map_df(sequence,get_chunk)

my_playlists_raw <- my_playlists_raw %>% 
  as_tibble()

#Save my playlists
saveRDS(my_playlists_raw,here("data/my_playlists_raw"))


# Let's find the tracks for each playlist ---------------------------------
my_playlists_raw <- my_playlists_raw %>% 
  filter(owner.display_name == "spotify" |
           owner.display_name == "jakerocksalot") 


playlist_ids <- my_playlists_raw$id

playlist_tracks <- get_playlist_tracks(my_playlists_raw$id[1], fields = c("track.id", "track.name")) %>% 
  head(0) %>% 
  mutate(playlist_id=character(length = 0L))
  
for(i in playlist_ids){
  tryCatch({
    Sys.sleep(.01)
    print(i)
    to_bind <- get_playlist_tracks(i, fields = c("track.id", "track.name")) %>% 
      mutate(playlist_id=i)
    playlist_tracks<-bind_rows(playlist_tracks,to_bind)},
    error=function(e){})
}

playlist_tracks <- playlist_tracks %>% as_tibble()

playlist_tracks <- playlist_tracks %>% 
  left_join(my_playlists_raw %>% select(id, name), by = c("playlist_id"="id"))

#This only got 100 of the 134 songs in "Biking in Purgatory to Cathart", because get_playlist_tracks()  has
# a limit of 100 tracks. I can fix this by just offsetting by 100 and getting those final 34
playlist_tracks %>% filter(name=="Biking in Purgatory to Cathart")

C_to_H <- get_playlist_tracks("29DhHXBzEsqRHa3KgWFqhV", 
                              fields = c("track.id", "track.name"),
                              offset = 100) %>% 
  mutate(playlist_id="29DhHXBzEsqRHa3KgWFqhV",
         name="Biking in Purgatory to Cathart")

#Add these final 34 songs to playlist_tracks
playlist_tracks <- playlist_tracks %>% 
  bind_rows(C_to_H) %>% 
  clean_names() %>% 
  rename("playlist"=name)


#Save playlist-track pairs
saveRDS(playlist_tracks,here("data/playlist_tracks_raw.rds"))


# Add features to playlist-track pairs data -------------------------------
features <- read_rds(here("data/full_data.rds"))
features <- features %>% 
  select(track_id,track_popularity, 
         danceability, energy, valence, tempo,  
         minutes, loudness, Date_Song_Saved, Track_Release_Date,
         duration_label, Song_Saved_Label, Track_Release_Label) 

playlist_tracks_features <- playlist_tracks %>% 
  left_join(features) %>% 
  relocate(c(playlist,playlist_id), .before = everything())

#Save playlist-track pairs with features
saveRDS(playlist_tracks_features,here("data/playlist_tracks.rds"))
