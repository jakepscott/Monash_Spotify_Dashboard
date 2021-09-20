obtain_track_features <- function(playlists_of_int, data){
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Grab tracks from playlists -----------------------------------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  playlists <- data %>% 
    filter(playlist_name %in% playlists_of_int)
  
  
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
  
  ###
  #Unnest track.artists nested column
  ### 
  # This function says give me an object, if I unlist it and its null (for example,
  # when a given row of images is a 0X0 dataframe) then just give me NA. Otherwise,
  # extract the image URL. This accounts for the fact that some playlists don't have
  # an artist listed
  unnest_artists <- function(x){
    if (is.null(unlist(x))) {
      NA
    } else{
      x %>% head(1) %>% pull(name)
    }
  }
  
  tracks <- tracks %>% 
    #This goes through each entry of track.artists and takes just the first row
    # This is selecting only the main artist of a given track, not the features
    #head() %>% 
    mutate(artist_name=map(track_artists, unnest_artists)) %>% 
    unnest(cols=artist_name) %>% 
    select(-track_artists)
  ###
  #Unnest track_album_images column, grabbing the 300X300 image
  ###
  # This function says give me an object, if I unlist it and its null (for example,
  # when a given row of images is a 0X0 dataframe) then just give me NA. Otherwise,
  # extract the image URL. This accounts for the fact that some tracks don't have
  # a album cover art
  unnest_album_covers <- function(x){
    if (is.null(unlist(x))) {
      NA
    } else{
      x %>% filter(row_number()==1) %>% pull(url)
    }
  }
  
  tracks <- tracks %>% 
    #This goes through each entry of track_album_images and takes just the second row
    # This is selecting the URL to the 600 by 600 album cover url
    mutate(album_image=map(track_album_images, unnest_album_covers)) %>% 
    unnest(cols=album_image) %>% 
    select(-track_album_images)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Grab track features -----------------------------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  ids_of_interest <- tracks %>% 
    distinct(track_id)
  
  
  full_features <- get_track_audio_features(ids_of_interest$track_id[1]) %>% 
    head(0)
  
  for(i in ids_of_interest$track_id){
    tryCatch({
      Sys.sleep(.01)
      print(i)
      full_features<-bind_rows(full_features,get_track_audio_features(i))},
      error=function(e){})
  }
  
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
  
  return(full_data)
}

