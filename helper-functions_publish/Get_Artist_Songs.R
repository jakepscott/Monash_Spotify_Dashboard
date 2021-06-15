get_artist_songs <- function(artist_name){
  load(here("data/keys"))
  artist_info <- searchArtist(artist_name,token=keys) %>% 
    head(1) %>% 
    as_tibble() 
  
  if (nrow(artist_info)==0) {
    return(tibble(character(length = 0)))
  } else {
    #Get the artists songs
    artist_data_raw <- spotifyr::get_artist_audio_features(artist_info$id) %>% 
      as_tibble() 
    artist_data <- artist_data_raw %>% 
      #Just selecting the variables I need
      select(artist_name,track_name,"track_release_date"=album_release_date, 
             "release_date_precision"=album_release_date_precision,
             danceability:tempo,time_signature, duration_ms,
             artist_id, album_id, track_id) %>% 
      #Fixing the track release date column
      mutate(track_release_date = ifelse(release_date_precision=="year", 
                                         glue("{track_release_date}-01-01"),
                                         track_release_date)) %>% 
      mutate(track_release_date=as.Date(track_release_date)) %>% 
      select(-release_date_precision)
    return(artist_data)
  }
}

#get_artist_songs("Frank Ocean")
