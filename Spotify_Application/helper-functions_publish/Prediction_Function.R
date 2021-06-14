predict_like <- function(track_name_choice, data, model){
  song_data <- data %>% 
    filter(track_name==track_name_choice) %>% 
    distinct(track_name, .keep_all = T)
 
  #Get album id from song  
  album_id <- song_data %>% 
    filter(track_name==track_name_choice) %>% 
    pull(album_id)
  
  #Grab artist id from song
  artist_id <- song_data %>% 
    filter(track_name==track_name_choice) %>% 
    pull(artist_id)
  
  
  #grab album popularity
  album_popularity <- get_albums(album_id) %>% 
    as_tibble() %>% 
    select("album_popularity"=popularity)
  
  #Grab artist popularity and followers
  artist_pop_followers <- get_artists(artist_id) %>% 
    as_tibble() %>% 
    select("artist_popularity"=popularity, "artist_followers"=followers.total)
  
  #Join all the data together
  song_data <- song_data %>% 
    filter(track_name==track_name_choice) %>% 
    bind_cols(album_popularity) %>% 
    bind_cols(artist_pop_followers) %>% 
    mutate(across(where(is.character), as.factor)) %>% 
    mutate(track_release_date=as.numeric(track_release_date)) %>% 
    mutate(key=as.factor(key),
           mode=as.factor(mode),
           time_signature=as.factor(time_signature)) %>% 
    mutate(across(where(is.numeric),as.numeric))
  
  
  
  
  prediction <- model %>% 
    predict(new_data=song_data) %>% 
    mutate(prediction=as.character(.pred_class)) %>% 
    pull(prediction)
   
  return(prediction)
}

#predict_like(track_name_choice="Ivy", data=artist_data, model=model)
