# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Load Libraries -----------------------------------------------------------
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# library(spotifyr) # for Spotify API
# library(tidyverse) # for tidyverse
# library(purrr) # for mapping
# library(here) # for here
# library(janitor) #For clean_names
# library(furrr) # for parallel processing
# library(future) # for parallel processing
# library(lubridate) # for dates
# library(glue) #for bringing strings together
# library(zoo)


get_playlist_data <- function(user_id,
                              number_of_playlists,
                              include_radios_mixes){
  
  #This placeholder business does the following. I make get_user_playlists into a 
  # "safe" function using purrr, meaning if it errors out it doesn't stop R,
  # it just records the error. So if the error portion of placeholder is not null,
  # it means the username was not found, so I return an empty tibble, which tells
  # the app that the username wasn't found. If it does work, the error bit will be 
  # null, and the rest of this function can run
  placeholder <- tibble()
  
  safe_get_user_playlists <- safely(get_user_playlists, otherwise = NA)
  
  placeholder <- safe_get_user_playlists(user_id)
  
  if (!is.null(placeholder$error)) {
    return(tibble())
  } else {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Obtain User Playlists ---------------------------------------------------
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #This is a sequence from 0 to the number of playlists plus 50 by 50. It is used
    # This is then used as the offset in the get playlists function. So we get 
    # the first 50 playlists, then the next 50, and so on until we have all the 
    # playlists
    sequence <- seq(0,200, 50)
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
    playlists_raw <- map2_df(user_seq, sequence, get_select_playlist)
    
    #Grab just the columns and playlists we want
    playlists <- playlists_raw %>% 
      as_tibble() %>% 
      select(name, id, images, tracks.total) %>% 
      filter(tracks.total>0) 
    
    #Remove radio and mix playlists if indicated by the user
    #Filter the ratio and mix playlists if the user says so
    if (include_radios_mixes == F) {
      playlists <- playlists %>% 
        filter(!str_detect(str_to_lower(name)," mix| radio")) 
    }
    
    # 
    # x <- playlists$images[1]
    # x <- playlists$images[16]
    # 
    # x %>% unlist()
    
    # This function says give me an object, if I unlist it and its null (for example,
    # when a given row of images is a 0X0 dataframe) then just give me NA. Otherwise,
    # extract the image URL. This accounts for the fact that some playlists don't have
    # an image
    unnest_images <- function(x){
      if (is.null(unlist(x))) {
        NA
      } else{
        x %>% filter(row_number()==1) %>% pull(url)
      }
    }
    
    
    
    #Unnest the playlist images
    playlists <- playlists %>% 
      #This goes through each entry of track_album_images and takes just the second row
      # This is selecting the URL to the 300 by 300 album cover url
      mutate(playlist_image=map(images, unnest_images)) %>% 
      unnest(cols=playlist_image) %>% 
      select(-images) %>% 
      clean_names() %>% 
      rename("playlist_name" = name)
    
    return(playlists)
  }
}
