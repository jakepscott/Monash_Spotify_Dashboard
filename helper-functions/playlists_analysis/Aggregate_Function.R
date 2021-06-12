#This function takes a set of track-playlist pairs, and takes the median value of user-selected columns. It 
# returns a tibble of playlists and the median values (as well as some labels)

aggregate_data <- function(main_variable, comparison_variable, data){
  agg <- data %>% 
    select(playlist_name, playlist_id, main_variable, comparison_variable) %>% 
    group_by(playlist_name) %>% 
    summarise(across(.cols = c(main_variable, comparison_variable), median, na.rm=T))
  
  #Join playlist ids back in
  agg <- agg %>% 
    left_join(data %>% distinct(playlist_name, playlist_id) %>% select(playlist_name, playlist_id))
  
  
  # If one of the vars is duration, make a duration_label column --------
  if (main_variable =="minutes" | comparison_variable == "minutes") {
    ##
    #Create duration label
    ##
    agg <- agg %>% 
      mutate(#get a minutes col that is a character so I can separate at the decimal
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
  }
  
  
  # If one of the vars is Date Song Saved, add label col -----------------------------------
  if (main_variable =="Date_Song_Saved" | comparison_variable == "Date_Song_Saved"){
    agg <- agg %>% 
      mutate(Song_Saved_Label = as.character(Date_Song_Saved))
  }
  # If one of the vars is Date Song Saved, add label col -----------------------------------
  if (main_variable =="Track_Release_Date" | comparison_variable == "Track_Release_Date"){
    agg <- agg %>% 
      mutate(Track_Release_Label = as.character(Track_Release_Date))
  }
  return(agg)
}





