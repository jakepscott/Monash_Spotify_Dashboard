scatterplot_function <- function(main_variable, comparison_variable, playlist_or_track, data){
  if (playlist_or_track=="playlist") {
    # Set labels --------------------------------------------------------------
    #If variable=="minutes", use the custom made "duration_label", otherwise use a generic label
    data_scatter <- data %>% 
      rowwise() %>% 
      mutate(x_label=case_when(comparison_variable=="minutes" ~ duration_label,
                               comparison_variable=="Date_Song_Saved" ~ Song_Saved_Label,
                               comparison_variable=="Track_Release_Date" ~ Track_Release_Label,
                               TRUE ~ as.character(!!as.symbol(comparison_variable))),
             y_label=case_when(main_variable=="minutes"~duration_label,
                               main_variable=="Date_Song_Saved" ~ Song_Saved_Label,
                               main_variable=="Track_Release_Date" ~ Track_Release_Label,
                               TRUE ~ as.character(!!as.symbol(main_variable))),
             label=glue("Playlist: {str_to_title(playlist_name)} \nMedian {str_to_title(str_replace_all(main_variable, '_', ' '))}: {y_label} \nMedian {str_to_title(str_replace_all(comparison_variable, '_', ' '))}: {x_label}"))
    
    
    # PLot --------------------------------------------------------------------
    data_scatter %>% 
      filter(!!as.symbol(main_variable)!=0) %>% 
      filter(!!as.symbol(comparison_variable)!=0) %>% 
      ggplot(aes(!!as.symbol(comparison_variable),!!as.symbol(main_variable))) +
      geom_vline(xintercept = (data %>% pull(comparison_variable) %>% median(na.rm = T))) +
      geom_hline(yintercept = (data %>% pull(main_variable) %>% median(na.rm = T))) +
      geom_point_interactive(color="grey",
                             alpha = 0.5,
                             aes(tooltip=label,
                                 data_id=playlist_id)) +
      geom_smooth(se=F,
                  color="#1DB954") +
      labs(title = glue("Median *{str_to_title(str_replace_all(main_variable,'_', ' '))}* Versus Median *{str_to_title(str_replace_all(comparison_variable, '_', ' '))}*"),
           subtitle = "By playlist",
           x=glue("Median {str_to_title(str_replace_all(comparison_variable,'_', ' '))}"),
           y=glue("Median {str_to_title(str_replace_all(main_variable,'_', ' '))}")) + 
      theme(plot.title.position = "plot",
            plot.title = element_markdown(size=rel(1.25)),
            plot.subtitle = element_text(color = "grey30"))
    
    
    # If the data is all the tracks: ------------------------------------------
  } else if (playlist_or_track=="track") {
    if (!is.null(data$playlist_name)) {
      #If it is more than one playlist and each track within those playlists plotted as color coded points
      # highlight every song in a a plylist
      if (length(unique(data$playlist_name))>1) {
        # Set labels --------------------------------------------------------------
        #If variable=="minutes", use the custom made "duration_label", otherwise use a generic label
        data_scatter <- data %>% 
          rowwise() %>% 
          mutate(x_label=case_when(comparison_variable=="minutes" ~ duration_label,
                                   comparison_variable=="Date_Song_Saved" ~ Song_Saved_Label,
                                   comparison_variable=="Track_Release_Date" ~ Track_Release_Label,
                                   TRUE ~ as.character(!!as.symbol(comparison_variable))),
                 y_label=case_when(main_variable=="minutes"~duration_label,
                                   main_variable=="Date_Song_Saved" ~ Song_Saved_Label,
                                   main_variable=="Track_Release_Date" ~ Track_Release_Label,
                                   TRUE ~ as.character(!!as.symbol(main_variable))),
                 label=glue("Track: {str_to_title(track_name)} \nPlaylist: {playlist_name} \n{str_to_title(str_replace_all(main_variable, '_', ' '))}: {y_label} \n{str_to_title(str_replace_all(comparison_variable, '_', ' '))}: {x_label} \n Album: {track_album_name} \n Artist: {artist_name}"))
        
        
        # PLot --------------------------------------------------------------------
        data_scatter %>% 
          filter(!!as.symbol(main_variable)!=0) %>% 
          filter(!!as.symbol(comparison_variable)!=0) %>% 
          ggplot(aes(!!as.symbol(comparison_variable),!!as.symbol(main_variable))) +
          geom_vline(xintercept = (data %>% pull(comparison_variable) %>% median(na.rm = T))) +
          geom_hline(yintercept = (data %>% pull(main_variable) %>% median(na.rm = T))) +
          geom_point_interactive(alpha = 0.5,
                                 show.legend=T,
                                 aes(tooltip=label,
                                     data_id=playlist_id,
                                     color=playlist_name)) +
          # geom_smooth(se=F,
          #             aes(color=playlist_name),
          #             show.legend = F) +
          labs(title = glue("*{str_to_title(str_replace_all(main_variable,'_', ' '))}* versus *{str_to_title(str_replace_all(comparison_variable, '_', ' '))}*"),
               x=str_to_title(str_replace_all(comparison_variable,"_", " ")),
               y=str_to_title(str_replace_all(main_variable,"_", " ")),
               color=NULL) + 
          theme(plot.title.position = "plot",
                plot.title = element_markdown(size=rel(1.5)))
      } else {
        # Set labels --------------------------------------------------------------
        #If variable=="minutes", use the custom made "duration_label", otherwise use a generic label
        data_scatter <- data %>% 
          rowwise() %>% 
          mutate(x_label=case_when(comparison_variable=="minutes" ~ duration_label,
                                   comparison_variable=="Date_Song_Saved" ~ Song_Saved_Label,
                                   comparison_variable=="Track_Release_Date" ~ Track_Release_Label,
                                   TRUE ~ as.character(!!as.symbol(comparison_variable))),
                 y_label=case_when(main_variable=="minutes"~duration_label,
                                   main_variable=="Date_Song_Saved" ~ Song_Saved_Label,
                                   main_variable=="Track_Release_Date" ~ Track_Release_Label,
                                   TRUE ~ as.character(!!as.symbol(main_variable))),
                 label=glue("Track: {str_to_title(track_name)} \n{str_to_title(str_replace_all(main_variable, '_', ' '))}: {y_label} \n{str_to_title(str_replace_all(comparison_variable, '_', ' '))}: {x_label} \n Album: {track_album_name} \n Artist: {artist_name}"))
        
        
        # PLot --------------------------------------------------------------------
        data_scatter %>% 
          filter(!!as.symbol(main_variable)!=0) %>% 
          filter(!!as.symbol(comparison_variable)!=0) %>% 
          ggplot(aes(!!as.symbol(comparison_variable),!!as.symbol(main_variable))) +
          geom_vline(xintercept = (data %>% pull(comparison_variable) %>% median(na.rm = T))) +
          geom_hline(yintercept = (data %>% pull(main_variable) %>% median(na.rm = T))) +
          geom_point_interactive(color="grey",
                                 alpha = 0.5,
                                 aes(tooltip=label,
                                     data_id=track_id)) +
          geom_smooth(se=F,
                      color="#1DB954") +
          labs(title = glue("*{str_to_title(str_replace_all(main_variable,'_', ' '))}* versus *{str_to_title(str_replace_all(comparison_variable, '_', ' '))}*"),
               x=str_to_title(str_replace_all(comparison_variable,"_", " ")),
               y=str_to_title(str_replace_all(main_variable,"_", " "))) + 
          theme(plot.title.position = "plot",
                plot.title = element_markdown(size=rel(1.5)))
      }
    } else {
      # Set labels --------------------------------------------------------------
      #If variable=="minutes", use the custom made "duration_label", otherwise use a generic label
      data_scatter <- data %>% 
        rowwise() %>% 
        mutate(x_label=case_when(comparison_variable=="minutes" ~ duration_label,
                                 comparison_variable=="Date_Song_Saved" ~ Song_Saved_Label,
                                 comparison_variable=="Track_Release_Date" ~ Track_Release_Label,
                                 TRUE ~ as.character(!!as.symbol(comparison_variable))),
               y_label=case_when(main_variable=="minutes"~duration_label,
                                 main_variable=="Date_Song_Saved" ~ Song_Saved_Label,
                                 main_variable=="Track_Release_Date" ~ Track_Release_Label,
                                 TRUE ~ as.character(!!as.symbol(main_variable))),
               label=glue("Track: {str_to_title(track_name)} \n{str_to_title(str_replace_all(main_variable, '_', ' '))}: {y_label} \n{str_to_title(str_replace_all(comparison_variable, '_', ' '))}: {x_label} \n Album: {track_album_name} \n Artist: {artist_name}"))
      
      
      # PLot --------------------------------------------------------------------
      data_scatter %>% 
        filter(!!as.symbol(main_variable)!=0) %>% 
        filter(!!as.symbol(comparison_variable)!=0) %>% 
        ggplot(aes(!!as.symbol(comparison_variable),!!as.symbol(main_variable))) +
        geom_vline(xintercept = (data %>% pull(comparison_variable) %>% median(na.rm = T))) +
        geom_hline(yintercept = (data %>% pull(main_variable) %>% median(na.rm = T))) +
        geom_point_interactive(color="grey",
                               alpha = 0.5,
                               aes(tooltip=label,
                                   data_id=track_id)) +
        geom_smooth(se=F,
                    color="#1DB954") +
        labs(title = glue("*{str_to_title(str_replace_all(main_variable,'_', ' '))}* versus *{str_to_title(str_replace_all(comparison_variable, '_', ' '))}*"),
             x=str_to_title(str_replace_all(comparison_variable,"_", " ")),
             y=str_to_title(str_replace_all(main_variable,"_", " "))) + 
        theme(plot.title.position = "plot",
              plot.title = element_markdown(size=rel(1.5)))
    }
  }
}


# scatterplot_function(main_variable = "energy",
#                      comparison_variable = "valence",
#                      playlist_or_track = "track",
#                      data=playlist_tracks)
