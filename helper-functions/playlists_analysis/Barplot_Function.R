#This function takes as input a feature to plot, the number of bars, and a data object. It automatically
# detects whether this is an aggregated tibble with playlists and their median values, or a disaggregated
# playlist-track pairing dataset. 
barplot_function <- function(main_variable, how_many, data){
  if ("playlist_name" %in% names(data)) {
    #Obain Top and bottom data  --------------------------------------------------------------
    ##Get the top "how_many" songs by chosen variable
    top <- data %>% 
      slice_max(order_by = !!as.symbol(main_variable),
                n=how_many, with_ties = F) %>% 
      mutate(top_or_bot="top")
    
    #Get the bottom "how_many" songs by chosen variable
    bottom <- data %>% 
      filter(!!as.symbol(main_variable)!=0) %>% 
      slice_min(order_by = !!as.symbol(main_variable),
                n=how_many, with_ties = F) %>% 
      mutate(top_or_bot="bottom")
    
    #Combine top and bottom tracks
    top_and_bottom <- top %>% 
      bind_rows(bottom) 
    
    
    # Set Label ---------------------------------------------------------------
    top_and_bottom <- top_and_bottom %>% 
      mutate(value=!!as.symbol(main_variable),
             label = glue("Playlist: {str_to_title(playlist_name)} \n{str_to_title(str_replace_all(main_variable, '_', ' '))}: {value}"))
    
    # Plot --------------------------------------------------------------------
    top_and_bottom %>% 
      ggplot(aes(fct_reorder(str_wrap(playlist_name,35),!!as.symbol(main_variable)),!!as.symbol(main_variable))) +
      geom_col_interactive(aes(fill=top_or_bot,
                               tooltip=label,
                               data_id=playlist_id), 
                           show.legend = F) +
      scale_fill_manual(values=c("grey","#1DB954")) +
      coord_flip() +
      labs(x=NULL,
           y=NULL,
           title = glue("<span style = 'color: #1DB954;'>**Top**</span> versus <span style = 'color: grey;'>**bottom**</span> {how_many} playlists by median *{str_replace_all(main_variable, '_', ' ')}*")) +
      theme(plot.title.position = "plot",
            plot.title = element_markdown(size=rel(1.25)),
            axis.text.y = element_text(size=rel(.6)))
  } else if ("track_name" %in% names(data)) {
    #Obain Top and bottom data  --------------------------------------------------------------
    ##Get the top "how_many" songs by chosen variable
    top <- data %>% 
      slice_max(order_by = !!as.symbol(main_variable),
                n=how_many, with_ties = F) %>% 
      mutate(top_or_bot="top")
    
    #Get the bottom "how_many" songs by chosen variable
    bottom <- data %>% 
      filter(!!as.symbol(main_variable)!=0) %>% 
      slice_min(order_by = !!as.symbol(main_variable),
                n=how_many, with_ties = F) %>% 
      mutate(top_or_bot="bottom")
    
    #Combine top and bottom tracks
    top_and_bottom <- top %>% 
      bind_rows(bottom) 
    
    
    # Set Label ---------------------------------------------------------------
    top_and_bottom <- top_and_bottom %>% 
      mutate(value=!!as.symbol(main_variable),
             label = case_when(main_variable== "minutes" ~glue ("Track: {str_to_title(track_name)} \n{str_to_title(str_replace_all(main_variable, '_', ' '))}: {duration_label} \n Album: {track_album_name} \n Artist: {artist_name}"),
                               main_variable== "Date_Song_Saved" ~ glue("Track: {str_to_title(track_name)} \n{str_to_title(str_replace_all(main_variable, '_', ' '))}: {Song_Saved_Label} \n Album: {track_album_name} \n Artist: {artist_name}"),
                               main_variable=="Track_Release_Date" ~ glue("Track: {str_to_title(track_name)} \n{str_to_title(str_replace_all(main_variable, '_', ' '))}: {Track_Release_Label} \n Album: {track_album_name} \n Artist: {artist_name}"),
                               TRUE~glue("Track: {str_to_title(track_name)} \n{str_to_title(str_replace_all(main_variable, '_', ' '))}: {value} \n Album: {track_album_name} \n Artist: {artist_name}")))
    
    # Plot --------------------------------------------------------------------
    top_and_bottom %>% 
      ggplot(aes(fct_reorder(str_wrap(track_name,35),!!as.symbol(main_variable)),!!as.symbol(main_variable))) +
      geom_col_interactive(aes(fill=top_or_bot,
                               tooltip=label,
                               data_id=track_id), 
                           show.legend = F) +
      scale_fill_manual(values=c("grey","#1DB954")) +
      coord_flip() +
      labs(x=NULL,
           y=NULL,
           title = glue("<span style = 'color: #1DB954;'>**Top**</span> versus <span style = 'color: grey;'>**bottom**</span> {how_many} songs by *{str_replace_all(main_variable, '_', ' ')}*")) +
      theme(plot.title.position = "plot",
            plot.title = element_markdown(size=rel(1.5)),
            axis.text.y = element_text(size=rel(.6)))
  }
}
  

