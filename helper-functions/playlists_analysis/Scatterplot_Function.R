scatterplot_function <- function(main_variable, how_many, data){
  if ("playlist_name" %in% names(data)) {
    
  } else if ("track_name" %in% names(data)) {
    
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
            plot.title = element_markdown(size=rel(2)))
    
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Bar Plot ----------------------------------------------------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
}


barplot_function(main_variable = "minutes", "Track_Release_Date",how_many = 5, data = full_data)
