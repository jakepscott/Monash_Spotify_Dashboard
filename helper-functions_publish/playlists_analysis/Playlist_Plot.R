Playlist_Plot <- function(method, 
                          main_variable, 
                          comparison_variable, 
                          playlist_of_interest,
                          playlists_to_compare,
                          bar_playlist_or_track="playlist", 
                          scatter_playlist_or_track="playlist",
                          how_many, 
                          data){
  #Set up data
  full_data <- data %>% select(-contains("playlist"))
  
  data_agg <- aggregate_data(main_variable = main_variable, 
                             comparison_variable = comparison_variable,
                             data=data)
  #Implement method
  if (method=="compare_playlists") {
    #Create the bar plot
    barplot <- barplot_function(main_variable = main_variable,
                                how_many = how_many,
                                playlist_or_track = bar_playlist_or_track,
                                data = data_agg)
    #Create the scatter plot
    scatterplot <- scatterplot_function(main_variable = main_variable,
                                        comparison_variable = comparison_variable,
                                        playlist_or_track = scatter_playlist_or_track,
                                        data = data_agg)
    #Join bar and scatter plot
    bar_and_scatter <- barplot + scatterplot + plot_layout(ncol = 1)
    
    #Make the plots interactive
    girafe(ggobj = bar_and_scatter,
           options = list(
             opts_selection(type = "multiple",only_shiny = FALSE),
             opts_hover(css = "fill:#1DB954;"),
             opts_hover_inv(css = "opacity:0.25;")))
    
  } else if (method=="songs_within_playlist") {
    single_playlist <- data %>% 
      filter(playlist_name==playlist_of_interest)
    #Create the bar plot
    barplot <- barplot_function(main_variable = main_variable,
                                how_many = how_many,
                                playlist_or_track = bar_playlist_or_track,                              
                                data = single_playlist)
    #Create the scatter plot
    scatterplot <- scatterplot_function(main_variable = main_variable,
                                        comparison_variable = comparison_variable,
                                        playlist_or_track = scatter_playlist_or_track,
                                        data = single_playlist)
    #Join bar and scatter plot
    bar_and_scatter <- barplot + scatterplot + plot_layout(ncol = 1)
    
    #Make the plots interactive
    girafe(ggobj = bar_and_scatter,
           options = list(
             opts_selection(type = "multiple",only_shiny = FALSE),
             opts_hover(css = "fill:#1DB954;"),
             opts_hover_inv(css = "opacity:0.25;")))
  } else if (method=="songs_across_playlists") {
    playlists_to_compare_data <- data %>% 
      filter(playlist_name %in% playlists_to_compare)
    #Create the bar plot
    barplot <- barplot_function(main_variable = main_variable,
                                how_many = how_many,
                                playlist_or_track = bar_playlist_or_track,   
                                #The distinct here is so if one song appears across multiple playlists, it only
                                data = playlists_to_compare_data %>% distinct(track_name, .keep_all = T))
    #Create the scatter plot
    scatterplot <- scatterplot_function(main_variable = main_variable,
                                        comparison_variable = comparison_variable,
                                        playlist_or_track = scatter_playlist_or_track,
                                        data = playlists_to_compare_data)
    #Join bar and scatter plot
    bar_and_scatter <- barplot + scatterplot + plot_layout(ncol = 1)
    
    #Make the plots interactive
    girafe(ggobj = bar_and_scatter,
           options = list(
             opts_selection(type = "multiple",only_shiny = FALSE),
             opts_hover(css = NULL),
             opts_hover_inv(css = "opacity:0.25;")))
  }
}

