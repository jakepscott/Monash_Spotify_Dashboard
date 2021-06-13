Playlist_Plot <- function(method, main_variable, comparison_variable, 
                          how_many, data){
  single_playlist <- data %>% 
    filter(playlist_name=="New 27")
  
  data_agg <- aggregate_data(main_variable = main_variable, 
                             comparison_variable = comparison_variable,
                             data=data)
  
  if (method=="compare_playlists") {
    #Create the bar plot
    barplot <- barplot_function(main_variable = main_variable,
                                how_many = how_many,
                                playlist_or_track = "playlist",
                                data = data_agg)
    #Create the scatter plot
    scatterplot <- scatterplot_function(main_variable = main_variable,
                                        comparison_variable = comparison_variable,
                                        playlist_or_track = "playlist",
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
    #Create the bar plot
    barplot <- barplot_function(main_variable = main_variable,
                                how_many = how_many,
                                playlist_or_track = "track",                              
                                data = single_playlist)
    #Create the scatter plot
    scatterplot <- scatterplot_function(main_variable = main_variable,
                                        comparison_variable = comparison_variable,
                                        playlist_or_track = "track",
                                        data = single_playlist)
    #Join bar and scatter plot
    bar_and_scatter <- barplot + scatterplot + plot_layout(ncol = 1)
    
    #Make the plots interactive
    girafe(ggobj = bar_and_scatter,
           options = list(
             opts_selection(type = "multiple",only_shiny = FALSE),
             opts_hover(css = "fill:#1DB954;"),
             opts_hover_inv(css = "opacity:0.25;")))
  }
}

