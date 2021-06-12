Playlist_Plot <- function(method, main_variable, comparison_variable, 
                          how_many, data){
  #If the method chosen is to aggregate and compare across playlists
  if (method=="compare_playlists") {
    #Aggregate the data
    data_agg <- aggregate_data(main_variable = main_variable, 
                               comparison_variable = comparison_variable,
                               data=data)
    #Create the bar plot
    barplot <- barplot_function(main_variable = main_variable,
                                how_many = how_many,
                                data = data_agg)
    #Create the scatter plot
    scatterplot <- scatterplot_function(main_variable = main_variable,
                                        comparison_variable = comparison_variable,
                                        data = data_agg)
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

