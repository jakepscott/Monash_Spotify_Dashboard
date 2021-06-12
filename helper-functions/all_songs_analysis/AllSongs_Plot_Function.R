AllSongs_Plot <- function(main_variable, comparison_variable, how_many, data) {
  #source(here("helper-functions/Barplot_Function.R"))
  #source(here("helper-functions/Scatterplot_Function.R"))
  
  #Create the bar plot
  barplot <- barplot_function(main_variable = main_variable,
                              how_many = how_many,
                              data = data)
  #Create the scatter plot
  scatterplot <- scatterplot_function(main_variable = main_variable,
                                      comparison_variable = comparison_variable,
                                      data = data)
  #Join bar and scatter plot
  bar_and_scatter <- barplot + scatterplot + plot_layout(ncol = 1)
  
  #Make the plots interactive
  girafe(ggobj = bar_and_scatter,
         options = list(
           opts_selection(type = "multiple",only_shiny = FALSE),
           opts_hover(css = "fill:#1DB954;"),
           opts_hover_inv(css = "opacity:0.25;")))
  
}

#all_songs_function_plot(main_variable = "Track_Release_Date",comparison_variable = "valence", how_many = 10,
#                      data=data)
