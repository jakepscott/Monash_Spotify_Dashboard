
song_scatter_plot_function <- function(x_axis, y_axis, data){
  if (x_axis=="minutes") {
    x_axis <- "duration_label"
  }
  
  if (y_axis=="minutes") {
    y_axis <- "duration_label"
  }
  
  # Set labels --------------------------------------------------------------
  #If variable=="minutes", use the custom made "duration_label", otherwise use a generic label
  data_scatter <- data %>% 
    mutate(x_value=!!as.symbol(x_axis),
           y_value=!!as.symbol(y_axis),
           label=glue("Track: {str_to_title(track_name)} \n{str_to_title(y_axis)}: {y_value} \n{str_to_title(x_axis)}:{x_value} \n Album: {track_album_name} \n Artist: {artist_name}"))
  
  # PLot --------------------------------------------------------------------
  plot <- data_scatter %>% 
     ggplot(aes(!!as.symbol(x_axis),!!as.symbol(y_axis))) +
     geom_vline(xintercept = (data %>% pull(x_axis) %>% median(na.rm = T))) +
     geom_hline(yintercept = (data %>% pull(y_axis) %>% median(na.rm = T))) +
     geom_point_interactive(color="grey",
                            alpha = 0.5,
                            aes(tooltip=label,
                                data_id=track_id)) +
     geom_smooth(se=F,
                 color="#1DB954") +
     labs(title = glue("*{str_to_title(y_axis)}* versus *{str_to_title(x_axis)}*"),
          x=str_to_title(x_axis),
          y=str_to_title(y_axis)) + 
     theme(plot.title.position = "plot",
           plot.title = element_markdown(size=rel(2)))
  
  girafe(ggobj = plot,
         options = list(
           opts_hover(css = "fill:#1DB954;"),
           opts_hover_inv(css = "opacity:0.25;")
         ))
}


