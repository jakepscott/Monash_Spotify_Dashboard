

all_songs_function_plot <- function(main_variable, comparison_variable, how_many, data) {

# Load Libs ---------------------------------------------------------------
  # library(patchwork)
  # library(tidyverse)
  # library(glue)
  # library(ggiraph)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Bar Plot ----------------------------------------------------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  bar_plot <- top_and_bottom %>% 
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
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Scatter Plot ----------------------------------------------------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
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
  scatterplot <- data_scatter %>% 
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
  
  bar_and_scatter <- bar_plot + scatterplot + plot_layout(ncol = 1)
  
  girafe(ggobj = bar_and_scatter,
         options = list(
           opts_selection(type = "multiple",only_shiny = FALSE),
           opts_hover(css = "fill:#1DB954;"),
           opts_hover_inv(css = "opacity:0.25;")))
}

#all_songs_function_plot(main_variable = "Track_Release_Date",comparison_variable = "valence", how_many = 10,
 #                      data=full_data)
  