# Load Libs and data ---------------------------------------------------------------
library(tidyverse)
library(here)
library(lubridate)
library(zoo)
library(ggtext)
library(glue)
library(ggiraph)
windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))

data <- read_rds(here("data/full_data.rds"))
theme_set(theme_minimal(base_family = "Roboto Condensed",
                        base_size=12))


# Set params --------------------------------------------------------------
variable <- "valence"
how_many <- 10

# Obain Data --------------------------------------------------------------
#Get the top "how_many" songs by chosen variable
top <- data %>% 
  slice_max(order_by = !!as.symbol(variable),
            n=how_many) %>% 
  mutate(top_or_bot="top")

#Get the bottom "how_many" songs by chosen variable
bottom <- data %>% 
  slice_min(order_by = !!as.symbol(variable),
            n=how_many) %>% 
  mutate(top_or_bot="bottom")

#Combine top and bottom tracks
top_and_bottom <- top %>% 
  bind_rows(bottom) 


# Set Label ---------------------------------------------------------------
top_and_bottom <- top_and_bottom %>% 
  mutate(value=!!as.symbol(variable),
         label=glue("Track: {str_to_title(track_name)} \n{str_to_title(variable)}: {value} \n Album: {track_album_name} \n Artist: {artist_name}"))

  


# Plot --------------------------------------------------------------------
(top_and_bottom_plot <- top_and_bottom %>% 
  ggplot(aes(fct_reorder(track_name,!!as.symbol(variable)),!!as.symbol(variable))) +
  geom_col_interactive(aes(fill=top_or_bot,
                           tooltip=label,
                           data_id=track_id), 
                       show.legend = F) +
  scale_x_discrete(labels=function(x) str_wrap(str_to_title(x), 30)) +
  scale_fill_manual(values=c("grey","#1DB954")) +
  coord_flip() +
  labs(x=NULL,
       y=NULL,
       title = glue("<span style = 'color: #1DB954;'>**Top**</span> versus <span style = 'color: grey;'>**bottom**</span> {how_many} songs by ***{(variable)}***")) +
  theme(plot.title.position = "plot",
        plot.title = element_markdown()))


girafe(ggobj = top_and_bottom_plot,
       options = list(
         opts_hover(css="fill:green;"),
         opts_hover_inv(css = "opacity:0.25;")
       ))

