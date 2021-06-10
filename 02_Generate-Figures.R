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
variable <- "minutes"
how_many <- 10


# Plot --------------------------------------------------------------------
top_10 <- data %>% 
  slice_max(order_by = !!as.symbol(variable),
            n=how_many) %>% 
  mutate(top_or_bot="top")

bottom_10 <- data %>% 
  slice_min(order_by = !!as.symbol(variable),
            n=how_many) %>% 
  mutate(top_or_bot="bottom")


top_and_bottom <- top_10 %>% 
  bind_rows(bottom_10) 

#Create label (this is so we can have a fancy duration label)
if (variable=="minutes") {
  top_and_bottom <- top_and_bottom %>% 
    mutate(value=!!as.symbol(variable),
           label=glue("{str_to_title(track_name)}: {duration_label}"))
} else {
  top_and_bottom <- top_and_bottom %>% 
    mutate(value=!!as.symbol(variable),
           label=glue("{str_to_title(track_name)}: {value}"))
}
  

(top_and_bottom_plot <- top_and_bottom %>% 
  ggplot(aes(fct_reorder(track_name,!!as.symbol(variable)),!!as.symbol(variable))) +
  geom_col_interactive(aes(fill=top_or_bot,
                           tooltip=label,
                           data_id=track_id), 
                       show.legend = F) +
  scale_x_discrete(labels=function(x) str_wrap(str_to_title(x), 30)) +
  scale_fill_manual(values=c("red","blue")) +
  coord_flip() +
  labs(x=NULL,
       y=NULL,
       title = glue("<span style = 'color: blue;'>**Top**</span> versus <span style = 'color: red;'>**bottom**</span> {how_many} songs by *{str_to_title(variable)}*")) +
  theme(plot.title.position = "plot",
        plot.title = element_markdown()))


girafe(ggobj = top_and_bottom_plot)

