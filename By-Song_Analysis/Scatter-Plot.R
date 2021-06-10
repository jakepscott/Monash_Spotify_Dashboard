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
x_axis <- "date"
y_axis <- "energy"


# PLot --------------------------------------------------------------------
(plot <- data %>% 
  ggplot(aes(!!as.symbol(x_axis),!!as.symbol(y_axis))) +
  geom_vline(xintercept = (data %>% pull(x_axis) %>% median(na.rm = T))) +
  geom_hline(yintercept = (data %>% pull(y_axis) %>% median(na.rm = T))) +
  geom_point_interactive(color="grey",
                         alpha = 0.5,
                         aes(tooltip=track_name)) +
  geom_smooth(se=F,
              color="#1DB954"))

ggiraph(ggobj = plot)

