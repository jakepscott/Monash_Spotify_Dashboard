# Load Libs and data ---------------------------------------------------------------
library(tidyverse)
library(here)
library(lubridate)
library(zoo)

data <- read_rds(here("data/full_data.rds"))
theme_set(theme_minimal())


# Add Date columns --------------------------------------------------------
data <- data %>% 
  select(added_at,danceability:time_signature) %>% 
  select(added_at:energy, loudness, speechiness:tempo,duration_ms) %>% 
  mutate(date=str_remove_all(added_at,"T.*"),
         date=ymd(date),
         year_month=as.yearmon(date))

# Variables over time -----------------------------------------------------
 data %>% 
  select(-added_at,-date) %>% 
  group_by(year_month) %>% 
  summarise(across(where(is.numeric), mean, na.rm=T),
            n=n()) %>% 
  pivot_longer(danceability:n, names_to = "feature") %>%
  ggplot(aes(year_month,value,color=feature)) +
  geom_smooth(show.legend = F) +
  geom_line(show.legend = F) +
  geom_vline(xintercept = as.yearmon("2020-03-01")) +
  facet_wrap(~feature, scales = "free_y")


# Features by Month -------------------------------------------------------
data %>% 
  mutate(month=month(date,label = T)) %>% 
  group_by(month) %>% 
  summarise(across(where(is.numeric), mean, na.rm=T),
            n=n()) %>% 
  pivot_longer(danceability:n, names_to = "feature") %>%
  ggplot(aes(month,value,fill=feature)) +
  geom_col(show.legend = F) +
  facet_wrap(~feature, scales = "free_y")
