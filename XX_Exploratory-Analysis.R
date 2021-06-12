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
  summarise(across(where(is.numeric), median, na.rm=T),
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
  summarise(across(where(is.numeric), median, na.rm=T),
            n=n()) %>% 
  pivot_longer(danceability:n, names_to = "feature") %>%
  ggplot(aes(month,value,fill=feature)) +
  geom_col(show.legend = F) +
  facet_wrap(~feature, scales = "free_y")


# Rolling median ----------------------------------------------------------
data %>% 
  pivot_longer(danceability:duration_ms, names_to = "feature") %>% 
  select(date,feature,value) %>% 
  arrange(date,feature) %>% 
  group_by(feature) %>% 
  mutate(rolling_val=rollmedian(value,k = 14,fill = NA,align = "right")) %>% 
  filter(year(date)>=2020) %>% 
  ggplot(aes(date,rolling_val,color=feature)) +
  geom_line(show.legend = F) +
  geom_vline(xintercept = as.Date("2020-03-01")) +
  facet_wrap(~feature, scales = "free_y")


# Playling with Playlists -------------------------------------------------
playlist_tracks_feats <- read_rds(here("data/playlist_tracks.rds"))
#playlist_tracks %>% count(track_name,sort=T) %>% count(n, sort=T)

playlists_agg <- playlist_tracks_feats %>% 
  group_by(playlist) %>% 
  summarise(across(.cols = c(added_at:loudness), median, na.rm=T))

playlists_agg %>% 
  select(added_at, playlist) %>% 
  arrange(added_at) %>% 
  View()
