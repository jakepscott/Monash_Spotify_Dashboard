# Loading Libraries and Data-------------------------------------------------------
library(readr)
library(tidyverse)
library(tidytext)
library(ggridges)
library(lexicon)
library(ggtext)
library(here)
library(patchwork)
library(stringr)
library(scales)
library(tools)

data <- read_rds(here("data/Full_Wrapped_Feat_Lyrics_Data.rds"))
comparison_data <- read_rds(here("data/Wrapped_Playlist_Data.rds"))
comparison_data <- comparison_data %>% rename("Median Loudness (dB)"=`Median Loudness`,
                                              "Median Tempo (BPM)"=`Median Tempo`,
                                              "Percent of Songs That Are Explicit"=`Percent Explicit`)

theme_set(theme_minimal(base_size = 12))

# Features ----------------------------------------------------------------
comparison_data %>% 
  select(Playlist,`Median Danceability`:`Median Years Since Release (Adj)`,`Average Words Per Song`,`Percent of Songs That Are Explicit`) %>% 
  select(-`Median Key (0 is C)`) %>% 
  pivot_longer(`Median Danceability`:`Percent of Songs That Are Explicit`,names_to="Feature") %>% 
  ggplot(aes(x=Playlist,y=value,group=1)) +
  geom_line(color="#1DB954",size=1.5) +
  geom_point(color="#1DB954",size=5) +
  facet_wrap(~Feature,scales = "free_y") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme(axis.title = element_blank(),
        plot.title.position = "plot",
        strip.text = element_text(size=rel(1.5)))



# Genres ------------------------------------------------------------------
genres <- comparison_data %>% select(Playlist,`Percent Hip Hop`:`Percent Other`)

genres %>% pivot_longer(cols=`Percent Hip Hop`:`Percent Other`,names_to="Genre",values_to="Percent") %>% 
  ggplot(aes(x=Playlist,y=Percent,group=1)) +
  geom_line(color="#1DB954",size=1.5) +
  geom_point(color="#1DB954",size=5) +
  facet_wrap(~Genre,scales = "free_y") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme(axis.title = element_blank(),
        plot.title.position = "plot",
        strip.text = element_text(size=rel(1.5)))

# NRC Sentiments --------------------------------------------------------------

sentiments <- comparison_data %>% select(Playlist,`Percent of Words in Trust Category`:`Percent of Words in Anticipation Category`)
names(sentiments) <- names(sentiments) %>% str_replace("Percent of Words in ","") %>% str_replace(" Category","")


sentiments %>% 
  pivot_longer(cols=Trust:Anticipation,
               names_to="Genre",
               values_to="Percent") %>% 
  ggplot(aes(x=Playlist,y=Percent,group=1)) +
  geom_line(color="#1DB954",size=1.5) +
  geom_point(color="#1DB954",size=5) +
  facet_wrap(~Genre,scales = "free_y") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
  labs(title="Percent of Words in Given Sentiment Cateogry") +
  theme(plot.title = element_text(size = rel(2)),
        axis.title = element_blank(),
        plot.title.position = "plot",
        strip.text = element_text(size=rel(1.5)))

# Overall Sentiment -------------------------------------------------
comparison_data %>% 
  select(Playlist, `Average Overall Sentiment`,`Average Corrected Sentiment`) %>% 
  pivot_longer(cols = `Average Overall Sentiment`:`Average Corrected Sentiment`,names_to="correct",values_to="sentiment") %>% 
  ggplot(aes(x=Playlist,y=sentiment,color=correct,group=correct)) +
  geom_line(size=1.5) +
  geom_point(size=5) +
  geom_hline(yintercept = 0,linetype="dashed") +
  scale_color_manual(values = c("#1DB954","#A9A9A9")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  guides(color="none") +
  labs(title="<span style='color: #1DB954'>**Corrected**</span> versus <span style='color: #A9A9A9'>**Uncorrected**</span> Overall Sentiment",
       subtitle = "Corrected means I account for \"negation\" words like \"not\"") +
  theme_minimal() +
  theme(plot.title=element_markdown(),
        axis.title.x = element_blank(),
        plot.title.position = "plot")

# Top Artists -------------------------------------------------------------
Artists <- data %>% count(Playlist,Artist) %>% arrange(desc(n)) %>% 
  group_by(Playlist) %>% 
  top_n(5, n) %>% 
  mutate(Artist=reorder_within(x = Artist,within = Playlist,by = n))

Artists$Playlist <- factor(Artists$Playlist,
                           levels=c("Your Top Songs 2017","Your Top Songs 2018","Your Top Songs 2019","Your Top Songs 2020"),
                           labels=c("2017","2018","2019","2020"))

(p1 <- ggplot(Artists, aes(Artist, n, fill = Playlist)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "count") +
    facet_wrap(~Playlist, ncol = 2, scales = "free_y") +
    coord_flip() +
    scale_x_reordered() +
    scale_fill_manual(values = c("#5BC680","#1DB954","#16873D","#1B3B26")) +
    labs(title="Top Artists by Year",
         y="Songs") +
    theme(plot.title = element_text(size = rel(2)),
          plot.title.position = "plot",
          axis.text.y = element_text(face="bold")))

#Number distinct artists
(p2 <- data %>% select(Playlist,Artist) %>% distinct(Playlist,Artist) %>% 
    group_by(Playlist) %>% 
    summarise(Distinct_Artists=n()) %>% 
    ggplot(aes(x=Playlist,y=Distinct_Artists,group=1)) +
    geom_line(color="#1DB954",size=1.5) +
    geom_point(color="#1DB954",size=5) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    labs(title="Number of Distinct Artists") +
    theme(plot.title = element_text(size = rel(2)),
          axis.title = element_blank(),
          plot.title.position = "plot"))

p1+p2 + plot_layout(ncol=1)


# Lyric Analysis ----------------------------------------------------------
Lyrics <- data %>% select(Playlist,Id,Song,full_lyrics)

words <- Lyrics %>% 
  unnest_tokens(input = full_lyrics,output = "words",token="words",to_lower = F) %>% 
  filter(words!="NA") %>%  #Removing "NA" from the lyrics. This is an artifact of the genius API, which sometimes has the first line as missing
  mutate(words= tolower(words))

# Top Clean Words ---------------------------------------------------------------

interesting_words <- words %>% 
  #Removing stop words
  anti_join(stop_words, by=c("words"="word")) %>% 
  #Removing racist and profane words
  anti_join(tibble(words=lexicon::profanity_racist)) %>% 
  anti_join(tibble(words=lexicon::profanity_alvarez)) %>% 
  #Removing more stop words
  filter(!(words %in% c("ya","yea","yeah","oh","ohh","ooh","ay","ayy","uh","gon"))) 

top_5_words <- interesting_words %>% 
  count(Playlist,words) %>% 
  arrange(desc(n)) %>% 
  group_by(Playlist) %>% 
  mutate(total=sum(n),
         percent=(n/total)*100) %>% 
  top_n(5, percent) %>% 
  mutate(words=str_to_title(words)) %>% 
  mutate(words=reorder_within(x = words,within = Playlist,by = percent))

top_5_words$Playlist <- factor(top_5_words$Playlist,
                               levels=c("Your Top Songs 2017","Your Top Songs 2018","Your Top Songs 2019","Your Top Songs 2020"),
                               labels=c("2017","2018","2019","2020"))


ggplot(top_5_words, aes(words, n, fill = Playlist)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "percent") +
  facet_wrap(~Playlist, ncol = 2, scales = "free_y") +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() +
  scale_x_reordered() +
  scale_fill_manual(values = c("#5BC680","#1DB954","#16873D","#1B3B26")) +
  labs(title="Top Words by Year",
       y="Percent") +
  theme(plot.title = element_text(size = rel(2)),
        plot.title.position = "plot",
        axis.text.y = element_text(face="bold"))

# Top Explicit Words ---------------------------------------------------------------
interesting_words_explicit <- words %>% 
  #Removing stop words
  anti_join(stop_words, by=c("words"="word")) %>% 
  #Removing more stop words
  filter(!(words %in% c("ya","yea","yeah","oh","ohh","ooh","ay","ayy","uh","gon"))) 

top_5_words_explicit <- interesting_words_explicit %>% 
  count(Playlist,words) %>% 
  arrange(desc(n)) %>% 
  group_by(Playlist) %>% 
  mutate(total=sum(n),
         percent=(n/total)*100) %>% 
  top_n(5, percent) %>% 
  mutate(words_clean=toTitleCase(words),
         words_clean=case_when(
           words %in% lexicon::profanity_racist~str_replace_all(string = words_clean,pattern = "A|a|e|i|o|u",replacement = "*"),
           words %in% lexicon::profanity_alvarez~str_replace_all(string = words_clean,pattern = "A|a|e|i|o|u",replacement = "*"),
    TRUE~words_clean)) %>%
  mutate(words_clean=reorder_within(x = words_clean,within = Playlist,by = percent))

top_5_words_explicit$Playlist <- factor(top_5_words_explicit$Playlist,
                               levels=c("Your Top Songs 2017","Your Top Songs 2018","Your Top Songs 2019","Your Top Songs 2020"),
                               labels=c("2017","2018","2019","2020"))


ggplot(top_5_words_explicit, aes(words_clean, percent, fill = Playlist)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "percent") +
  facet_wrap(~Playlist, ncol = 2, scales = "free_y") +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() +
  scale_x_reordered() +
  scale_fill_manual(values = c("#5BC680","#1DB954","#16873D","#1B3B26")) +
  labs(title="Top Words by Year",
       y="Percent") +
  theme(plot.title = element_text(size = rel(2)),
        plot.title.position = "plot",
        axis.text.y = element_text(face="bold"))

# Percent of Explicit Words -----------------------------------------------

Per_Explicit_Words <- words %>% 
  mutate(explicit=ifelse(words %in% lexicon::profanity_racist | words %in% lexicon::profanity_alvarez,1,0)) %>% 
  group_by(Playlist) %>% 
  summarise(Percent_Explicit=mean(explicit,na.rm = T)) 

Per_Explicit_Words %>% 
  ggplot(aes(x=Playlist,y=Percent_Explicit,group=1)) +
  geom_line(color="#1DB954",size=1.5) +
  geom_point(color="#1DB954",size=5) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  labs(title="Percent of all Words that are Explicit") +
  theme(axis.title = element_blank(),
        plot.title.position = "plot",
        strip.text = element_text(size=rel(1.5)))

  
  

# Most Unique Words -------------------------------------------------------
#source(here("Analysis/Getting_Relative_Importance.R"))
Relative_Importance <- read_rds(here("data/Relative_Importance.rds"))

Top_10_Relative_Importance <- Relative_Importance %>% 
  select(Playlist,word,difference) %>%
  distinct() %>% 
  group_by(Playlist) %>% 
  top_n(5, difference) %>%
  mutate(word_clean=toTitleCase(word),
         word_clean=case_when(
           word %in% lexicon::profanity_racist~str_replace_all(string = word_clean,pattern = "A|a|e|i|o|u",replacement = "*"),
           word %in% lexicon::profanity_alvarez~str_replace_all(string = word_clean,pattern = "A|a|e|i|o|u",replacement = "*"),
           TRUE~word_clean)) %>%
  mutate(word_clean=reorder_within(x = word_clean,within = Playlist,by = difference))



Top_10_Relative_Importance$Playlist <- factor(Top_10_Relative_Importance$Playlist,
                                              levels=c("Your Top Songs 2017","Your Top Songs 2018","Your Top Songs 2019","Your Top Songs 2020"),
                                              labels=c("2017","2018","2019","2020"))

Top_10_Relative_Importance %>%
  ggplot(aes(word_clean, difference, fill = Playlist)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "count") +
  facet_wrap(~Playlist, ncol = 2, scales = "free_y") +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() +
  scale_x_reordered() +
  scale_fill_manual(values = c("#5BC680","#1DB954","#16873D","#1B3B26")) +
  labs(title="Most Uniquely Important Words for Each Year",
       subtitle = "Percent of words made up by word X in year Y minus percent of words made up of word X outside of year Y",
       y="Proportional Importance (Percent)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(size = rel(2)),
        plot.title.position = "plot",
        axis.text.y = element_text(face="bold")) 
