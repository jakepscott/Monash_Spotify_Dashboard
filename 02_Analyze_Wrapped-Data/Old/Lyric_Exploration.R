library(wordcloud)
Lyrics$Lyrics[[i]] %>% 
  tibble(word=.) %>%
  unnest_tokens(output = "word",input = "word",token = "words")


negate_words <- c("not", "no", "never", "won't", "don't")
Lyrics$Lyrics[[i]] %>% 
  tibble(word=.) %>%
  unnest_tokens(output = "word",input = "word",token = "ngrams", n=2) %>% 
  separate(word,into=c("word1","word2"),sep = " ") %>% 
  filter(word1 %in% negate_words)

  

Lyrics$Lyrics[[i]] %>% paste(collapse = " ")

Full_Lyrics <- Full_Lyrics %>% mutate(full_lyrics="a")

for (i in 1:nrow(Lyrics)) {
  Full_Lyrics$full_lyrics[i] <- Full_Lyrics$Lyrics[[i]] %>% paste(collapse = " ")
}


songs <- tracks %>% select(Id,Song,Playlist)
words <- songs %>% 
  left_join(Full_Lyrics) %>% 
  select(Id,Song,full_lyrics,Playlist) %>% 
  unnest_tokens(input = full_lyrics,output = "words",token="words")

interesting_words <- words %>% anti_join(stop_words, by=c("words"="word")) %>% filter(!(words %in% c("ya","yea","yeah","oh","ohh","ooh","ay","ayy","uh",
                                                                                                     "uh","nigga","niggas","fuck","bitch","bitches",
                                                                                                     "ass","fucks","fucking","shit","gon")))

top_words <- interesting_words %>% 
  count(Playlist,words) %>% 
  arrange(desc(n)) %>% 
  group_by(Playlist) %>% 
  top_n(15, n) %>% 
  mutate(words=reorder_within(x = words,within = Playlist,by = n))


ggplot(top_words, aes(words, n, fill = Playlist)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "count") +
  facet_wrap(~Playlist, ncol = 2, scales = "free") +
  coord_flip() +
  scale_x_reordered()

wordcloud(words = top_words$words, freq = top_words$n, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
