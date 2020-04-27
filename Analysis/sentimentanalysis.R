library(tidytext)
library(scales)

covmarch <- read.csv("C:/Users/mthng/OneDrive/Documents/Personal/School/3 Spring 2020/Predictive Analytics (ITEC 621-002)/Project/twittercovid19/WIP/twittercovid19/Data/covid19march.csv")

summary(covmarch)
str(covmarch)

covmarch %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d %H:%M:%S"),
         name = as.character(name),
         text = as.character(text)) %>% 
  rename(id = X) -> 
  covmarch

#Sentiments Analysis
sentiments %>% arrange(word)
get_sentiments("afinn")

get_sentiments("bing")
unique(get_sentiments("bing")$sentiment)

get_sentiments("nrc")
unique(get_sentiments("nrc")$sentiment)

#NRC Sentiments
nrc_words <- get_sentiments("nrc")$word
bing_words <- get_sentiments("bing")$word

c(ncr_words, bing_words) -> word_bank

covmarch %>%
  select(id, date, text) %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  filter(word %in% nrc_words) -> text_df

text_df %>%
  inner_join(get_sentiments("nrc")) -> text_df

text_df %>%
  group_by(id, date, sentiment) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = sentiment, values_from = count) -> sent_count

sent_count[is.na(sent_count)] <- 0

#BY DEVICES/SOURCES
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF", "#FF9999")

covmarch %>%
  group_by(sources) %>%
  summarize(device_retw = sum(retweets)) %>% 
  mutate(prop = round(device_retw/sum(device_retw),2)*100) %>%
  arrange(desc(sources)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)-> pie

pie %>%
  ggplot(aes(x = 2, y = prop, fill = sources)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(y = lab.ypos, label = paste(sources,":", prop,"%", sep = "")), color = "black") +
  scale_fill_manual(values = mycols) +
  theme_void() +
  theme(legend.position = "none") +
  xlim(0.5, 2.5)

#BY DATE
covmarch %>%
  inner_join(sent_count, by = c("id" = "id", "date" = "date")) -> covmarch

covmarch %>% 
  select(date, retweets, everything(), -id, -name, -sources, -text) -> 
  covsent

#Save to CSV
write.csv(covsent, file = "twittercovsentiment.csv", row.names = FALSE)

#TF-IDF
covmarch %>%
  select(date, text) %>%
  unnest_tokens(word, text) %>% 
  mutate(word, str_extract(word, "[a-z']+")) %>%
  count(date, word, sort = TRUE) -> book_words

book_words %>%
  group_by(date) %>%
  summarize(total = sum(n)) ->
  total_words

book_words %>%
  left_join(total_words) ->
  book_words

#Term Frequency Plot
book_words %>% 
  ggplot(aes(n/total, fill = date)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~date, ncol = 2, scales = "free_y")

#TF-IDF
book_words %>% bind_tf_idf(word, date, n) ->
  book_words
book_words

book_words %>%
  filter(!is.numeric(word)) %>%
  mutate(word = fct_reorder(word, tf_idf)) %>%
  mutate(date = factor(date, levels = c("2020-03-01", 
                                        "2020-03-02", 
                                        "2020-03-03", 
                                        "2020-03-04",
                                        "2020-03-05", 
                                        "2020-03-06", 
                                        "2020-03-07", 
                                        "2020-03-08",
                                        "2020-03-09", 
                                        "2020-03-10", 
                                        "2020-03-11", 
                                        "2020-03-12",
                                        "2020-03-13", 
                                        "2020-03-14"
                                        ))) ->
  tweet_plot

str(tweet_plot)

tweet_plot %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(date) %>% 
  top_n(5) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = date)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~date, ncol = 3, scales = "free") +
  coord_flip()