# Dummy file for ensuring directory is added
# Working file
# John Hopkins COVID19 DATA:
# https://github.com/CSSEGISandData/COVID-19

library(tidyverse)
library(keyring)
library(tibble)
library(purrr)
library(devtools)
library(usethis)
library(twitteR)
library(stringr)
library(lubridate)

key_set("key")
key_set("secret")
key_set("atoken")
key_set("asecret")

key <- key_get("key")
secret <- key_get("secret")
atoken <- key_get("atoken")
asecret <- key_get("asecret")

setup_twitter_oauth(key, secret, atoken, asecret)

avail_trends <- availableTrendLocations()

globaltrends <- getTrends(1)
ustrends <- getTrends(23424977)

#covid19

mar1 <- searchTwitter('#covid19', n=3000, since='2020-03-01', until='2020-03-02')
mar2 <- searchTwitter('#covid19', n=3000, since='2020-03-02', until='2020-03-03')
mar3 <- searchTwitter('#covid19', n=3000, since='2020-03-03', until='2020-03-04')
mar4 <- searchTwitter('#covid19', n=3000, since='2020-03-04', until='2020-03-05')
mar5 <- searchTwitter('#covid19', n=3000, since='2020-03-05', until='2020-03-06')
mar6 <- searchTwitter('#covid19', n=3000, since='2020-03-06', until='2020-03-07')
mar7 <- searchTwitter('#covid19', n=3000, since='2020-03-07', until='2020-03-08')
mar8 <- searchTwitter('#covid19', n=3000, since='2020-03-08', until='2020-03-09')
mar9 <- searchTwitter('#covid19', n=3000, since='2020-03-09', until='2020-03-10')
mar10 <- searchTwitter('#covid19', n=3000, since='2020-03-10', until='2020-03-11')

#Combine Lists
covid19 <- c(mar1, mar2, mar3, mar4, mar5, mar6, mar7, mar8, mar9, mar10)

head(covid19)

#Date Created
datecreated <- covid19 %>% map(~.$created)
datecreated <- map(datecreated, ~data.frame(.))
datecreated <- map_dfr(datecreated, ~mutate_all(.,as_datetime))
datecreated <- rename(datecreated, date = .)

#Screen Name
screenname <- covid19 %>% map(~.$screenName)
screenname <- map(screenname, ~data.frame(.))
screenname <- map_dfr(screenname, ~mutate_all(.,as.character()))
screenname <- rename(screenname, name = .)

#text
tweettext <- sapply(covid19,function(x) x$getText())
tweettext <- as.data.frame(tweettext)
tweettext <- rename(tweettext, text = tweettext)
tweettext %>%
  mutate(length = str_length(text)) -> tweettext

#favorite count
favoritecount <- covid19 %>% map(~.$favoriteCount)
favoritecount <- map(favoritecount,~data.frame(.))
favoritecount <- map_dfr(favoritecount,~mutate_all(.,as.integer))
favoritecount <- rename(favoritecount, favorites = .)

#retweet count
retweetcount <- covid19 %>% map(~.$retweetCount)
retweetcount <- map(retweetcount,~data.frame(.))
retweetcount <- map_dfr(retweetcount,~mutate_all(.,as.integer))
retweetcount <- rename(retweetcount, retweets = .)

#sources
statussources <- sapply(covid19,function(x) x$getStatusSource())
statussources <- gsub("</a>","",statussources)
statussources <- strsplit(statussources, ">")
statussources <- sapply(statussources, function(x) ifelse(length(x) > 1, x[2], x[1]))
statussources <- as.data.frame(statussources)
statussources <- rename(statussources, sources = statussources)

statussources %>%
  mutate(sources = as.character(sources)) %>%
  mutate(sources = case_when(str_detect(sources, "iPad") ~"iPad",
                             str_detect(sources, "iPhone") ~ "iPhone",
                             str_detect(sources, "Android") ~"Android",
                             str_detect(sources, "Web") ~"Web",
                             TRUE ~ sources))  -> statussources

statussources$sources = replace(x = statussources$sources, 
                             list =  !statussources$sources %in% c('iPad', 'iPhone', 'Android',
                                                                'Web'),
                             values =  'others')

#tidy data frame
covid19tidy <- cbind(datecreated, screenname, statussources, tweettext, favoritecount, retweetcount)

covid19tidy %>%
  mutate(isretweeted = str_extract_all(text, "RT"),
         isretweeted = ifelse(isretweeted == "RT", TRUE, FALSE),
         text = str_replace_all(text, "RT\\s+", "")) %>%
  select(date, name, sources, isretweeted, text, length, favorites, retweets) -> covid19tidy

#word mining
covid19tidy %>%
  mutate(ishealth = str_count(text, "health"),
         ispandemic = str_count(text, "pandemic"),
         isvirus = str_count(text, "virus"),
         isemergency = str_count(text, "emergency"),
         isdeaths = str_count(str_to_sentence(text), c("dead","death")),
         iswho = str_count(str_to_sentence(text), c("who", "wolrd health organization")),
         isnih = str_count(str_to_sentence(text), c("cdc", "centers for disease control")),
         iswho = str_count(str_to_sentence(text), c("nih", "national institutes of health"))) -> covid19tidy

#Save to CSV
write.csv(covid19tidy, file = "covid19mar110.csv")
