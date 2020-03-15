#Pull covid19 Tweets
mar13 <- searchTwitter('#covid19', n=3000, since='2020-03-13', until='2020-03-14')
mar14 <- searchTwitter('#covid19', n=3000, since='2020-03-14', until='2020-03-15')

#Combine Lists
covid19 <- c(mar13, mar14)

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
         iscdc = str_count(str_to_sentence(text), c("cdc", "centers for disease control")),
         isnih = str_count(str_to_sentence(text), c("nih", "national institutes of health")),
         isdisease = str_count(str_to_sentence(text), "disease"), 
         isquarantine = str_count(str_to_sentence(text), "quarantine"), 
         isrecover = str_count(str_to_sentence(text), "recover"),
         isban = str_count(str_to_sentence(text), "ban"), 
         iscoronavirus = str_count(str_to_sentence(text), "coronavirus"),
         iscovid19 = str_count(str_to_sentence(text), "covid19"), 
         iswash = str_count(str_to_sentence(text), "wash"), 
         isracist = str_count(str_to_sentence(text), c("racist","racism")), 
         isasian = str_count(str_to_sentence(text), "asian"), 
         ischinese = str_count(str_to_sentence(text), c("chinese", "china")), 
         isinfectious = str_count(str_to_sentence(text), c("infectious", "infections"))) -> covid19tidy

covid19tidy %>%
  mutate(sources = as.factor(sources)) ->
  covid19tidy

#Read current CSV
covid19current <- read.csv("C:/Users/mthng/OneDrive/Documents/Personal/School/3 Spring 2020/Predictive Analytics (ITEC 621-002)/Project/twittercovid19/WIP/twittercovid19/Data/covid19march.csv")

covid19current %>%
  mutate(date = as_datetime(date),
         name = as.character(name),
         text = as.character(text)) %>%
  select(-X)->
  covid19current

#Combine
covid19march <- rbind(covid19current, covid19tidy)

#Save to CSV
write.csv(covid19march, file = "covid19march.csv")