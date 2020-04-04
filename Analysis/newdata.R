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

#text mining
covid19tidy %>%
  mutate(ishealth = str_count(text, "health"),
         ispandemic = str_count(text, "pandemic"),
         isvirus = str_count(text, "virus"),
         isemergency = str_count(text, "emergency"),
         isdeaths = str_count(str_to_sentence(text), c("dead","death")),
         iswho = str_count(str_to_sentence(text), c("who", "world health organization")),
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

summary(covid19current)

covid19current %>%
  mutate(date = as_datetime(date),
         name = as.character(name),
         text = as.character(text)) %>%
  select(-X)->
  covid19current

#Combine
covid19current <- rbind(covid19current, covid19tidy)

#Add Confirmed Counts for China, Italy, Others and US
covid19current <- covid19current %>%
  mutate(date = ymd_hms(date))

covid19current <- covid19current %>%
  mutate(ch.confirmed = case_when(day(date) == 1 ~ confirmed.total[[1,2]],
                           day(date) == 2 ~ confirmed.total[[1,3]],
                           day(date) == 3 ~ confirmed.total[[1,4]],
                           day(date) == 4 ~ confirmed.total[[1,5]],
                           day(date) == 5 ~ confirmed.total[[1,6]],
                           day(date) == 6 ~ confirmed.total[[1,7]],
                           day(date) == 7 ~ confirmed.total[[1,8]],
                           day(date) == 8 ~ confirmed.total[[1,9]],
                           day(date) == 9 ~ confirmed.total[[1,10]],
                           day(date) == 10 ~ confirmed.total[[1,11]],
                           day(date) == 11 ~ confirmed.total[[1,12]],
                           day(date) == 12 ~ confirmed.total[[1,13]],
                           day(date) == 13 ~ confirmed.total[[1,14]],
                           day(date) == 14 ~ confirmed.total[[1,15]]))

covid19current <- covid19current %>%
  mutate(it.confirmed = case_when(day(date) == 1 ~ confirmed.total[[2,2]],
                                  day(date) == 2 ~ confirmed.total[[2,3]],
                                  day(date) == 3 ~ confirmed.total[[2,4]],
                                  day(date) == 4 ~ confirmed.total[[2,5]],
                                  day(date) == 5 ~ confirmed.total[[2,6]],
                                  day(date) == 6 ~ confirmed.total[[2,7]],
                                  day(date) == 7 ~ confirmed.total[[2,8]],
                                  day(date) == 8 ~ confirmed.total[[2,9]],
                                  day(date) == 9 ~ confirmed.total[[2,10]],
                                  day(date) == 10 ~ confirmed.total[[2,11]],
                                  day(date) == 11 ~ confirmed.total[[2,12]],
                                  day(date) == 12 ~ confirmed.total[[2,13]],
                                  day(date) == 13 ~ confirmed.total[[2,14]],
                                  day(date) == 14 ~ confirmed.total[[2,15]]))

covid19current <- covid19current %>%
  mutate(ot.confirmed = case_when(day(date) == 1 ~ confirmed.total[[3,2]],
                                  day(date) == 2 ~ confirmed.total[[3,3]],
                                  day(date) == 3 ~ confirmed.total[[3,4]],
                                  day(date) == 4 ~ confirmed.total[[3,5]],
                                  day(date) == 5 ~ confirmed.total[[3,6]],
                                  day(date) == 6 ~ confirmed.total[[3,7]],
                                  day(date) == 7 ~ confirmed.total[[3,8]],
                                  day(date) == 8 ~ confirmed.total[[3,9]],
                                  day(date) == 9 ~ confirmed.total[[3,10]],
                                  day(date) == 10 ~ confirmed.total[[3,11]],
                                  day(date) == 11 ~ confirmed.total[[3,12]],
                                  day(date) == 12 ~ confirmed.total[[3,13]],
                                  day(date) == 13 ~ confirmed.total[[3,14]],
                                  day(date) == 14 ~ confirmed.total[[3,15]]))

covid19current <- covid19current %>%
  mutate(us.confirmed = case_when(day(date) == 1 ~ confirmed.total[[4,2]],
                                  day(date) == 2 ~ confirmed.total[[4,3]],
                                  day(date) == 3 ~ confirmed.total[[4,4]],
                                  day(date) == 4 ~ confirmed.total[[4,5]],
                                  day(date) == 5 ~ confirmed.total[[4,6]],
                                  day(date) == 6 ~ confirmed.total[[4,7]],
                                  day(date) == 7 ~ confirmed.total[[4,8]],
                                  day(date) == 8 ~ confirmed.total[[4,9]],
                                  day(date) == 9 ~ confirmed.total[[4,10]],
                                  day(date) == 10 ~ confirmed.total[[4,11]],
                                  day(date) == 11 ~ confirmed.total[[4,12]],
                                  day(date) == 12 ~ confirmed.total[[4,13]],
                                  day(date) == 13 ~ confirmed.total[[4,14]],
                                  day(date) == 14 ~ confirmed.total[[4,15]]))

#Add Death Counts for China, Italy, Others and US
covid19current <- covid19current %>%
  mutate(ch.deaths = case_when(day(date) == 1 ~ deaths.total[[1,2]],
                                  day(date) == 2 ~ deaths.total[[1,3]],
                                  day(date) == 3 ~ deaths.total[[1,4]],
                                  day(date) == 4 ~ deaths.total[[1,5]],
                                  day(date) == 5 ~ deaths.total[[1,6]],
                                  day(date) == 6 ~ deaths.total[[1,7]],
                                  day(date) == 7 ~ deaths.total[[1,8]],
                                  day(date) == 8 ~ deaths.total[[1,9]],
                                  day(date) == 9 ~ deaths.total[[1,10]],
                                  day(date) == 10 ~ deaths.total[[1,11]],
                                  day(date) == 11 ~ deaths.total[[1,12]],
                                  day(date) == 12 ~ deaths.total[[1,13]],
                                  day(date) == 13 ~ deaths.total[[1,14]],
                                  day(date) == 14 ~ deaths.total[[1,15]]))

covid19current <- covid19current %>%
  mutate(it.deaths = case_when(day(date) == 1 ~ deaths.total[[2,2]],
                                  day(date) == 2 ~ deaths.total[[2,3]],
                                  day(date) == 3 ~ deaths.total[[2,4]],
                                  day(date) == 4 ~ deaths.total[[2,5]],
                                  day(date) == 5 ~ deaths.total[[2,6]],
                                  day(date) == 6 ~ deaths.total[[2,7]],
                                  day(date) == 7 ~ deaths.total[[2,8]],
                                  day(date) == 8 ~ deaths.total[[2,9]],
                                  day(date) == 9 ~ deaths.total[[2,10]],
                                  day(date) == 10 ~ deaths.total[[2,11]],
                                  day(date) == 11 ~ deaths.total[[2,12]],
                                  day(date) == 12 ~ deaths.total[[2,13]],
                                  day(date) == 13 ~ deaths.total[[2,14]],
                                  day(date) == 14 ~ deaths.total[[2,15]]))

covid19current <- covid19current %>%
  mutate(ot.deaths = case_when(day(date) == 1 ~ deaths.total[[3,2]],
                                  day(date) == 2 ~ deaths.total[[3,3]],
                                  day(date) == 3 ~ deaths.total[[3,4]],
                                  day(date) == 4 ~ deaths.total[[3,5]],
                                  day(date) == 5 ~ deaths.total[[3,6]],
                                  day(date) == 6 ~ deaths.total[[3,7]],
                                  day(date) == 7 ~ deaths.total[[3,8]],
                                  day(date) == 8 ~ deaths.total[[3,9]],
                                  day(date) == 9 ~ deaths.total[[3,10]],
                                  day(date) == 10 ~ deaths.total[[3,11]],
                                  day(date) == 11 ~ deaths.total[[3,12]],
                                  day(date) == 12 ~ deaths.total[[3,13]],
                                  day(date) == 13 ~ deaths.total[[3,14]],
                                  day(date) == 14 ~ deaths.total[[3,15]]))

covid19current <- covid19current %>%
  mutate(us.deaths= case_when(day(date) == 1 ~ deaths.total[[4,2]],
                                  day(date) == 2 ~ deaths.total[[4,3]],
                                  day(date) == 3 ~ deaths.total[[4,4]],
                                  day(date) == 4 ~ deaths.total[[4,5]],
                                  day(date) == 5 ~ deaths.total[[4,6]],
                                  day(date) == 6 ~ deaths.total[[4,7]],
                                  day(date) == 7 ~ deaths.total[[4,8]],
                                  day(date) == 8 ~ deaths.total[[4,9]],
                                  day(date) == 9 ~ deaths.total[[4,10]],
                                  day(date) == 10 ~ deaths.total[[4,11]],
                                  day(date) == 11 ~ deaths.total[[4,12]],
                                  day(date) == 12 ~ deaths.total[[4,13]],
                                  day(date) == 13 ~ deaths.total[[4,14]],
                                  day(date) == 14 ~ deaths.total[[4,15]]))

           
#Save to CSV
write.csv(covid19current, file = "covid19march.csv")
