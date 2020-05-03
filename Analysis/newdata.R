#Pull covid19 Tweets
mar13 <- searchTwitter('#covid19', n=3000, since='2020-03-13', until='2020-03-14')
mar14 <- searchTwitter('#covid19', n=3000, since='2020-03-14', until='2020-03-15')

apr19 <- searchTwitter('#covid19', n=3000, since='2020-04-19', until='2020-04-20')
apr20 <- searchTwitter('#covid19', n=3000, since='2020-04-20', until='2020-04-21')
apr21 <- searchTwitter('#covid19', n=3000, since='2020-04-21', until='2020-04-22')
apr22 <- searchTwitter('#covid19', n=3000, since='2020-04-22', until='2020-04-23')
apr23 <- searchTwitter('#covid19', n=3000, since='2020-04-23', until='2020-04-24')
apr24 <- searchTwitter('#covid19', n=3000, since='2020-04-24', until='2020-04-25')
apr25 <- searchTwitter('#covid19', n=3000, since='2020-04-25', until='2020-04-26')
apr26 <- searchTwitter('#covid19', n=3000, since='2020-04-26', until='2020-04-27')
apr27 <- searchTwitter('#covid19', n=3000, since='2020-04-27', until='2020-04-28')
apr28 <- searchTwitter('#covid19', n=3000, since='2020-04-28', until='2020-04-29')
apr29 <- searchTwitter('#covid19', n=3000, since='2020-04-29', until='2020-04-30')
apr30 <- searchTwitter('#covid19', n=3000, since='2020-04-30', until='2020-05-01')

#Combine Lists
covid19 <- c(apr19, apr20, apr21, apr22, apr23, apr24, apr25, apr26, apr27, apr28, apr29, apr30)

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

covid19current <- covid19tidy

#OR Read current CSV
covid19current <- read.csv("C:/Users/mthng/OneDrive/Documents/Personal/School/3 Spring 2020/Predictive Analytics (ITEC 621-002)/Project/twittercovid19/WIP/twittercovid19/Data/covid19march.csv")

summary(covid19current)
str(covid19current)

covid19current %>%
  mutate(date = as.Date(date, format = "%Y/%M/%D"),
         name = as.character(name),
         text = as.character(text)) ->
  covid19current

#Combine
covid19current <- rbind(covid19current, covid19tidy)

#Add Confirmed Counts from counts.R for China, Italy, Others and US
covid19current %>%
  left_join(epi_total, by = c("date" = "Date")) ->
  covid19current

#Save to CSV
write.csv(covid19current, file = "covid19apr10apr30.csv")
