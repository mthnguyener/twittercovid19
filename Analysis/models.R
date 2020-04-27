#Read current CSV
covid19current <- read.csv("C:/Users/mthng/OneDrive/Documents/Personal/School/3 Spring 2020/Predictive Analytics (ITEC 621-002)/Project/twittercovid19/WIP/twittercovid19/Data/covid19march.csv", header=TRUE, row.names=1, sep=",")

summary(covid19current)
str(covid19current)

covid19current %>%
  mutate(date = as_datetime(date),
         name = as.character(name),
         text = as.character(text)) ->
  covid19current

#Generalized Linear Models (GLM) - retweets
retweets.glm <- glm(retweets ~ isretweeted + length + favorites + ishealth + ispandemic + 
                     isvirus + isemergency + isdeaths + iswho + iscdc + isnih + 
                     isdisease + isquarantine + isrecover + isban + iscoronavirus + 
                     iscovid19 + iswash + isracist + isasian + ischinese + isinfectious  +
                     ch.confirmed + it.confirmed + ot.confirmed + us.confirmed + 
                     ch.deaths + it.deaths + ot.deaths + us.deaths, 
                   data = covid19current)
summary(retweets.glm)

#Generalized Linear Models (GLM) - retweets with NRC SENTIMENTS
covidsent<- read.csv("C:/Users/mthng/OneDrive/Documents/Personal/School/3 Spring 2020/Predictive Analytics (ITEC 621-002)/Project/twittercovid19/WIP/twittercovid19/Data/twittercovsentiment.csv", header=TRUE)

summary(covidsent)
str(covidsent)

covidsent %>%
  select(-date) ->
  covidsent

retweets.sent.glm <- glm(retweets ~ ., data = covidsent)
summary(retweets.sent.glm)

covidsent %>%
  select(-c(5:23)) -> sentonly
  
sent.glm <- glm(retweets ~ ., data = sentonly)
summary(sent.glm)

#Regression Tree - Retweets total
tree.retweets <- tree(retweets ~ isretweeted + length + favorites + ishealth + ispandemic + 
                        isvirus + isemergency + isdeaths + iswho + iscdc + isnih + 
                        isdisease + isquarantine + isrecover + isban + iscoronavirus + 
                        iscovid19 + iswash + isracist + isasian + ischinese + 
                        isinfectious  + ch.confirmed + it.confirmed + ot.confirmed + 
                        us.confirmed + ch.deaths + it.deaths + ot.deaths + us.deaths, 
                      data = covid19current)
summary(tree.retweets)
plot(tree.retweets)
text(tree.retweets)

#Regression Tree - Retweets total with NRC SENTIMENTS
tree.sent.retweets <- tree(retweets ~ ., data = covidsent)
summary(tree.sent.retweets)
plot(tree.sent.retweets)
text(tree.sent.retweets)

sent.tree <- tree(retweets ~ ., data = sentonly)
summary(sent.tree)
plot(sent.tree)
text(sent.tree)

#Classification Tree - Is it a retweet?
tree.isretweeted <- tree(isretweeted ~ length + favorites + retweets + ishealth + 
                           ispandemic + isvirus + isemergency + isdeaths + iswho + 
                           iscdc + isnih + isdisease + isquarantine + isrecover + 
                           isban + iscoronavirus + iscovid19 + iswash + isracist + 
                           isasian + ischinese + isinfectious + ch.confirmed + 
                           it.confirmed + ot.confirmed + us.confirmed + ch.deaths + 
                           it.deaths + ot.deaths + us.deaths, 
                         data = covid19current)
summary(tree.isretweeted)
plot(tree.isretweeted)
text(tree.isretweeted)
