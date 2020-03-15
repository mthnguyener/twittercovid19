#Ordinary Least Squares (OLS) - retweets and favorites total
retweets.ols <- lm(retweets ~ length + favorites + ishealth + ispandemic + 
                     isvirus + isemergency + isdeaths + iswho + iscdc + isnih + 
                     isdisease + isquarantine + isrecover + isban + iscoronavirus + 
                     iscovid19 + iswash + isracist + isasian + ischinese + isinfectious, 
                   data = covid19march)
summary(retweets.ols)

favorites.ols <- lm(favorites ~ length + retweets + ishealth + ispandemic + 
                      isvirus + isemergency + isdeaths + iswho + iscdc + isnih + 
                      isdisease + isquarantine + isrecover + isban + iscoronavirus + 
                      iscovid19 + iswash + isracist + isasian + ischinese + isinfectious, 
                    data = covid19march)
summary(favorites.ols)

#Generalized Linear Models (GLM) - is it a retweet?
isretweeted.glm <- glm(isretweeted ~ length + favorites + retweets + ishealth + ispandemic + 
                         isvirus + isemergency + isdeaths + iswho + iscdc + isnih + 
                         isdisease + isquarantine + isrecover + isban + iscoronavirus + 
                         iscovid19 + iswash + isracist + isasian + ischinese + isinfectious, 
                       data = covid19march)
summary(isretweeted.glm)

#Regression Tree - Retweets total
tree.retweets <- tree(retweets ~ length + favorites + ishealth + ispandemic + 
                        isvirus + isemergency + isdeaths + iswho + iscdc + isnih + 
                        isdisease + isquarantine + isrecover + isban + iscoronavirus + 
                        iscovid19 + iswash + isracist + isasian + ischinese + isinfectious, 
                      data = covid19march)
summary(tree.retweets)
plot(tree.retweets)
text(tree.retweets)

#Classification Tree - Is it a retweet?
tree.isretweeted <- tree(isretweeted ~ length + favorites + retweets + ishealth + ispandemic + 
                           isvirus + isemergency + isdeaths + iswho + iscdc + isnih + 
                           isdisease + isquarantine + isrecover + isban + iscoronavirus + 
                           iscovid19 + iswash + isracist + isasian + ischinese + isinfectious, 
                         data = covid19march)
summary(tree.isretweeted)
plot(tree.isretweeted)
text(tree.isretweeted)
