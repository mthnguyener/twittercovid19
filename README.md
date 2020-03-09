# twittercovid19

<!-- badges: start -->

<!-- badges: end -->

The goal of this project is to gather covid19 related tweets and analyze how retweets and favorites are impacted.

Business Understanding:
Are retweets and favorites affected by which words are being used in the tweet? Can we increase retweets and favorites by using key words?
Our hypothesis is that words implying fear will have a positive impact on reteweets and favorites since people typically react to fear.

Data Understanding:
datecreated - date tweet was created
screenname - the screen name of the person posting the tweet
tweettext - the content of the tweet
favoritecount - the number of favorites
retweetcount - the number of retweets
statussources - device used to post the tweet

Data Preparation:
Covid19 data from feb 29, 2020 to mar 8, 2020 were pulled from Twitter. 

Step 1) Combine the selected dates into one big list, map desired variables (created,  screenName, favoriteCount, retweetCount), pull StatusSource, pull tweet content and clean the data.
Step 2) Identify retweets and clean tweet text. 
Step 3) Concatenated into a data frame, covid19tidy.
Step 4) Text mining using words of interest

Modeling:
Evaluate 3 different modeling methods with 2 different specifications for each, 6 different models (3 model methods x 2 specifications)
LM
GLM
Decision Tree

Evaluation:

Deployment (Report):


