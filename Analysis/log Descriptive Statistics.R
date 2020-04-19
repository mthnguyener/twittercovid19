covid.new <- read.csv("~/Desktop/covid19march.csv")
covid.new <- covid[,-7]
covid.new <- covid[,-4]
covid.new <- covid[,-3]
summary(covid.new)

library(psych)
describe(covid.new)


# normality plots
par(mfrow=c(1,2))
hist(covid.new$retweets, main="Histogram", xlab="Retweets")
qqnorm(covid.new$retweets, main="QQ Plot")
qqline(covid.new$retweets)


# correcting non normality
hist(log(covid.new$retweets),main="Histogram",xlab="Retweets")
qqnorm(log(covid.new$retweets), main="QQ Plot", ylim = c(0,20))
qqline(log(covid.new$retweets))
