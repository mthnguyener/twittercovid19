library(tidyverse)
library(tibble)
library(purrr)
library(stringr)
library(lubridate)
library(tree)
library(dplyr)
library(tidytext)
library(scales)
library(keyring)
library(devtools)
library(usethis)
library(twitteR)
library(countrycode)

# John Hopkins COVID19 DATA:
# https://github.com/CSSEGISandData/COVID-19
# https://github.com/CSSEGISandData/COVID-19.git

key_set("key")
key_set("secret")
key_set("atoken")
key_set("asecret")

key <- key_get("key")
secret <- key_get("secret")
atoken <- key_get("atoken")
asecret <- key_get("asecret")

setup_twitter_oauth(key, secret, atoken, asecret)
