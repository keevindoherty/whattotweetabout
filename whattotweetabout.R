install.packages("twitteR")
install.packages("plyr")
install.packages("stingr")
install.packages("ggplot2")

install.packages(c("devtools", "rjson", "bit64", "httr"))
library(devtools)
install_github("twitteR", username="geoffjentry")
library(twitteR)
setup_twitter_oauth(consumerKey, consumerSecret)

#introduce clustering based on recent tweets from followers with variable
#sample size

#place greater weight on hashtags

#option for restrictions within friends or across twitter users