# INSTALL REQUIRED PACKAGES
############################

install.packages("plyr")
install.packages("stingr")
install.packages("ggplot2")

install.packages("RTextTools")
install.packages("RSiteCatalyst")

install.packages("sentiment")
install.packages("wordcloud")

install.packages(c("devtools", "rjson", "bit64"))
install_github("httr", username="hadley")
install_github("twitteR", username="geoffjentry")
install_github("sentiment140", username="okugami79")

# ALL LIBRARIES USED
############################

library(RTextTools)
library(RSiteCatalyst)
library(devtools)
library(tm)
library(httr)
library(twitteR)
library(igraph)
library(plyr)
library(stringr)
library(ggplot2)

library(sentiment)
library(wordcloud)
library(RColorBrewer)

# OAuth Setup
############################

load(file="twitter_auth.Rdata")
setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessSecret)


# CREATE TEXT CORPUS
############################

numTweets = 1000
tweetBatch <- homeTimeline(n=numTweets, maxID = NULL, sinceID = NULL)
print(length(tweetBatch))
wordList <- {}
for (i in 1:(length(tweetBatch)-1)){ 
tweetText <- tweetBatch[[i]]$getText()
wordList <- c(wordList,unlist(strsplit(tweetText, " ")))
}
dtm <- create_matrix(wordList,
				stemWords = FALSE,
				removeStopwords = TRUE,
				minWordLength = 2,
				removePunctuation = TRUE)
print(dtm)
findFreqTerms(dtm, lowfreq = 15)

## Running a Twitter search

searchTerm <- readline("What search would you like to know about?")
print(searchTerm)
searchTerm.list <- searchTwitter(searchTerm, n=1000)
searchTerm.df = twListToDF(searchTerm.list)

score.sentiment = function(sentences, pos.words, neg.words, .progress ='no')
{
require(plyr)
require(stringr)
scores = laply(sentences, function(sentence, pos.words, neg.words){
	sentence = gsub('[[:punct:]]', '', sentence)
	sentence = gsub('[[:cntrl:]]', '', sentence)
	sentence = gsub('\\d+', '', sentence)
	sentence = tolower(sentence)
	word.list = str_split(sentence, '\\s+')
	words = unlist(word.list)
	pos.matches = match(words, pos.words)
	neg.matches = match(words, neg.words)
	pos.matches = !is.na(pos.matches)
	neg.matches = !is.na(neg.matches)
	score = sum(pos.matches) - sum(neg.matches)
	return(score)
}, pos.words, neg.words, .progress=.progress )
scores.df = data.frame(score=scores, text=sentences)
return(scores.df)
}

## Loading Sentiment Lists

hu.liu.pos = scan('./positive-words.txt',
			what='character',
			comment.char=';')
hu.liu.neg = scan('./negative-words.txt',
			what='character',
			comment.char=';')
pos.words = c(hu.liu.pos, 'upgrade')
neg.words = c(hu.liu.neg, 'wtf', 'wait', 'waiting', 'epicfail')

DatasetSearchTerm <- searchTerm.df
DatasetSearchTerm$text <- gsub('[^[:alnum:]/S]', ' ', DatasetSearchTerm$text)
DatasetSearchTerm$text <- as.factor(DatasetSearchTerm$text)

##Score the tweets
searchTerm.scores = score.sentiment(DatasetSearchTerm$text, pos.words, neg.words, .progress='text')
hist(searchTerm.scores$score)
qplot(searchTerm.scores$score)

#gt <- getText()
#searchTerm_txt = lapply(searchTerm.list, gt)
searchTerm_txt = sapply(searchTerm.list, function(x) x$getText())

searchTerm_txt = gsub("(RT|via)((?:\\b\\w*@\\w+)+)", "", searchTerm_txt)
searchTerm_txt = gsub("@\\w+", "", searchTerm_txt)
searchTerm_txt = gsub("[[:punct:]]", "", searchTerm_txt)
searchTerm_txt = gsub("[[:digit:]]", "", searchTerm_txt)
searchTerm_txt = gsub("httpl\\w+", "", searchTerm_txt)
searchTerm_txt = gsub("[ \t]{2,}", "", searchTerm_txt)
searchTerm_txt = gsub("^\\s+|\\s+$", "", searchTerm_txt)

try.error = function(x)
{
	y = NA
	try_error = tryCatch(tolower(x), error=function(e) e)
	if(!inherits(try_error, "error"))
		y = tolower(x)
	return(y)
}
searchTerm_txt = sapply(searchTerm_txt, try.error)
searchTerm_txt = searchTerm_txt[!is.na(searchTerm_txt)]
names(searchTerm_txt) = NULL

## Classify Emotion
class_emo = sentiment(searchTerm_txt)
qplot(class_emo$polarity)
)