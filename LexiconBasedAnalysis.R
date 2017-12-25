# list of pos and negative words - manually created - approx. 6800 

# download the txt files to your wd

# import positive and negative words
getwd()
pos = readLines("Positive-Words.txt")
neg = readLines("Negative-Words.txt")

# Lets run a test to see how this works!

mytest= c("great you re here", "awesome experience", 
          "You had a bad night", "She loves ugly candy")

# the score.sentiment function is self written
install.packages("sentimentr")
library("sentimentr")


#----
# score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
# {
#   require(plyr)
#   require(stringr)
#   
#   # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
#   # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
#   scores = ?laply(sentences, function(sentence, pos.words, neg.words) {
#     
#     # clean up sentences with R's regex-driven global substitute, gsub():
#     sentence = gsub('[[:punct:]]', '', sentence)
#     sentence = gsub('[[:cntrl:]]', '', sentence)
#     sentence = gsub('\\d+', '', sentence)
#     # and convert to lower case:
#     sentence = tolower(sentence)
#     
#     # split into words. str_split is in the stringr package
#     word.list = str_split(sentence, '\\s+')
#     # sometimes a list() is one level of hierarchy too much
#     words = unlist(word.list)
#     
#     # compare our words to the dictionaries of positive & negative terms
#     pos.matches = match(words, pos.words)
#     neg.matches = match(words, neg.words)
#     
#     # match() returns the position of the matched term or NA
#     # we just want a TRUE/FALSE:
#     pos.matches = !is.na(pos.matches)
#     neg.matches = !is.na(neg.matches)
#     
#     # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
#     score = sum(pos.matches) - sum(neg.matches)
#     
#     return(score)
#   }, pos.words, neg.words, .progress=.progress )
#   
#   scores.df = data.frame(score=scores, text=sentences)
#   return(scores.df)
# }

#----

testsentiment = score.sentiment(mytest, pos, neg)

class (testsentiment)

testsentiment$score
# output corresponds to the 4 test sentences - sentences can be manipulated

# Lets do the whole process: writting the function and scraping - approach after J. Breen

library("stringr")

library("plyr")

# function score.sentiment - this is how the whole function is written
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of postive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  
  # create simple array of scores with laply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation - using global substitute
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits
                   sentence = gsub('\\d+', '', sentence)
                   
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}

testsentiment = score.sentiment(mytest, pos, neg)
class (testsentiment)
testsentiment$score

# Setting uup Twitter:

library("twitteR")
library("httr")
library("tm")
library("SnowballC")

key = ""

secret = ""

accessToken = ""

accessTokenSecret = ""

setup_twitter_oauth(key,secret,accessToken,accessTokenSecret)


# tweets for country
#usatweets = searchTwitter("#usa", n=900, lang="en", since = "2017-12-10", until = "2017-12-10")
usatweets = searchTwitter("#usa", n=900, lang="en")
indiatweets = searchTwitter("#india", n=900, lang="en")
russiatweets = searchTwitter("russia", n=900, lang="en")
chinatweets = searchTwitter("china", n=900, lang="en")

# get text
usa_txt = sapply(usatweets, function(x) x$getText())
india_txt = sapply(indiatweets, function(x) x$getText())
russia_txt = sapply(russiatweets, function(x) x$getText())
china_txt = sapply(chinatweets, function(x) x$getText())

# how many tweets of each country
nd = c(length(usa_txt), length(india_txt), length(russia_txt), length(china_txt))

# join texts
country = c(usa_txt, india_txt, russia_txt, china_txt) 
typeof(country)
# apply function score.sentiment
scores = score.sentiment(country, pos, neg, .progress='text')

# add variables to data frame
scores$country = factor(rep(c("usa", "india", "russia", "china"), nd))
scores$very.pos = as.numeric(scores$score >= 2)
scores$very.neg = as.numeric(scores$score <= -2)

# how many very positives and very negatives
numpos = sum(scores$very.pos)
numneg = sum(scores$very.neg)

# global score
global_score = round( 100 * numpos / (numpos + numneg) )

head(scores)

boxplot(score~country, data=scores, col = c("red", "grey"))

library("lattice")

histogram(data=scores, ~score|country, main="Sentiment Analysis of 4 Countries", col = c("blue", "red", "grey", "green"), xlab="", sub="Sentiment Score")
#-------------------------------

constructWordcloud = function(tweettext){
  
typeof(tweettext)
  tweettext=iconv(tweettext, to= "utf-8", sub="")

mycorpus = Corpus(VectorSource(tweettext))

mycorpus <- tm_map(mycorpus,content_transformer(tolower))
mycorpus <- tm_map(mycorpus, removePunctuation)
mycorpus <- tm_map(mycorpus,removeWords,stopwords("english"))
mycorpus <- tm_map(mycorpus, removeNumbers)

dtm=DocumentTermMatrix(mycorpus)
tdm=TermDocumentMatrix(mycorpus)

dtmMatrix=as.matrix(dtm)
dtmMatrix

tdm2=as.matrix(tdm)
tdm2

frequency=colSums(dtmMatrix)
frequency=sort(frequency,decreasing = TRUE)

library("wordcloud")
library("RColorBrewer")

words=names(frequency)
wordcloud(words[1:100], frequency[1:100])

col <- brewer.pal(5,"Dark2")
wordcloud(words[1:100], frequency[1:100], scale = c(5,1), rot.per = 0, colors= col,random.color=F, max.word=90, random.order=F)
}

constructWordcloud(india_txt)

### removing words out of vectors
mytest = c("dfd", "sadf", "wer")

class(mytest)

myindex = which(mytest == "dfd")

mytest[-myindex]

tweettext2 = which(tweettext == "amp")
