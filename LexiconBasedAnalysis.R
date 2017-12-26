#Lexicon-Based Sentiment Analysis: A list of words are used to compare your scrapped text

#These words are taken from Hu and Liu's Lexicon list (https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html)
#They act as a dictionary and have a list of positive and negative words (6800 words approximately) 

#Set your working directory
setwd("")

#Download the text files and place it in your directory

#Import the positive and negative words
pos = readLines("Positive-Words.txt")
neg = readLines("Negative-Words.txt")

#Example:
mytest= c("I love John Cena", "Cute cat!", "That was a bad night", "She loves bad boys")
testsentiment = score.sentiment(mytest, pos, neg)
testsentiment
#Output:
#                  text        score
#1     I love John Cena            1
#2            Cute cat!            1
#3 That was a bad night           -1
#4   She loves bad boys            0


#Installing the sentimentr package
#install.packages("sentimentr")
library("sentimentr")

# Lets do the whole process: writting the function and scraping - approach after J. Breen

library("stringr")

library("plyr")

#We follow Jeffrey Breen's approach and create the score.sentiment function
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  #Parameters
  #sentences: a vector of text
  #pos.words: a vector of words of postive sentiment
  #neg.words: a vector of words of negative sentiment
  #.progress: passed to laply() to control of progress bar: It shows the time elapsed in the console/terminal
  
  #Creating an array of scores using laply
  scores = laply(sentences, function(sentence, pos.words, neg.words)
                 {
                   #Removing punctuation using global substitute
                   sentence = gsub("[[:punct:]]", "", sentence)
                   #Removing control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   #Removing digits
                   sentence = gsub('\\d+', '', sentence)
                   
                   #Error handling while trying to get the text in lower case
                   tryTolower = function(x)
                   {
                     #Create missing value
                     y = NA
                     #Try Catch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     #If not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     #Return the result
                     return(y)
                   }
                                          
                   #Use this tryTolower function in sapply
                   sentence = sapply(sentence, tryTolower)
                   
                   #Split sentences into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   #Unlist produces a vector which contains all atomic components in word.list
                   words = unlist(word.list)
                   
                   #Compare these words to the dictionaries of positive & negative words
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   #Example: If a sentence is "He is a good boy", 
                   #then, pos.matches returns: [NA, NA, NA, *some number*, NA] : the number depends on the severity of the word is my guess
                   #neg.matches returns: [NA, NA, NA, NA, NA] 
                   #So the output has NAs and numbers
                 
                   #We just want a TRUE/FALSE value for the pos.matches & neg.matches
                   #Getting the position of the matched term or NA
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   #This would return : [F, F, F, T, F] depending on the NA or the match
                                          
                   #The TRUE or FALSE values are treated as 1/0
                   #To get the final score of the sentence:
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  
  #Now the scores are put in a dataframe and returned
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}
#Testing our 'mytest' sentences
testsentiment = score.sentiment(mytest, pos, neg)
class (testsentiment)
testsentiment$score
testsentiment
            
#Setting up Twitter:
            
#Install the required packages
#install.packages("twitteR")

#twitteR acts as an interface between R and Twitter and helps to scrap Twitter data
library("twitteR")
library("httr")
library("tm")
library("SnowballC")

#Create a Developer account in Twitter : https://apps.twitter.com
#Create a new App

#Get the key, secret, access token and access token secret from the Keys and Tokens tab
key = ""
secret = ""
accessToken = ""
accessTokenSecret = ""

#Download the cacert.pem file and store it in your working directory
#Authenticate handshake. Get the PIN and just run it in R
#This comppletes the handshake between R and Twitter
setup_twitter_oauth(key,secret,accessToken,accessTokenSecret)

#Now R is connected to Twitter. Start mininig!
#Note that Twitter allows only 15 scrapes in 15 minutes
#The Twitter Search API searches against a sampling of recent Tweets published in the past 7 days.
#So only tweets in the last 7 days can be mined

#Compare tweets of countries:
#Get the tweets of USA, India, China and Syria
#usatweets = searchTwitter("usa", n=1000, lang="en", since = "2017-12-10", until = "2017-12-10")
usatweets = searchTwitter("#USA", n=1000, lang="en")
indiatweets = searchTwitter("#India", n=1000, lang="en")
chinatweets = searchTwitter("#China", n=1000, lang="en")
syriatweets = searchTwitter("#Syria", n=1000, lang="en")

#Since it is Christmas today, getting the #MerryChristmas tweets :D
christmastweets = searchTwitter("#MerryChristmas", n=1000, lang="en")

#Get only texts from the tweets
usa_txt = sapply(usatweets, function(x) x$getText())
india_txt = sapply(indiatweets, function(x) x$getText())
china_txt = sapply(chinatweets, function(x) x$getText())
syria_txt = sapply(syriatweets, function(x) x$getText())
christmas_txt = sapply(christmastweets, function(x) x$getText())

#Counting the number of tweets for each country
count = c(length(usa_txt), length(india_txt), length(china_txt), length(syria_txt), length(christmas_txt))

#Joining texts
country = c(usa_txt, india_txt, china_txt, syria_txt, christmas_txt) 

#Use the score.sentiment function
scores = score.sentiment(country, pos, neg, .progress='text')

#Adding variables to 'scores' dataframe
scores$country = factor(rep(c("USA", "India", "China", "Syria", "Merry Christmas"), count))

#Calculating the number of very positive and very negative tweets
scores$very.pos = as.numeric(scores$score >= 2)
scores$very.neg = as.numeric(scores$score <= -2)
numpos = sum(scores$very.pos)
numneg = sum(scores$very.neg)

#Calculating the global sentiment score
global_score = round( 100 * numpos / (numpos + numneg) )
head(scores)

#Plotting a Box Plot of these countries
#We observe that USA, India and #MerryChristmas have a positive sentiment score, while Syria's sentiment score is negative!
boxplot(score~country, data=scores, col = c("red", "grey"))

#Importing the 'lattice' package for data visualization
library("lattice")

#Plotting a histogram
histogram(data=scores, ~score|country, main="Sentiment Analysis of 4 Countries", col = c("red", "grey"), xlab="", sub="Sentiment Score")

#Writing the Word Cloud function
constructWordcloud = function(tweettext){
  
  #Converting them to UTF-8
  tweettext=iconv(tweettext, to= "utf-8", sub="")
  #Putting them in a Corpus
  mycorpus = Corpus(VectorSource(tweettext))
  #Data Cleaning processes
  mycorpus <- tm_map(mycorpus,content_transformer(tolower))
  mycorpus <- tm_map(mycorpus, removePunctuation)
  mycorpus <- tm_map(mycorpus,removeWords,stopwords("english"))
  mycorpus <- tm_map(mycorpus, removeNumbers)
  
  #Converting them to a DocumentTermMatrix & TermDocumentMatrix
  dtm=DocumentTermMatrix(mycorpus)
  tdm=TermDocumentMatrix(mycorpus)
  
  dtmMatrix=as.matrix(dtm)
  dtmMatrix
  
  tdm2=as.matrix(tdm)
  tdm2
  
  #Getting the frequency of the words
  frequency=colSums(dtmMatrix)
  frequency=sort(frequency,decreasing = TRUE)
  
  #Importing the 'wordcloud' & 'RColorBrewer' packages
  library("wordcloud")
  library("RColorBrewer")
  
  #Get the names of the words
  words=names(frequency)
  #Basic Word Cloud
  wordcloud(words[1:100], frequency[1:100])
  
  #Advanced Word Cloud
  col <- brewer.pal(5,"Dark2")
  wordcloud(words[1:100], frequency[1:100], scale = c(4,1), rot.per = 0, colors= col,random.color=F, max.word=100, random.order=F)
}

#Constructing Word Clouds for the countries
constructWordcloud(india_txt)
constructWordcloud(usa_txt)
constructWordcloud(china_txt)
constructWordcloud(syria_txt)

#Constructing Word Cloud for '#MerryChristmas'
constructWordcloud(christmas_txt)

#If you want to remove specific words from the tweets:
mytest = c("Su", "Cena", "love")
myindex = which(mytest == "love")
mytest[-myindex]
