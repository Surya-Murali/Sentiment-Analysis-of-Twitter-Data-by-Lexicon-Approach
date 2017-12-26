# Analysis-by-Lexicon-Approach

This project uses Lexicon-based approach for sentimental analysis of 1000 recent tweets of 4 countries - USA, India, China and Syria.
The general idea is to calculate a sentiment score for each tweet and thereby find out how positive or negative the tweet is.

The sentiment score for each tweet is calculated as follows:          
Score  =  Number of positive words  -  Number of negative words
* If Score > 0, the tweet has an overall 'positive opinion'
* If Score < 0, the tweet has an overall 'negative opinion'
* If Score = 0, the tweet is considered to be a 'neutral opinion'

In order to count the number of positive and negative words in a tweet, an Opinion Lexicon in English is used.
It is provided by Hu & Liu and has [Positive](https://github.com/Surya-Murali/Analysis-by-Lexicon-Approach/blob/master/Positive-Words.txt) and [Negative](https://github.com/Surya-Murali/Analysis-by-Lexicon-Approach/blob/master/Negative-Words.txt) Words list. It is taken from: http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html

Example:

mytest= c("I love John Cena", "Cute cat!", "That was a bad night", "She loves bad boys")
testsentiment = score.sentiment(mytest, pos, neg)
testsentiment

Output:
                     text          score
 1       I love John Cena              1
 2              Cute cat!              1
 3   That was a bad night             -1
 4     She loves bad boys              0
 
 **Note :** 
* The code for Twitter-R connection and analysis can be found [here](https://github.com/Surya-Murali/Analysis-by-Lexicon-Approach/blob/master/LexiconBasedAnalysis.R)
* The [results](https://github.com/Surya-Murali/Analysis-by-Lexicon-Approach/tree/master/Outputs) are shown in the form of as Box Plots, histograms and Word Clouds
* Twitter allows only 15 scrapes in 15 minutes
* The Twitter Search API searches against a sampling of recent Tweets published in the past 7 days. So only tweets in the last 7 days can be mined
* Merry Christmas :P
