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

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = ?laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentences[1])
    sentence = gsub('[[:cntrl:]]', '', sentences[1])
    sentence = gsub('\\d+', '', sentences[1])
    # and convert to lower case:
    sentence = tolower(sentences[2])
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos)
    neg.matches = match(words, neg)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

#----

testsentiment = score.sentiment(mytest, pos, neg)

str (testsentiment)

testsentiment$text
testsentiment$score
