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
