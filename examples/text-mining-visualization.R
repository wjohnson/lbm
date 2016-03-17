library(tm)
library(tau)
library(ngram)
library(wordcloud)
library(arules)

library(XML)
#Ubuntu Users see...
#http://stackoverflow.com/a/7765470/950280

library(RCurl)
#Ubuntu users see...
#http://stackoverflow.com/a/20695549/950280

story_url <-"https://www.gutenberg.org/files/2591/2591-h/2591-h.htm"

download.file(story_url,"grimm.html")

#TRIM Function from...
#http://stackoverflow.com/a/2261149/950280
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
#Slight change to remove multiple spaces to one space
trim2 <- function(x) gsub("\\s\\s*", " ", x)

clean_string <-function(x){
  #Remove special characters from string
  chars <- c('[',']','^','$','.','|',
             '?','*','+','(',')')
  pattern <- paste(paste('\\',chars,'|',sep=""),collapse="")
  pattern <- substr(pattern, 1, nchar(pattern)-1)
  return(gsub(pattern,'',x))
}
escape_string <- function(x){
  #Escape special characters from a string
  chars <- c('[',']','^','$','.','|',
             '?','*','+','(',')')
  for (char in chars){
    pattern <- paste('\\',char,sep="")
    x <- gsub(pattern,paste('\\\\',char,sep=''),x)
  }
  return(x)
}

#Connect to file we downloaded
#Read through it and combine into single string
f<-file("grimm.html")
story_data <-readLines(f)
story_data <- paste(story_data,sep="",collapse="")
story_data <-trim(story_data)
close(f)

#Look for anything between an h2 tag
#This will be used to distinguish story borders
match <- gregexpr('<h2>(.+?)</h2>',story_data)
story_names<-regmatches(story_data,match)

#Clean up the Story Names
#Potentially useful for document names
clean_names<-lapply(
  lapply(story_names,
         gsub,pattern='<.+?>',
         replacement=''),
  trim)

#Use the story names as the beginning of a regex
#Anything between that and the next h2 tag is the story
#Add the story to a list
story_list <- list()
for(i in 2:(length(story_names[[1]])-1)){
  pattern <- paste(
    escape_string(story_names[[1]][i]),
    '(.+?)<h2>',sep="")
  n <- clean_string(clean_names[[1]][i])
  m <- regexec(pattern, story_data)
  r <- regmatches(story_data,m)
  s <- unlist(lapply(lapply(r,
              gsub,pattern='<.+?>',
              replacement=''),
       trim2))
  story_list[n] <- trim(s)
}

#Unpack the list and turn to a corpus
#Process the corpus
story_string <- unlist(story_list)
corp <- VCorpus(VectorSource(story_string))
corp <- tm_map(corp,PlainTextDocument)
corp <- tm_map(corp,content_transformer(tolower))
corp <- tm_map(corp,removePunctuation)
corp <- tm_map(corp,removeWords,stopwords("en"))
tdm <- TermDocumentMatrix(corp)

#####TM Package Examples#####

#Finding Frequent Terms
findFreqTerms(tdm,lowfreq = 100)

#Getting count of words
termcount<-apply(tdm,1,sum)
head(termcount[order(termcount,decreasing = T)],20)

#====Finding Frequent Associations====
findAssocs(tdm, c("king","queen"), c(0.5,0.82))

#This is just correlations

dtm_mat <- t(as.matrix(tdm))

cor(dtm_mat[,colnames(dtm_mat)=="king"],
    dtm_mat[,colnames(dtm_mat)=="ordered"])

#####Using ARULES to get associations####

s1<- lapply(story_list,strsplit,split=c('\\!|\\?|\\.|;|:'))
s2<-unlist(s1)
s3<-sapply(s2,trim)
s4 <-VCorpus(VectorSource(s3))
s4 <- tm_map(s4,PlainTextDocument)
s4 <- tm_map(s4,content_transformer(tolower))
s4 <- tm_map(s4,removePunctuation)
s4 <- tm_map(s4,removeWords,c(stopwords("en"),"said"))
dtm_sent <-DocumentTermMatrix(s4)
dim(dtm_sent)

dtm_mat_sent <- as.matrix(dtm_sent)
dim(dtm_mat_sent)

dtm_mat_sent2 <- apply(dtm_mat_sent,2,
                       function(x) as.numeric(x>0))
dim(dtm_mat_sent2)


#####Association Rules####
dtm_trans <-as(dtm_mat_sent,"itemMatrix")

rule_param <- list(support=0.0125,
                   confidence=0.50,
                   minlen=2,
                   maxlen=3)

rules <- apriori(dtm_trans,
                 rule_param 
)

summary(rules)

inspect(rules)

dim(tdm) #1266 x 20

##### NGRAMS ####
#Needs a character vector or block of text
ng <- ngram(story_string,n=2)

#Produce your own story
babble(ng,100)

print(ng,full=TRUE)

##### WORDCLOUDS ####
wordcloud(names(termcount),termcount,
          min.freq=10,random.order=FALSE)

##### Sentiment Analysis ####
#Get the Positive and Negatives Terms
#https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#lexicon
pos_words <-read.table("positive-words.txt",skip = 35,
                       colClasses = "character")
neg_words <- read.table("negative-words.txt",skip = 35,
                        colClasses = "character")
dtm_sentiment <- dtm_mat_sent
for (col in 1:dim(dtm_sentiment)[2]){
  if (colnames(dtm_sentiment)[col] %in% pos_words$V1){
    dtm_sentiment[,col]<-dtm_sentiment[,col]*1
  }
  else if(colnames(dtm_sentiment)[col] %in% neg_words$V1){
    dtm_sentiment[,col]<-dtm_sentiment[,col]*-1
  }
  else{
    dtm_sentiment[,col]<-0
  }
}
summary(dtm_sentiment[,1:10])

#Sentiment Polarity Method
#Positive Words - Negative Words / (Positive Words + Negative Words)
sent_nume <- rowSums(dtm_sentiment)
sent_denom <- rowSums(abs(dtm_sentiment))
sent_polarity <- sent_nume / sent_denom

#Get the Sentences with the most classified words
head(s2[order(sent_denom, decreasing = T)])
#Get the highest polarity sentences
head(sent_polarity[order(sent_denom, decreasing = T)])
#Plot the sentences
hist(sent_denom,main="# Tokens with Sentiment",xlab="# Tokens")
