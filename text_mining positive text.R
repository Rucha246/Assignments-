library(plyr)
library(stringr)
library(e1071)
library(tm)
library(SentimentAnalysis)
library(randomForest)
#load up word polarity list and format it
positive_list <- read.delim(file="~/Downloads/positive-words.txt", header=FALSE, stringsAsFactors=FALSE)

positive_list$V1 <- tolower(positive_list$V1)
library(tm)
library(stringr)
positive_list=Corpus(VectorSource(positive_list$V1))
positive_list=tm_map(positive_list,function(x)iconv(enc2utf8(x),sub = "byte"))
#convert my Corpus in to ;lower case
positive_list<-tm_map(positive_list,content_transformer(tolower))
#remove puncuation
positive_list<-tm_map(positive_list,removePunctuation)
positive_list<-tm_map(positive_list,removeNumbers)

Textprocessing <- function(x)
{gsub("http[[:alnum:]]*",'', x)
  gsub('http\\S+\\s*','', x) ## Remove URLs
  gsub('\\b+RT','', x) ## Remove RT
  gsub('#\\S+', '', x) ## Remove Hashtags
  gsub('@\\s+','', x) ## Remove Mentions
  gsub('[[:cntrl:]]','', x) ## Remove Controls and special characters
  gsub("\\d", '', x) ## Remove Controls and special characters
  gsub('[[:punct:]]','',x) ## Remove Punctuations
  gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
  gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
  gsub('+','',x) ## Remove extra whitespaces
}
positive_list <- tm_map(positive_list,Textprocessing)
positive_list<-tm_map(positive_list,stripWhitespace)
positive_list<-tm_map(positive_list,removeWords,stopwords("english"))
tdm_pl<-TermDocumentMatrix(positive_list)

m=as.matrix(tdm_pl)
v=sort(rowSums(m),decreasing = T)
d=data.frame(word=names(v),frequency=v)
head(d,20)



barplot(d[1:10,]$frequency,las=2,names.arg = d[1:10,]$word,col = "lightblue",main = "most frequent words",ylab = "word frequencies")
library(wordcloud)

library(RColorBrewer)
set.seed(1234)
wordcloud(words= d$word,frequency=d$frequency,min.freq=10,max.words=500,random.order=FALSE,                
          scale=c(3,0.5),colors=rainbow(50))
