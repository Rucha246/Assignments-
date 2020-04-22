library(tm)
library(ROAuth)
library(twitteR)
api_key="pCpB5nLbiZbjbMcxzSa7hTYim"
api_secret="fYLvgR0eKdHezedf2ZEMDag4nhHwJt5najhxC5Z6sCqvXuKx9V"
access_token="127058048-r0sKWyBwpLpI9sL2j2gVte5lw1I1tf0j9WNmLT1U"
access_token_secret="H8soRuDbnoYCru6zwlSo2dEFbdC8W5VtNDYOINsQzm8qO"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
tweets=twitteR::searchTwitter("#article370",n=10000,lang = "en",since = "2019-08-05")
length(tweets)
# conver to a data frame 
df=twListToDF(tweets)
# Build a Corpus
library(tm)
library(stringr)
corpus_370=Corpus(VectorSource(df$text))
corpus_370=tm_map(corpus_370,function(x)iconv(enc2utf8(x),sub = "byte"))
#convert my Corpus in to ;lower case
corpus_370<-tm_map(corpus_370,content_transformer(tolower))
#remove puncuation
corpus_370<-tm_map(corpus_370,removePunctuation)
corpus_370<-tm_map(corpus_370,removeNumbers)

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
corpus_370 <- tm_map(corpus_370,Textprocessing)
corpus_370<-tm_map(corpus_370,stripWhitespace)
corpus_370<-tm_map(corpus_370,removeWords,stopwords("english"))
tdm_370<-TermDocumentMatrix(corpus_370)

m=as.matrix(tdm_370)
v=sort(rowSums(m),decreasing = T)
d=data.frame(word=names(v),frequency=v)
head(d,20)



barplot(d[1:10,]$frequency,las=2,names.arg = d[1:10,]$word,col = "lightblue",main = "most frequent words",ylab = "word frequencies")
library(wordcloud)

library(RColorBrewer)
set.seed(1234)
wordcloud(words= d$word,frequency=d$frequency,min.freq=10,max.words=500,random.order=FALSE,                
             scale=c(3,0.5),colors=rainbow(50))

