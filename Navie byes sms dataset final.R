library(tm)
library(textcat)
message<-read.csv("~/Downloads/sms_raw_NB.csv",stringsAsFactors = F)
head(message)
message$type=factor(message$type)
str(message)
library(textcat)
table(textcat(x=message$type),message$type)
msg_cps<-Corpus(VectorSource(message$type))
str(msg_cps)
clean_cps<-tm_map(msg_cps,tolower)
clean_cps<-tm_map(clean_cps,removeNumbers)
clean_cps<-tm_map(clean_cps,removeWords,stopwords(kind = "en"))
clean_cps<-tm_map(clean_cps,removePunctuation)
clean_cps<-tm_map(clean_cps,stripWhitespace)

# document term matrix
msd_dtm=DocumentTermMatrix(clean_cps)

#splitting up a data in train and test
nrow(message)
set.seed(101)
split<-sample(1:nrow(message),nrow(message)*.7,F)
train_msg<-message[split,]
test_msg<-message[-split,]

# split a data of document term matrix
dtm_train=msd_dtm[split,]
dtm_test=msd_dtm[-split,]

##corpus
cor_train=clean_cps[split]
cor_test=clean_cps[-split]

round(prop.table(table(train_msg$type)))

library(wordcloud)

wordcloud::wordcloud(cor_train,min.freq = 10,max.words = 300,
                     random.order = F,colors = ifelse(message$type=="spam","red","blue"))


# Blue words for Ham messages, and red words are most frequently used in Spam messages

# Lets reduce the number of column base on the lowest frequency
freq_terms = findFreqTerms(x = dtm_train, lowfreq = 10)
dtm_train_ = DocumentTermMatrix(cor_train, list(dictionary=freq_terms))
dtm_test_ =  DocumentTermMatrix(cor_test, list(dictionary=freq_terms))

# We reduced the dimension
dim(dtm_test_);dim(dtm_test)

c_counts = function(x) {
  x = ifelse(x > 0, 1, 0)
  x = factor(x, levels = c(0, 1), labels=c("No", "Yes"))
  return (x)
}

dtm_train_ = apply(dtm_train_, MARGIN=2, c_counts);table(dtm_train_) #run once
dtm_test_  = apply(dtm_test_, MARGIN=2, c_counts);table(dtm_test_) #run once


# Model Selection ----

library(e1071)
# Model 1
model_1 = naiveBayes(dtm_train_, train_msg$type)
pred_1 = predict(model_1,dtm_test_)
table(Predicted=pred_1,Actual=test_msg$type)

efficiency1 = mean(pred_1==test_msg$type);efficiency1

# I am sertisfied with my efficiency i.e. 1 which is 100 %











