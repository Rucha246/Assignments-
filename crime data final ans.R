library(readr)
attach(crime_data_2)
normalized_data<-scale(crime_data_2[,2:4]) #excluding the university name columnbefore normalizing
?dist
d <- dist(normalized_data, method = "euclidean") # distance matrix
?hclust
fit <- hclust(d, method="complete")
?hclust
plot(fit) # display dendrogram
plot(fit, hang=-1)
rect.hclust(fit, k=4, border=c("red","blue","yellow","green"))

groups <- cutree(fit, k=4) 
table(groups)
Crime_Rate_Categories<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(crime_data_2, Crime_Rate_Categories)
final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
View(final1)
aggregate(crime_data_2[,-1],by=list(final$Crime_Rate_Categories),mean)
#### grope 2 and 1  are  showing high crime rate that means those cities who are follwing under group 1 and 2 are risky 

