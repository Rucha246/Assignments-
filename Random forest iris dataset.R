data("iris")
ind <- sample(2,nrow(iris),replace=TRUE,prob=c(0.7,0.3))
table(ind)
trainData <- iris[ind==1,]
testData <- iris[ind==2,]
dim(trainData)
dim(testData)
head(trainData)
head(testData)
library(randomForest)
iris_rf <- randomForest(Species~.,data=trainData,ntree=100,proximity=TRUE)
print(iris_rf)
#importance of class descriotion
importance(iris_rf)
irisPred<-predict(iris_rf,newdata=testData)
table(irisPred, testData$Species)
#plotting the margins,positive margin means correct classification
plot(margin(iris_rf,testData$Species))
#checking the classification accuracy
# the number of correct prediction
print(sum(irisPred==testData$Species))
# out of these many data points used to test the prediction
print(length(testData$Species)) 
#the accuracy
print(sum(irisPred==testData$Species)/length(testData$Species)) # we found accuracy is 0.9285714