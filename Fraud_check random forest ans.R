library(randomForest)
library(MASS)
library(caret)
set.seed(123)
hist(Fraud_check$Taxable.Income)
hist(Fraud_check$Taxable.Income, main = "Sales of Companydata",xlim = c(0,100000),
     breaks=c(seq(40,60,80)), col = c("blue","red", "green","violet"))
Risky_Good = ifelse(Fraud_check$Taxable.Income<= 30000, "Risky", "Good")
# if Taxable Income is less than or equal to 30000 then Risky else Good.
FCtemp= data.frame(Fraud_check,Risky_Good)
FC = FCtemp[,c(1:7)]
str(FC)
table(FC$Risky_Good)
# Spliting up a data in train and test data set
set.seed(123)
ind <- sample(2, nrow(FC), replace = TRUE, prob = c(0.7,0.3))
train <- FC[ind==1,]
test  <- FC[ind==2,]
set.seed(213)
rf <- randomForest(Risky_Good~., data=train)
head(train$Risky_Good)
attributes(rf)
# Prediction and Confusion Matrix - Training data 
pred1 <- predict(rf, train)
head(pred1)
# looks like the first six predicted value and original value matches.

confusionMatrix(pred1, train$Risky_Good)   # 100 % accuracy on training data 
# Sensitivity for Yes and No is 100 % 

# Prediction with test data - Test Data 
pred2 <- predict(rf, test)
confusionMatrix(pred2, test$Risky_Good) # 100 % accuracy on test data 
plot(rf)

# Tune Random Forest Model mtry 
tune <- tuneRF(train[,-6], train[,6], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)

pred1 <- predict(rf1, train)
confusionMatrix(pred1, train$Risky_Good)  # 100 % accuracy on training data 
#Variable Importance :
  
varImpPlot(rf1)
# removed. Taxable.Income is very important and Urban is not that important.

varImpPlot(rf1 ,Sort = T, n.var = 5, main = "Top 5 -Variable Importance")
varUsed(rf) 
# Partial Dependence Plot 
partialPlot(rf1, train, Taxable.Income, "Good")
tr1 <- getTree(rf1, 2, labelVar = TRUE)



# Multi Dimension scaling plot of proximity Matrix
MDSplot(rf1, FC$Risky_Good)















