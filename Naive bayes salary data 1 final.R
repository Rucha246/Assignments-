# Libraries
library(naivebayes)
library(ggplot2)
library(caret)
library(psuch)
library(e1071)
str(SalaryData_Train_1_)
View(SalaryData_Train_1_)
SalaryData_Train_1_$educationno <- as.factor(SalaryData_Train_1_$educationno)
class(SalaryData_Train_1_)
SalaryData_Train_1_$workclass<-as.factor(SalaryData_Train_1_$workclass)
SalaryData_Train_1_$education<-as.factor(SalaryData_Train_1_$education)
SalaryData_Train_1_$maritalstatus<-as.factor(SalaryData_Train_1_$maritalstatus)
SalaryData_Train_1_$occupation<-as.factor(SalaryData_Train_1_$occupation)
SalaryData_Train_1_$relationship<-as.factor(SalaryData_Train_1_$relationship)
SalaryData_Train_1_$race<-as.factor(SalaryData_Train_1_$race)
SalaryData_Train_1_$sex<-as.factor(SalaryData_Train_1_$sex)
SalaryData_Train_1_$native<-as.factor(SalaryData_Train_1_$native)
SalaryData_Train_1_$Salary<-as.factor(SalaryData_Train_1_$Salary)

str(SalaryData_Train_1_)
SalaryData_Test_1_$workclass<-as.factor(SalaryData_Test_1_$workclass)
SalaryData_Test_1_$education<-as.factor(SalaryData_Test_1_$education)
SalaryData_Test_1_$maritalstatus<-as.factor(SalaryData_Test_1_$maritalstatus)
SalaryData_Test_1_$occupation<-as.factor(SalaryData_Test_1_$occupation)
SalaryData_Test_1_$race<-as.factor(SalaryData_Test_1_$race)
SalaryData_Test_1_$sex<-as.factor(SalaryData_Test_1_$sex)
SalaryData_Test_1_$native<-as.factor(SalaryData_Test_1_$native)
SalaryData_Test_1_$Salary<-as.factor(SalaryData_Test_1_$Salary)
SalaryData_Test_1_$relationship<-as.factor(SalaryData_Test_1_$relationship)
str(SalaryData_Test_1_)

SalaryData_Test_1_$educationno <- as.factor(SalaryData_Test_1_$educationno)

#Visualization 
# Plot and ggplot 
ggplot(data=SalaryData_Train_1_,aes(x=SalaryData_Train_1_$Salary, y = SalaryData_Train_1_$age, fill = SalaryData_Train_1_$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
plot(SalaryData_Train_1_$workclass,SalaryData_Train_1_$Salary)
plot(SalaryData_Train_1_$education,SalaryData_Train_1_$Salary)

plot(SalaryData_Train_1_$educationno,SalaryData_Train_1_$Salary)
plot(SalaryData_Train_1_$maritalstatus,SalaryData_Train_1_$Salary)
plot(SalaryData_Train_1_$occupation,SalaryData_Train_1_$Salary)
plot(SalaryData_Train_1_$relationship,SalaryData_Train_1_$Salary)
plot(SalaryData_Train_1_$race,SalaryData_Train_1_$Salary)
plot(SalaryData_Train_1_$sex,SalaryData_Train_1_$Salary)

############################
ggplot(data=SalaryData_Train_1_,aes(x=SalaryData_Train_1_$Salary, y = SalaryData_Train_1_$capitalgain, fill = SalaryData_Train_1_$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
############################

ggplot(data=SalaryData_Train_1_,aes(x=SalaryData_Train_1_$Salary, y = SalaryData_Train_1_$capitalloss, fill = SalaryData_Train_1_$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
############################
ggplot(data=SalaryData_Train_1_,aes(x=SalaryData_Train_1_$Salary, y = SalaryData_Train_1_$hoursperweek, fill = SalaryData_Train_1_$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
############################
plot(SalaryData_Train_1_$native,SalaryData_Train_1_$Salary)
# Naive Bayes Model 
Model <- naiveBayes(SalaryData_Train_1_$Salary ~ ., data = SalaryData_Train_1_)
Model

Model_pred <- predict(Model,SalaryData_Test_1_)
mean(Model_pred==SalaryData_Test_1_$Salary)#0.8187

confusionMatrix(Model_pred,SalaryData_Test_1_$Salary)









