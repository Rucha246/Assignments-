library(readr)
View(Company_Data)
str(Company_Data)
attach(Company_Data)
class(Sales)
head(Company_Data)
range(Company_Data$Sales)
median(Company_Data$Sales)
sort(Company_Data$Sales)
length(Company_Data$Sales)

#assume that 3 sales may consider low ,mediam and high
sort(Company_Data$Sales)[(400/3*2)]#cutoff value 8.67
#we can consider that those sales which values are greter than8.5 will be considered as high sales and other low sales

Sale1 <- ifelse(Company_Data$Sales > 8.5,"High","Low")
prop.table(table(Sale1)) # Desired result achieved
company <- data.frame(Sale1,Company_Data[,-1])

set.seed(101);split <- sample(nrow(company),nrow(company)*.7,F)
Train_company <- company[split,]
Test_company <- company[-split,]

# model 1 with whole data

Model1 <- ctree(Sale1~.,data = Train_company)
pred1 <- predict(Model1,Test_company)
table(Actual=Test_company$Sale1,Predicted=pred1)
mean(Test_company$Sale1==pred1) # Accuracy is 75.8%
plot(Model1)


#Model-2 with whole data
Model2 <- C5.0(Sale1~.,data = Train_company)
pred2 <- predict(Model2,Test_company)
table(Test_company$Sale1,pred2)
mean(Test_company$Sale1==pred2) # Accuracy is 80%
plot(Model2)

#Model-3 with whole data
Model3 <- C5.0(Sale1~.,data = Train_company,trials=100)
pred3 <- predict(Model3,Test_company)
table(Test_company$Sale1,pred3)
mean(Test_company$Sale1==pred3) # Accuracy is 85%
plot(Model3,cex=0.5)
plot(Model3)
C5imp(Model3)
library(party)
plot(Model3_2,terminal_panel = node_terminal(Model3_2))


for( i in 1:50){
  Model3 <- C5.0(Sale1~.,data = Train_company,trials=i)
  pred3 <- predict(Model3,Test_comp)
  print(paste( i , mean(Test_comp$SaleC==pred3)))
  
}

