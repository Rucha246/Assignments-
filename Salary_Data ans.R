library(readr)
View(Salary_Data)
#####EDA#####
summary(Salary_Data)
###Scatter plot####
plot(Salary_Data$YearsExperience,Salary_Data$Salary) # plot (X,Y)
attach(Salary_Data)
##### finding out correlation####### correlation ceficient(r)######
cor(YearsExperience,Salary)
#Its is like it is showing positive correlation
#simple liner regression model###
reg<-lm(Salary_Data$Salary~Salary_Data$YearsExperience) # lm(Y,X)
summary(reg) # 0.957
pred1 <- predict(reg)

reg$residuals
summary(reg$residuals)


cor(Salary_Data$Salary,pred1)#correlation is 0.9782 our predicted value and actual value is highly correlated
sqrt(sum(reg$residuals^2)/nrow(Salary_Data))#RMSE =5592.0.44
# it seems 
sqrt(mean(reg$residuals^2))    

confint(reg,level=0.95)
predict(reg,interval="predict")     

#### ggplot for adding regression line to the data###
library(ggplot2)
require(ggplot2)

ggplot(data = Salary_Data, aes(x = YearsExperience, y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = Salary_Data, aes(x=YearsExperience, y=pred1))

##### logrithmic model####

plot(log(YearsExperience), Salary)
cor(log(YearsExperience), Salary)# showes 0.9240611 showes the positive correlation

reg_log <- lm(Salary ~ log(YearsExperience))   # lm(Y ~ X)

summary(reg_log) #showes the RMSE 0.8539
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(Salary_Data))  #RMSE =10302.89

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

######Exponential Model######
# x = year of expirience and y = log(salary)

plot(YearsExperience, log(Salary))

cor(YearsExperience, log(Salary)) # showes the positive correlation 0.9653 

reg_exp <- lm(log(Salary) ~ YearsExperience)  #lm(log(Y) ~ X)

summary(reg_exp) # RMSE value is 0.932

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

logat <- predict(reg_exp)
at <- exp(logat)

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")
######Polynominal model with 3 degree###
reg2 <- lm(Salary_Data$Salary~poly(x = Salary_Data$YearsExperience,degree = 3))
summary(reg2) # Coefficient of determination is 0.9636 i.e. 96 percent of variation on the dependent variable is explained by the independent variable
predict2 <- predict(reg2,newdata = Salary_Data)
cor(predict2,Salary_Data$Salary) # 0.9816 i.e. more than the previous model
sqrt(sum(reg2$residuals^2)/nrow(Salary_Data))  #RMSE = 5142.642 Here we are getting less RMSE value compared to model1 but its negligibl. 
ggplot(data = Salary_Data,mapping=aes(x = YearsExperience,y=Salary))+geom_smooth(method = "lm",formula = y~poly(x,3))+geom_point()+ggtitle("Model 2",subtitle = "3rd degree polynomial transformation")






































     
     
     
     
     
     
     
     
     
     