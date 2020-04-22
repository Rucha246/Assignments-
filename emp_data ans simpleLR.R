library(readr)
View(emp_data)

####Exploratary data analysis###
summary(emp_data)

#Scatter plot
plot(emp_data$Salary_hike, emp_data$Churn_out_rate)  # plot(X,Y) seems like having negative correlation

attach(emp_data)
#Correlation Coefficient (r)
cor(emp_data$Salary_hike,emp_data$Churn_out_rate)# cor(X,Y)
# correlation between two variable is -0.91172
plot(emp_data$Salary_hike,emp_data$Churn_out_rate)
# Simple Linear Regression model
reg <- lm(emp_data$Churn_out_rate ~ emp_data$Salary_hike) # lm(Y ~ X)

summary(reg) # RMSE = 0.8312
 #Here our coeeficient of determination is 0.8312 that is about 83% of variation on target variable Churn out rate is explained by the independent variable salary hike
cor(emp_data$Churn_out_rate,)

pred <- predict(reg)
reg$residuals
sum(reg$residuals)
cor(emp_data$Churn_out_rate,pred)#correlation i 0.91172 that is actual value and predicted values are very much correlated


mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(emp_data))

sqrt(mean(reg$residuals^2)) #RMSE =3.9975

confint(reg,level=0.95)
predict(reg,interval="predict")

###### ggplot for linear regression######
library(ggplot2)
ggplot(data = emp_data, aes(x = Salary_hike, y = Churn_out_rate)) + 
  geom_point(color='red') +
  geom_line(color='blue',data = emp_data, aes(x=Salary_hike, y=pred))
#################
# A simple ggplot code for directly showing the line

# ggplot(emp_data,aes(Salary_hike,churn_out_rate))+stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm')

#####################

# Logrithamic Model

# x = log(Salary_hike); y = Churn_out_rate

plot(log(Salary_hike), Churn_out_rate)
cor(log(Salary_hike), Churn_out_rate)

reg_log <- lm(Churn_out_rate ~ log(Salary_hike))   # lm(Y ~ X)

summary(reg_log)# showing the RMSE value =0.8486
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(emp_data))  #RMSE=3.7860

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

######################

# Exponential Model

# x = Salary_hike and y = log(Churn_out_rate)

plot(Salary_hike, log(Churn_out_rate))

cor(Salary_hike, log(Churn_out_rate))
# showes the negative correlation
reg_exp <- lm(log(Churn_out_rate) ~ Salary_hike)  #lm(log(Y) ~ X)

summary(reg_exp)# shows the RMSE value is 0.8735

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))#RMSE value is 0.04617

logat <- predict(reg_exp)
at <- exp(logat)


confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")


##############################
# Polynomial model with 2 degree 

reg2<- lm(emp_data$Churn_out_rate~poly(x = emp_data$Salary_hike,degree = 2))
summary(reg2) # Coefficient of determination is 0.9737 i.e. 97 percent of variation on the dependent variable is explained by the independent variable
predict2 <- predict(reg2,newdata = emp_data)
cor(predict2,emp_data$Churn_out_rate) # 0.9867 i.e. more than the previous model
sqrt(sum(reg2$residuals^2)/nrow(emp_data)) #RMSE =1.5779 here we are getting less RMSE value compared to reg model

#plotting
plot(x = emp_data$Salary_hike,y = predict2,type = "b",col="red",xlab = "Calories consumed",ylab = "Weight Gain")
points(x = emp_data$Salary_hike,y = emp_data$Churn_out_rate)
# visualization
require(ggplot2)
ggplot(data = emp_data,mapping=aes(x = Salary_hike,y=Churn_out_rate))+geom_smooth(method = "lm",formula = y~poly(x,2))+geom_point()+ggtitle("Model 2",subtitle = "2nd degree polynomial transformation")










































