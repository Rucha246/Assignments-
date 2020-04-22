library(readr)
View(delivery_time)
#######Exploratory Data analysis#####
summary(delivery_time)
#normalitty test
shapiro.test(delivery_time$Delivery.Time)# p value is 8963 data is normaly distributed
shapiro.test(delivery_time$Sorting.Time)#p value is 1881 that means data is normaly distributed



#####Scatterplot####
plot(delivery_time$Delivery.Time,delivery_time$Sorting.Time)

attach(delivery_time)

####correlaton coeficient####

cor(delivery_time$Delivery.Time,delivery_time$Sorting.Time)
# correlation between two variable is 8255
plot(delivery_time$Delivery.Time,delivery_time$Sorting.Time)
 # it seems that data is linear with positive correlation with 0.8259
######simple linear regression#####
model1<- lm(delivery_time$Delivery.Time~delivery_time$Sorting.Time)
summary(model1)
#Here  our coeffiecient of determination is 0.6823that is about  68% variation on target variable dilivery time explained by sorting time which is independent variable
# one data is influencing our model which is observation no.1
sqrt(sum(model1$residuals^2)/nrow(delivery_time))  #RMSE = 2.79165
predict1 <- predict(object =model1,newdata = delivery_time)

####ggplot for adding regresion line for data####ggplot(data = delivery_time, aes(x = Delivery.Time, y = Sorting.Time)) + 
require(ggplot2)
plot(x = delivery_time$Sorting.Time,y = predict1,type = "b",col="red",xlab = "Calories consumed",ylab = "Weight Gain")
points(x = delivery_time$Sorting.Time,y = delivery_time$Delivery.Time)
# ggplot
ggplot(data = delivery_time,mapping=aes(y = Delivery.Time,x=Sorting.Time))+geom_smooth(method = "lm")+geom_point()+ggtitle("Model 1",subtitle = "Y~X")

# Lets do polynomial transformation
model2 <- lm(delivery_time$Delivery.Time~poly(x = delivery_time$Sorting.Time,degree = 2))
summary(model2) # Coefficient of determination is 0.6934 i.e. 69 percent of variation on the dependent variable is explained by the independent variable
predict2 <- predict(model2,newdata = delivery_time)
cor(predict2,delivery_time$Delivery.Time) # 0.8327 i.e. more than the previous model
sqrt(sum(model2$residuals^2)/nrow(delivery_time))  #RMSE = 2.742148 Here we are getting less RMSE value compared to model1
ggplot(data = delivery_time,mapping=aes(x = Sorting.Time,y=Delivery.Time))+geom_smooth(method = "lm",formula = y~poly(x,2))+geom_point()+ggtitle("Model 2",subtitle = "2nd degree polynomial transformation")

# I am doing my model with 5 degree polynomial 
model3 <- lm(delivery_time$Delivery.Time~poly(x = delivery_time$Sorting.Time,degree = 5))
summary(model3) # Coefficient of determination is 0.7142 i.e. 71 % of variation on y is explained by the 5 degree polynomial of x
sqrt(sum(model3$residuals^2)/nrow(delivery_time))  #RMSE = 2.6475 Here we are getting less RMSE value compared to model2
predict3 <- predict(object = model3,newdata = delivery_time)
cor(predict3,delivery_time$Delivery.Time) # 0.845121 highly correlated
ggplot(data = delivery_time,mapping=aes(x = Sorting.Time,y=Delivery.Time))+geom_smooth(method = "lm",formula = y~poly(x,5))+geom_point()+ggtitle("Model 3",subtitle = "5 degree polynomial")

#Doing Log transformation on the target variable. 
model4 <- lm(log(delivery_time$Delivery.Time)~poly(delivery_time$Sorting.Time,2))
summary(model4) # Coefficient of determination is 0.7649 i.e. 76 % of variation on y is explained by the 3 degree polynomial of x
sqrt(sum(model4$residuals^2)/nrow(delivery_time))  #RMSE = 0.1505 Here we are getting less RMSE value compared to model2
predict4 <-  predict(object = model4,newdata = delivery_time) #log of predicted value

pred4_act <- exp(predict4) # Predicted value
delivery_time$Delivery.Time # Actyal value
sqrt(sum((delivery_time$Delivery.Time-pred4_act)^2)/nrow(delivery_time)) # RMSE = 2.799042

cor(exp(predict4),delivery_time$Delivery.Time) # 0.825883 highly correlated
ggplot(data = delivery_time,mapping=aes(x = Sorting.Time,y=log(Delivery.Time)))+geom_smooth(method = "lm",formula = y~poly(x,5))+geom_point()+ggtitle("Model 4",subtitle = "log(y) and x is of 2 degree polynomial")








































