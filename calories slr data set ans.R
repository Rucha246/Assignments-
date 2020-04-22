library(ggplot2)
library(car)
attach(calories_consumed)
colnames(calories_consumed)<-c("weightGain","CaloriesConsumed")
#target variable is Weight gain and independent variable is calories consued 
# Normality test
shapiro.test(calories_consumed$weightGain)#p value is 0.006646 that means data is not normaly distributed
shapiro.test(calories_consumed$CaloriesConsumed)# p value is 0.4887 that means data is normaly distributed
#lets find the correlation between these two variable
cor(calories_consumed$weightGain,calories_consumed$CaloriesConsumed)
plot(calories_consumed$weightGain~calories_consumed$CaloriesConsumed)
#Its seem to be the positive correlation with 0.9466

# building a model 
model1<-lm(calories_consumed$weightGain~calories_consumed$CaloriesConsumed)
summary(model1)
#coeffiecient of determination is 0.896 about 90% variation in target variable
predict1<-predict(object = model1,newdata=calories_consumed)
cor(calories_consumed$weightGain,predict1)# correlation is 0.946 ,our predicted value and actule values are very much correlated
sqrt(sum(model1$residulas^2)/nrow(calories_consumed))


# plotting regression line 
plot(x=calories_consumed$CaloriesConsumed,y=predict1,type = "b",col="red",xlab = "Calories consumed",ylab = "Weight Gain")
points(x = calories_consumed$CaloriesConsumed,y = calories_consumed$WeightGain)
# ggplot
ggplot(data = calories_consumed,mapping=aes(x = CaloriesConsumed,y=weightGain))+geom_smooth(method = "lm")+geom_point()+ggtitle("Model 1",subtitle = "Y~X")
# polinominal transformation
model2 <- lm(calories_consumed$weightGain~poly(x = calories_consumed$CaloriesConsumed,degree = 2))
summary(model2)# coeffiecientof determination is 0.9521that means 95% of variation on the dependet variable is explained by independent variable
predict2 <- predict(model2,newdata = calories_consumed)
cor(calories_consumed$weightGain,predict2) #0.9757 that is more than preivionus model
sqrt(sum(model2$residuals^2)/nrow(calories_consumed))#RMSE=70.40752 here we are getting less RMSE compared to model1

ggplot(data = calories_consumed,mapping=aes(x = CaloriesConsumed,y=weightGain))+geom_smooth(method = "lm",formula = y~poly(x,2))+geom_point()+ggtitle("Model 2",subtitle = "Second degree polynomial")

# model building with 3 degree polynominal
model3<-lm(calories_consumed$weightGain~poly(x = calories_consumed$CaloriesConsumed,degree = 3))
summary(model3)# coeffieciant of determination is 0.9811that is 98% variation on y axis
predict3 <- predict(object = model3,newdata = calories_consumed)
cor(calories_consumed$weightGain,predict3)#0.9905292 highly correlated
sqrt(sum(model3$residuals^2)/nrow(calories_consumed))#RMSE =44.15011 Here we are getting less RMSE value compared to model1

ggplot(data = calories_consumed,mapping=aes(x = CaloriesConsumed,y=weightGain))+geom_smooth(method = "lm",formula = y~poly(x,3))+geom_point()+ggtitle("Model 3",subtitle = "3rd degree polynomial Transformation")
plot(model3)







