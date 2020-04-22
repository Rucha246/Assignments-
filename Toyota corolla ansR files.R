library(readr)
attach(ToyotaCorolla)
library(ggplot2)
library(car)
Corolla<-ToyotaCorolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
summary(Corolla)
cor(Corolla)
pairs(Corolla)
# building a 1st model
model1<-lm(Price~.,data = Corolla)
summary(model1) # R squured value=0.8638
pred1<-predict(model1,Corolla)
cor(pred1,Corolla$Price) # we are getting a correlation 0.9293

rmse1<-mean(model1$residuals^2)^.5;rmse1 # Rmse value is 1338.258
avPlots(model1)
#check for the influence plot
influenceIndexPlot(model1,grid = T,id = list(col="red",cex=1.5))
influencePlot(model1,id=list(col="red"))
influence_index <- as.integer(rownames(influencePlot(model1))) # Ok, here we are getting 3 influence index in our model
vif(model1) # ok, in our model we can say there is no colinearity problem exists as all the vif values are less than 10
influenceIndexPlot(model1,grid = T,id = list(col="red",cex=2))

#Building my Second model
df_Corola <- Corolla[-c(influence_index),]

model2 <- lm(Price~.,data = df_Corola)
summary(model2) # R2 = 0.8852... cc and doors are now significant
pred2 <- predict(model2,df_Corola)
cor(pred2,df_Corola$Price) # We are getting correlation  as 0.9408425
rmse2 <- mean(model2$residuals^2)^.5;rmse2 # and RMSE value as 1227.474

avPlots(model2)



