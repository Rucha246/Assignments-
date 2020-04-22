library(readr)
colnames(Computer_Data)
df_Comp<-Computer_Data[,-1]
head(Computer_Data)
head(df_Comp)
attach(df_Comp)
summary(df_Comp)
str(df_Comp)
df_Comp$cd=as.integer(df_Comp$cd)
df_Comp$multi=as.integer(df_Comp$multi)
df_Comp$premium=as.integer(df_Comp$premium)
str(df_Comp)
str(df_Comp)
ls_df_comp = list("speed" = table(df_Comp$speed),"ram"=table(df_Comp$ram),"screen"=table(df_Comp$screen),
               "cd"=table(df_Comp$cd),"multi"=table(df_Comp$multi),table(df_Comp$premium),"ads"=table(df_Comp$ads),"trend"=table(df_Comp$trend))

barplot(ls_df_comp$speed,main="speed",xlab = "speed",ylab = "No of computers")
str(df_Comp)


summary(df_Comp)


# model building####
model1<-lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend,data = df_Comp)
summary(model1) # R squred=0.7756
library(car)
library(ggplot2)
vif(model1)
#No colinarity problem in our model
avPlots(model1)
pred1 <- predict(model1,newdata = df_Comp)
cor(pred1,df_Comp$price) # Correlation is 0.8806631
rmse1 <- mean(model1$residuals^2)^.5 # rmse = 275.1298
influence_1 <- as.integer(rownames(influencePlot(model1,id = list(n=5,col="blue",cex=1.2))))
influenceIndexPlot(model1,id=list(n=10,col='red'))

# model 2
##### log transfermation#####
df_comp2 <- data.frame(scale(log(df_Comp[,-c(1,2,7,8,9)])),"price" = df_Comp$price,"cd" = df_Comp$cd,"premium" = df_Comp$premium,"multi" = df_Comp$multi)
model_Comp_2 <- lm(price~.,data=df_comp2)
summary(model_Comp_2) # 0.6241
influenceIndexPlot(model_Comp_2,id = list(n=20,col='red'))
influ_comp <- as.integer(rownames(influencePlot(model_Comp_2,id = list(n=20,col="blue"))))
length(influ_comp)
df_comp3 <- df_comp2[-c(influ_comp),]#head(df_comp2)
model_Comp_3 <- lm(price~.,data=df_comp3)
summary(model_Comp_3) # R2 = 0.6344
avPlots(model_Comp_3)
pred_C_3 <- predict(model_Comp_3,newdata = df_comp3)
cor(pred_C_3,df_comp3$price) # Correlation is 0.79649
rmse_C_3 <- mean(model_Comp_3$residuals^2)^.5 # rmse = 281.3819

# model 4 ----

influence(model1)
influenceIndexPlot(model1)
influencing_obs <- length(which(rowSums(influence.measures(model1)$is.inf) > 0));influencing_obs # These are the influencing observations
influence_obs <- as.integer(rownames(influencePlot(model1,id=list(n=90,col="red"))))


length(influence_obs)

1- 186/nrow(df_Comp)
df_Comp_scale <- data.frame(df_Comp[,-c(6,7,8)],"premium"=df_Comp$premium,"cd"=df_Comp$cd,"multi"=df_Comp$multi)#,"cd"=df_Comp3$cd,"multi"=df_Comp3$multi
df_Comp_scale <- df_Comp_scale[-c(influence_obs),]
head(df_Comp_scale)

df_comp[influence_obs,]

model_Comp_4 <- lm(price~.,data=df_Comp_scale) 
summary(model_Comp_4) # R2 = 0.804
avPlots(model_Comp_4)
pred_C_4 <- predict(model_Comp_4,newdata = df_Comp)
cor(pred_C_4,df_Comp$price) # Correlation is 0.879
rmse_C_4 <- mean(model_Comp_4$residuals^2)^.5 # rmse = 238.0004










