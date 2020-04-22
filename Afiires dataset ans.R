head(affairs)
str(affairs)
library(car)
library(RCOR)
#Column cration
Extra <- ifelse(affairs$affairs>0,"yes","no")
df_Affair <- data.frame(affairs[,-2],Extra)

head(df_Affair)
str(df_Affair)

#Model-1 fitting
model_A_1 <- glm(Extra~.,data = df_Affair,family = "binomial")
summary(model_A_1) # AIC value is 18
pred_A_1 <- predict(model_A_1,newdata = df_Affair,type = "response")

conf_A_1 <- table(pred_A_1>0.5,df_Affair$Extra)
effi_A_1 <- sum(diag(conf_A_1))/sum(conf_A_1);effi_A_1 # Efficiency is 1

influenceIndexPlot(model_A_1,id=list(col="red",n=5))
inf_A_1 <- as.integer(rownames(influencePlot(model_A_1,id=list(n=10,col="red"))))
length(inf_A_1)#11
df_Affair[inf_A_1,]
head(df_Affair)

# ROC Curve
require(RCOR)
rocrpred<-prediction(pred_A_1,df_Affair$Extra)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)
rocrperf@x.values
plot(rocrperf,colorize=T) 

#Testing the significance of all insignificant variables in our previous model
summary(glm(df_Affair$Extra~df_Affair$education,family = 'binomial'))
summary(glm(df_Affair$Extra~df_Affair$gender,family = 'binomial'))
summary(glm(df_Affair$Extra~df_Affair$age,family = 'binomial'))
summary(glm(df_Affair$Extra~df_Affair$education,family = 'binomial'))
summary(glm(df_Affair$Extra~df_Affair$occupation,family = 'binomial'))
summary(glm(df_Affair$Extra~df_Affair$X,family = 'binomial'))
# Removing influencing rows
df_Affair2 <- df_Affair[-c(inf_A_1),]
model2 <- glm(Extra~.,data = df_Affair2,family = "binomial")
summary(model2) # AIC value is 18
pred_A_2 <- predict(model2,newdata = df_Affair2,type = "response")
conf_A_2 <- table(pred_A_2>0.5,df_Affair2$Extra);conf_A_2
effi_A_2 <- sum(diag(conf_A_2))/sum(conf_A_2);effi_A_2 # Efficiency is 1
# ROC CURVE
require(RCOR)
rocrpred_2<-prediction(pred_A_2,df_Affair2$Extra)
rocrperf_2<-performance(rocrpred_2,'tpr','fpr')

# both the model showing same AIC value and efiiciency so we can conclude that both model are preferable 






