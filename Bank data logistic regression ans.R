library(car)
library(RCOCR)
head(bank.full)
str(bank.full)
###clear unnessery columns####
table(bank.full$job);table(bank.full$marital);table(bank.full$education);table(bank.full$default);table(bank.full$housing);table(bank.full$loan);table(bank.full$contact);table(bank.full$day);table(bank.full$campaign);table(bank.full$previous);table(bank.full$poutcome);table(bank.full$y);class(bank.full$y)

df_bank<-bank.full[,-c(9)]
df_bank$y

####building a model####
model1<-glm(y~.,data = df_bank,family = "binomial")
summary(model1) # AIC= 22184
Y_1 <- predict(model1,df_bank)
plot(Y_1,df_bank$y)
plot(Y_1)
prob_1 <-predict(model1,df_bank,type = "response")
confu_Bank_1 <- table(prob_1>0.5,df_bank$y)
effi_Bank_1 <- sum(diag(confu_Bank_1))/sum(confu_Bank_1);effi_Bank_1 # Efficiency of my model is 0.9011303 


#### Splitting a data in train and test data######

set.seed(101)
Test_data <- sample(x = 1:nrow(df_bank),size = round(nrow(df_bank)*(30/100)),replace = F)
Train1 <- df_bank[-c(Test_data),]
Test1 <- df_bank[c(Test_data),]
model_Bankt1 <- glm(y~.,data = Train1,family = "binomial")
summary(model_Bankt1) # AIC = 15393
Y_BankT_1 <- predict(model_Bankt1,Test1)
prob_BankT_1 <-predict(model_Bankt1,Test1,type = "response")
plot(prob_BankT_1,Test1$y,
     col=ifelse((prob_BankT_1<0.5 & Test1$y =="no")|(prob_BankT_1>0.5 & Test1$y =="yes"),"green","red")
     ,pch = ifelse((prob_BankT_1<0.5 & Test1$y =="no")|(prob_BankT_1>0.5 & Test1$y =="yes"),1,4) # Uncomment it (Ctrl+Shift+C) if you want different character also
)
# In this plot the red circle represents wrong prediction and the gree circle represents correct prediction
plot(Y_BankT_1,col=ifelse((prob_BankT_1<0.5 & Test1$y =="no")|(prob_BankT_1>0.5 & Test1$y =="yes"),"green","red"))

confu_BankT_1 <- table(prob_BankT_1>0.5,Test1$y) ;confu_Bank_1
effi_BankT_1 <- sum(diag(confu_BankT_1))/sum(confu_BankT_1);effi_BankT_1 # Efficiency of my model is 0.9005382 


influencePlot(model1)
influencePlot(model1)
influ <-as.integer(intersect(rownames(influencePlot(model1)),rownames(influencePlot(model1))));influ
# ROC Curve

require(RCOR)
rocrpred<-prediction(prob_BankT_1,Test1$y)
rocrperf<-performance(rocrpred,'tpr','fpr')


str(rocrperf)
rocrperf@x.values
colorize(x=rocrperf,colors = c(1,2,3))
plot(rocrperf,colorize=T) #text.adj=c(-0.2,1.7)


#######Removing some insignificant column########

df_bank2 <- df_bank[-c(influ),]
model_bank2 <- glm(y~.,data = df_bank2,family = "binomial")
summary(model_bank2) # AIC= 22177
Y_bank2 <- predict(model_bank2,df_bank2)
plot(Y_bank2,df_bank2$y)
plot(Y_bank2)
prob_bank2 <-predict(model_bank2,df_bank2,type = "response")
confu_bank2 <- table(prob_bank2>0.5,df_bank2$y)
effi_bank2 <- sum(diag(confu_bank2))/sum(confu_bank2);effi_bank2 # Efficiency of my model is 0.901148 

# Train and Test Split and Model Evaluation
set.seed(101)
Test_2 <- as.integer(sample(x = rownames(df_bank2),size = round(nrow(df_bank2)*(30/100)),replace = F))
Train_2 <- df_bank2[-c(Test_2),]
Test_2 <- df_bank2[c(Test_2),]
model_B2 <- glm(y~.,data = Train_2,family = "binomial")
summary(model_B2) # AIC = 15456
Y_Bank_2 <- predict(model_B2,Test_2)
prob_BT_2 <-predict(model_B2,Test_2,type = "response")
plot(prob_BT_2,Test_B_2$y,
     col=ifelse((prob_BT_2<0.5 & Test_B_2$y =="no")|(prob_BT_2>0.5 & Test_B_2$y =="yes"),"green","red")
     #,pch = ifelse((prob_BT_1<0.5 & Test_B_1$y =="no")|(prob_BT_1>0.5 & Test_B_1$y =="yes"),1,4)
)
# In this plot the red circle represents wrong prediction and the gree circle represents correct prediction
plot(Y_Bank_2,col=ifelse((prob_BT_2<0.5 & Test_2$y =="no")|(prob_BT_2>0.5 & Test_2$y =="yes"),"green","red"))

confu_BT_2 <- table(prob_BT_2>0.5,Test_2$y) ;confu_BT_2
effi_BT_2 <- sum(diag(confu_BT_2))/sum(confu_BT_2);effi_BT_2 # Efficiency of my model is 0.8966229

#Cutoff value

str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)

####As we compred the both model first model is more significant




