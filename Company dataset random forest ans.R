#install.packages("randomForest")
library(randomForest)

# Importing data set
head(Company_Data)
nrow(Company_Data)
str(Company_Data)
boxplot(Company_Data)
boxplot(scale(Company_Data[,c(1:6,8,9)])) ### we can see tha sale,comprice and price are having outliers
#Outlier tretment



outlie1 <- which(Company_Data$Sales >= min(boxplot(Company_Data$Sales)$out))
outlie2 <- which(Company_Data$CompPrice == max(boxplot(Company_Data$CompPrice)$out)
|Company_Data$CompPrice == min(boxplot(Company_Data$CompPrice)$out) )

outlie3 <- which(Company_Data$Price <= 53 |Company_Data$Price >= 185)
outlier <- c(outlie1,outlie2,outlie3)
Company_Data_o <- Company_Data[-outlier,]
boxplot(Company_Data_o)$out
boxplot(company_Data_o$Price)$out
Company_Data_final <- Company_Data_o[Company_Data_o$Price >55,]
nrow(Company_Data_final)
SaleC <- ifelse(Company_Data_final$Sales > 8.5,"High","Low")
df_Company <- data.frame(Company_Data_final[,-1],SaleC)

# Presence of missing values
colSums(is.na(df_Company)) # No missing data in my data set


# Splitting up a data in Train and Test dataset
set.seed(101);splitC <- sample(nrow(Company_Data_final),nrow(Company_Data_final)*.7,F)
trainC <- df_Company[splitC,]
testC <- df_Company[-splitC,]

trainC_n <- normalize_dummy(trainC[,-11])
cn <- c("CompPrice","Income","Advertising","Population","Price","Age","Education","CBad","CGood","CMedium","CNo","CYes","CNo.1","CYes.1")
colnames(trainC_n) <- cn
testC_n <- normalize_dummy(testC[,-11])
colnames(testC_n) <- cn

#Model Building Model no 1
set.seed(101);modelC1 <- randomForest(trainC$SaleC~.,data = trainC[,-11])
summary(modelC1)
predC1 <- predict(modelC1,testC) 
table(Actual=testC$SaleC,Predicted=predC1)
mean(testC$SaleC==predC1) # 82.9% accuracy
plot(modelC1)
varImpPlot(modelC1, pch = 20, main = "Importance of Variables")












