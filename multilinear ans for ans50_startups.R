library(readr)
View(`50_Startups`)
# Exploratory Data Analysis(60% of time)
# 1. Measures of Central Tendency
# 2. Measures of Dispersion
# 3. Third Moment Business decision
# 4. Fourth Moment Business decision
# 5. Probability distributions of variables
# 6. Graphical representations
#  > Histogram,Box plot,Dot plot,Stem & Leaf plot, 
#     Bar plot

summary(`50_Startups`)
####Remove column#####
my_data<-`50_Startups`[,-4]
View(my_data)
head(my_data)

## 7. Find the correlation b/n Output (Profit) & (Administration,Marketing.Spend,R.D.Spend)-Scatter plot##
pairs(my_data)

# 8. Correlation Coefficient matrix - Strength & Direction of Correlation
cor(my_data)

### Partial Correlation matrix - Pure Correlation  b/n the varibles
#install.packages("corpcor")

library(corpcor)
cor2pcor(cor(my_data))
####scatter plot for Profit and R.D.Spend
plot(my_data$Profit, my_data$R.D.Spend)  # plot(X,Y)
attach(my_data)
#Correlation Coefficient (r)
cor(Profit, R.D.Spend) #showes the good correlation positive correlation having value 0.9729
# Simple Linear Regression model
# lets build a simple model 
model1<-lm(Profit~R.D.Spend+Administration+Marketing.Spend)
summary(model1)#R squred value is 0.9507
rmse1 <- mean(model1$residuals^2)^.5 # RMSE value is 8855.344
pred_1 <- predict(model1,newdata = my_data)
cor(pred_1,my_data$Profit) #correlation is 0.9750
library(car)
library(ggplot2)

vif(model1)
avPlots(model1)
influencePlot(model1)
# checking singnificace level of with individual variable
summary(lm(Profit~Administration)) # Only 4 % of variation in the "Profit" is explained by the "Administration"
summary(lm(Profit~R.D.Spend)) # 94 % of variation in the "Profit" is explained by the "R.D.Spend"
summary(lm(Profit~Marketing.Spend)) # Only 55 % of variation in the "Profit" is explained by the "Marketing.Spend"

# final model
model3 <- lm(Profit~R.D.Spend+Marketing.Spend,data = my_data)
summary(model3) # In our model R² = 0.9612 (0.9594 Adjusted) and other variable except R.D.Spend are insignificant.avPlots(model.S.3,id=list(n=3,col="red"),lwd=2,col='forestgreen')
pred3 <- predict(model3,my_data) # Predicted Values
cor(pred3,my_data$Profit) # 0.9748121
rmse3 <- mean(model3$residuals^2)^.5 # RMSE value is 6899.99


