library(readr)
View(BuyerRatio)
attach(BuyerRatio)
#Hypothesis testing 
#Ho=male-female buyer rations are similar across region
#Ha=Male-female buyer rations are not similer
chisq.test(BuyerRatio[,2:5])
chisq.test(t(BuyerRatio[,2:5]))
