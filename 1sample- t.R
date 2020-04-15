setwd("F:\\Office\\Coca Cola\\Training at Corp office\\Training\\Hypo testing")

#1- sample t test

library(readxl)
Bolt_diameter <- read_excel("F:/Classes/Day Wise/Day Wise/Day 10 Hypothesis Testing/R Codes _ data/Bolt diameter.xlsx")
View(Bolt_diameter)
attach(Bolt_diameter)

shapiro.test(Diameter)#p value= 0.6133, p high Ho fly=> data are normal

# population std deviation unknowm, perform 1 sample t test

t.test(Diameter,mu=10 ,alternative = "two.sided",conf.level = 0.95)

# p value= 0.0005896, p low Ho go, 
#reject Ho => current mean is not equal to the historical mean

