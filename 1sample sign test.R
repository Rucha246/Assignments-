setwd("F:\\Office\\Coca Cola\\Training at Corp office\\Training\\Hypo testing")

#1- sample sign test


library(readxl)
Marks_data <- read_excel("F:/Classes/Day Wise/Day Wise/Day 10 Hypothesis Testing/R Codes _ data/Marks-1sample sign test.xlsx")
View(Marks_data)
attach(Marks_data)

#normality test
shapiro.test(Marks_data$Marks)# p - value =0.0008013, reject Ho, Data are non- normal

#historical median=82

SIGN.test(Marks,md = 82, alternative = "two.sided",
          conf.level = 0.95)
#p value=0.1153 , p high Ho fly, fail to reject Ho 
#Current median is equal to historic median
