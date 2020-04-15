library(readr)
View(LabTAT)
summary(LabTAT)
library(nortest)
attach(LabTAT)
str(LabTAT)
str(LabTAT)
head((LabTAT))
####To check the normality of Data we have to perform normality test#######
shapiro.test(LabTAT$V1)#p value=0.000414 = Data is not normaly distributed
shapiro.test(LabTAT$V2)#p value=0.000456
shapiro.test(LabTAT$V3)#p value=0.0004534
shapiro.test(LabTAT$V4)# p value=0.0004126
ad.test(LabTAT$V1);ad.test(LabTAT$V2);ad.test(LabTAT$V3);ad.test(LabTAT$V4)
####### data is not normaly distributed ####
library(foreign)
####Mann whitney test#######
wilcox.test(LabTAT$V1)~(LabTAT$V2),correct=FALSE,exact=FALSE)$p.value
wilcox.test(LabTAT$V1,LabTAT$V3,mu=0,alt="two.sided",paired=F,exact=T,correct=T)
wilcox.test(LabTAT$V2,LabTAT$V4,mu=0,alt="two.sided",paired=F,exact=T,correct=T)
wilcox.test(LabTAT$V1,LabTAT$V4,mu=0,alt="two.sided",paired=F,exact=T,correct=T)
wilcox.test(LabTAT$V1,LabTAT$V2,mu=0,alt="two.sided",paired=F,exact=T,correct=T)
wilcox.test(LabTAT$V3,LabTAT$V4,mu=0,alt="two.sided",paired=F,exact=T,correct=T)
wilcox.test(LabTAT$V3,LabTAT$V2,mu=0,alt="two.sided",paired=F,exact=T,correct=T)
wilcox.test(LabTAT$V1,LabTAT$V2,LabTAT$V3,LabTAT$V4,conf.int=T,conf.level=0.95,mu=0,alt="two.sided",paired=F,exact=T,correct=T)
