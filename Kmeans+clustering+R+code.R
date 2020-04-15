install.packages("plyr")
library(plyr)

x <-  runif(50) # generating 50 random numbers from uniform distribution(0,1)
x
y <-  runif(50) # generating 50 random numbers 
y

data <- cbind(x,y) 
plot(data)

km <- kmeans(data,4) #kmeans clustering - 4 clusters; k ~ sqrt(n/2)
#install.packages("animation")
library(animation)

km <- kmeans.ani(data, 4)
km$centers

km_8 <- kmeans(data,8)


wss<-(nrow(data)-1)*sum(apply(data,2,var))
for(i in 2:15) wss[i]<-sum(kmeans(data,centers = i)$withinss)
plot(1:15,wss,type = "b", xlab = "No of clusters",ylab = "Avg distance")
