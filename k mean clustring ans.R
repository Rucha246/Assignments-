library("dummies")
library("dendextend")
library("dendextendRcpp")
library("gridExtra")
library("cluster")
library("factoextra")
library("MASS")
library("fpc")
df_airline<-read_xlsx("~/Downloads/EastWestAirlines.xlsx",sheet =2)
View(df_airline)
attach(df_airline)
airline <- data.frame(df_airline[,-c(4,5,6)],"cc1_miles" = as.factor(df_airline$cc1_miles),
                      "cc2_miles"=as.factor(df_airline$cc2_miles),"cc3_miles"=as.factor(df_airline$cc3_miles))
dim(airline)
df_air<-airline[,-1]
1721/nrow(airline)
####Create requred function
normalize_dummy <- function(x){
  col <- ncol(x)
  row <- nrow(x)
  y <- 1:nrow(x)
  for (i in 1:col){
    if(class(x[,i])=="numeric" | class(x[,i])=="integer")
    {
      minx <- min(x[,i])
      maxx <- max(x[,i])
      for(j in 1:row)
      {
        x[j,i] <- ifelse((x[j,i] - minx) != 0,yes =((x[j,i] - minx) / (maxx - minx)),no = 0)
      }
    }
    
  }
  f <- c()
  for(i in 1:ncol(x)){
    if(class(x[,i])=="factor"){
      dummies <- data.frame(dummies::dummy(x[,i]))
      y <- data.frame(y,dummies)
      f <- c(f,i)
    }
    else{
      next
    }
  }
  if(is.null(f)){
    output <- x
  }
  else{output <- data.frame(x[,-f],y[,-1])}
  return(output)
}
all_hclust <- function(dist,k=3,method="all"){
  method=c("single", "complete", "average", "mcquitty", "ward.D", "ward.D2", "centroid","median")
  row <- 0
  clust_df <<- as.data.frame(method,matrix(0,nrow = length(method),ncol = k))
  all_clus <- list()
  for(i in method){
    row <- row+1
    hcl <- hclust(d = dist,method = i)
    clusters <- cutree(tree = hcl,k = k)
    all_clus[[i]] <- clusters
    clust_df[row,2:(k+1)] <- as.integer(tabulate(clusters))
  }
  
  all_clus[["table"]] <- clust_df
  return(all_clus)
}

# Normalize_dummycreation and hierarchical clustering
df_air <- normalize_dummy(df_air)

dist_A <-dist(df_air,method = "euclidian")
clustlist <- all_hclust(dist = dist_A,k = 3)


clustlist$table # From table we can see that ward.D2 is giving best result among all
#write.csv(clustlist$table,"hclust_1.csv")
df_AA <-data.frame(airline,cluster = clustlist$ward.D2) 
mode <- function(x){
  which.max(tabulate(x))
}
Exp_A_1 <- data.frame(aggregate(x = airline[,-c(10:12)],by = list(df_AA$cluster),FUN =mean),aggregate(x = airline[,c(10:12)],by = list(df_AA$cluster),FUN =mode)[,-1])
#write.csv(Exp_A_1,"ExpectedA1.csv")

# Check Using the sub Sample
df_s_airline <-airline[sample(1:nrow(airline),size = round(nrow(airline)*0.7),replace = F),]
df_s_air <- normalize_dummy(df_s_airline)
dist_s_A <-dist(df_s_air,method = "euclidian")
clustlist_s_a <- all_hclust(dist_s_A,k = 3)
clustlist_s_a$table
df_SA <- data.frame(df_s_airline,cluster = clustlist_s_a$ward.D2) 
Exp_SA_1 <- data.frame(aggregate(x = df_s_airline[,-c(10:12)],by = list(df_SA$cluster),FUN =mean),
                       aggregate(x = airline[,c(10:12)],by = list(df_AA$cluster),FUN =mode)[,-1])

list(Exp_A_1,Exp_SA_1)
#write.csv(Exp_SA_1,"ExpectedAS1.csv")
# Dendogrm
cl <- hclust(d= dist_A,method = "ward.D2")
plot(cl,hang = -1,labels=airline$id,cex = 0.25)
rect.hclust(cl,k=3)

# KMeans Clustering
df_norm_A <- normalize_dummy(airline[,-1])
km_A <- kmeans(x = df_norm_A,5)
table(km_A$cluster)

Twss<-c();k <- 1:7
for(i in 1:7){
  set.seed(33) # seed = 33  ; I set this seed for constant plot for every iteration so please dont remove it
  km_a <- kmeans(x = df_norm_A,i)
  Twss[i]<-km_a$tot.withinss
}
Twss
plot(y=Twss,x = k,"b",
     col=c(rep(1,2),3,rep(1,4)),cex = c(rep(1,2),2,rep(1,4)),lwd=3,main = 'Scree Plot',
     col.main="blue",col.axis="blue") # I may consider 3 as my optimum k value

text(x = 3,y = 1800,labels = "Elbow point")
# From the plot I get my elbow point for k = 3.
set.seed(33) # I set the same seed for same answer for every iteration
km_A <- kmeans(x = df_norm_A,3)
table(km_A$cluster)

aggregate(x = airline[,-c(1,3,10:12)],by = list(km_A$cluster),mean)
#write.csv(aggregate(x = df_finalA[,-c(1,3,10:12)],by = list(km_A$cluster),mean),"aggreAirKmea.csv")
head(airline)







