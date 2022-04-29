library(readxl)
data_1 <- read_excel("C:/Users/laphouse/Desktop/data_1.csv")
View(data_1)


#_____________________________________________________________________________
library(dplyr)
#select valid columns to cluster them, *id & variance columns ignored
mydata = select(data_1,c(2,3))
mydata

#_____________________________________________________________________________
#WSS plot to choose the optimum number of clusters
#WSS plot function
wssplot <- function(data, nc=15, seed=2345){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}
wssplot(mydata)

#_____________________________________________________________________________
#kmeans clustering
Km = kmeans(mydata,4)
Km

#show centers of clusters
Km$centers 

#Ploting the clusters
library(ggplot2)
autoplot(Km,mydata,frame=TRUE)

library(cluster)
clusplot(mydata, Km$cluster,color=T,shade=T)

#_____________________________________________________________________

#hierarchical clustering:
#Agglomerative method:


h_data=(mydata) 
h_data

#compute the distance between data rows
distances=dist(h_data, method ="euclidean") 
distances

#____________________________________________________________________________
#method 1
#Agglomerative method:

#complete with agnes
aglom_h=agnes(h_data,method="complete")
aglom_h
#look at the agglomerative coeffecient
aglom_h$ac
#the lower the agglomerative coeffecient, the better the clustering
pltree(aglom_h)
#___________________________________________________________________________
#complete with hclust
hclusters=hclust(distances)
plot(hclusters)

#_____________________________________________________________________________
#method2
#divisive method
divisive_h=diana(h_data)
divisive_h$dc #Disisive coeffecient
#plot the dendogram
pltree(divisive_h)
rect.hclust(aglom_h, k = 2, border = 2:4)

#_____________________________________________________________________________
#density-based clustering
d= select(data_1,c(2,3))
library(fpc)
Dbscan_cl<-dbscan(d, eps= 10, MinPts= 6)
Dbscan_cl
Dbscan_cl$cluster

plot(Dbscan_cl,d, main = "DBScan")
#____________________________________________________________________________
#Model-based clustering plots
library(mclust)
d.mclust<-Mclust(d)
plot(d.mclust)
