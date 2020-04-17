View(Iris)
attach(Iris)
names(Iris)
summary(Iris)
dim(Iris)
head(Iris)
pairs(iris[,1:4])
pairs(iris[,1:4],col=iris[,5],oma=c(4,4,6,12))
par(xpd=TRUE)
legend(0.85,0.6, as.vector(unique(iris$Species)),fill=c(1,2,3))
#Now i would like to try little experiment. Knowng that we have three different species I am going to assume, 
#that we can distinguish each of the species just looking to the measurements of these flowers. 
#Fot visualization I am going to use clustering analysis. We will cluster all data and use the K-Means method for that.
library(dplyr)
library(ggplot2)
library(corrplot)
data(iris)
#set seed for visualtion
set.seed(8593)
iris2 <- iris
#assign species to NULL
iris2$Species <- NULL
#Three clusters for three species
(kmeans.result <- kmeans(iris2, 3))
#using table extract information about accuracy of species in each cluster.
table(iris$Species, kmeans.result$cluster)
#plotting the clusters
plot(iris2[c("Sepal.Length", "Sepal.Width")], col = kmeans.result$cluster)
#plot the cluster centers
points(kmeans.result$centers[c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 8, cex= 2)
library(cluster)
library(fpc)
data(iris) 

data_for_clustering <- iris[,-5] 

clusters_iris <- kmeans(data_for_clustering, centers = 3) 

plotcluster(data_for_clustering,clusters_iris$cluster) 
clusplot(data_for_clustering, clusters_iris$cluster, color = TRUE, shade = TRUE)
