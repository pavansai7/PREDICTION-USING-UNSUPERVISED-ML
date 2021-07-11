
#Importibfg iris dataset


iris
dt<- iris
head(dt)
attach(dt)
str(dt)
summary(dt)
sapply(iris[,-5], var)


#Plotting a scatter plot for the variables.
 
library(ggplot2)
ggplot(dt,aes(x= Sepal.Length, y= Sepal.Width, col= Species))+geom_point()
ggplot(dt,aes(x= Petal.Length, y= Petal.Width, col= Species))+geom_point()

#Finding within cluster sum of squares(WCSS)


set.seed(1)
k.max<- 20
wss<- sapply(1:k.max,function(k){kmeans(dt[,1:4],k, nstart = 20, iter.max = 20)$tot.withinss})
wss

#Plotting a graph b/w number of cluster vs wcss.

plot(1:k.max,wss,type = "b", xlab = "Number of cluster(k)", ylab = "Within cluster sum of squares", main = "Number of cluster vs sum of squares")

#Finding k-mean.

k<- kmeans(iris[,1:4],3,nstart = 20)
table(k$cluster, dt$Species)


#Plotting a clusplot


library(NbClust)
library(cluster)
clusplot(dt, k$cluster, labels = 2, shade = TRUE, color = TRUE)

#Finding accuracy.

#Total number of correctly classified instances are : 50+48+36 = 134

#Total number of incorrectly classified instances are : 14+2 = 16

accuracy<- ((134/150)*100)
accuracy

