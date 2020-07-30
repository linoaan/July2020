rm(list=ls())
library(ggplot2)
library(animation)
# Iris dataset: ?iris
str(iris) # 5 variables; Species is a factor

# There seems to be variation among different species
boxplot(iris$Sepal.Length ~ iris$Species)

# Visualize the boxplot
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_histogram()

# Classification: K-Means

set.seed(1) # to ensure reproducibility
irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20)
irisCluster
table(irisCluster$cluster, iris$Species)
irisCluster$cluster

options(digits = 3)
irisClusterAni <- kmeans.ani(iris[,3:4], 3)
table(irisClusterAni$cluster, iris$Species)/nrow(iris)

# cluster = k: number of clusters to use
# centers: matrix of cluster centers
# withinss: vector of within-cluster sum of squares: one component for each cluster
# tot.withinss = sum(withinss)
# size: number of points in each cluster
# totss = Total sum of squares
# betweenss = totss - totwithinss

table(irisCluster$cluster, iris$Species)
# setosa -> cluster 1
# versicolor -> cluster 3; two points misspecified in cluster 2
# virginica -> cluster 2, 4 points classified wrongly in cluster 3

irisCluster$cluster <- as.factor(irisCluster$cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color = irisCluster$cluster)) + geom_point()
