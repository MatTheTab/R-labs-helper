library(MASS)
library(MVN)

data("iris")
iris

Y = as.matrix(iris[, -5])
n1 = length(which(iris[,5] == "setosa"))
n2 = length(which(iris[,5] == "versicolor"))
n3 = length(which(iris[,5] == "virginica"))
n = n1+n2+n3

Y1 = Y[1:n1, ]
Y2 = Y[(n1+1):(n1+n2), ]
Y3 = Y[(n1+n2+1):n, ]

mvn(Y1, mvnTest="mardia")$multivariateNormality
mvn(Y2, mvnTest="mardia")$multivariateNormality
mvn(Y3, mvnTest="mardia")$multivariateNormality

library(heplots)
boxM(Y, iris$Species) # we should reject the hypothesis and not use ANOVA but for simplicity lets assume it works

manova(iris ~ iris$Species)
