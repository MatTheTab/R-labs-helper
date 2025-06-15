# Exercise 1

library(HSAUR2)
library(MVN)
library(tidyverse)
library(heplots)

for (i in distinct(as_tibble(skulls$epoch))){
  subset_skulls = subset(skulls, epoch = i)
  mvn(as.matrix(subset_skulls), mvnTest="mardia")$multivariateNormality
}

skulls
labels = skulls[, 1]
feats = skulls[, -1]
boxM(feats, labels)

m = manova(as.matrix(feats)~labels)
summary(m, test="Wilks")
summary(m, test="Roy")
summary(m, test="Pillai")
summary(m, test="Hotelling-Lawley")

# Reject H0, they are not equal

for (feat in 1:4){
  manovaTest = manova(as.matrix(feats[, -feat]) ~ labels)
  print(paste("feat", feat))
  print(summary(manovaTest, test="Wilks"))
}

feats = as.matrix(feats)
for (feat in 1:4){
  an = anova(lm(feats[, feat] ~ labels))
  print(paste("feat", feat))
  print(an)
}

# Exercise 2

setwd("C:/Users/mateu/OneDrive/Pulpit/R_labs_Master_Degree_Studies")
data = read.csv("risk.csv", sep=";")

levels = unique(data$method)
for (i in levels){
  sub_data = subset(data, method == i)
  sub_data = sub_data[, -3]
  print(mvn(sub_data, mvnTest = "mardia")$multivariateNormality)
}

manovaTest = manova(as.matrix(data[, -3]) ~ data$method)  
summary(manovaTest, test="Wilks")
summary(manovaTest, test="Roy")
summary(manovaTest, test="Pillai")
summary(manovaTest, test="Hotelling-Lawley")
summary.aov(manovaTest)

# Exercise 3
setwd("C:/Users/mateu/OneDrive/Pulpit/R_labs_Master_Degree_Studies")
data = read.csv("pottery.csv", sep=",")

sites = unique(data$site)
for (i in sites){ # The error is caused by not enough examples
  sub_data = subset(data, site == i)
  if (dim(sub_data)[1] < 7){
    next
  }
  sub_data = as.matrix(sub_data[, -1])
  print(mvn(sub_data, mvn="mardia")$multivariateNormality)
}

# H0 -> population samples have equal means
# H1 -> population means have different means

manovaTest = manova(as.matrix(data[, -1]) ~ data$site)
summary(manovaTest)

# p-val <- alpha -> we reject H0

values = data[, -1]
labels = data[, 1]
for (feat in 1:5){
  an = anova(lm(as.matrix(values[, feat]) ~ labels))
  print(paste("feat", feat))
  print(an)
}

# Exercise 4

# H0 The means are equal
# H1 the means are different

speciess = unique(iris$Species)
for (i in speciess){
  sub_data = subset(iris, Species == i)
  sub_data = sub_data[, -5]
  print(mvn(sub_data, mvnTest = "mardia")$multivariateNormality)
}

manovaTest = manova(as.matrix(iris[, -5]) ~ iris$Species)
summary(manovaTest)

# p-value lower than alpha, so we reject the null hypothesis, they are not equal

values = iris[, -5]
labels = iris[, 5]
for (feat in 1:4){
  an = anova(lm(as.matrix(values[, feat]) ~ labels))
  print(paste("feat", feat))
  print(an)
}

summary.aov(manovaTest)
