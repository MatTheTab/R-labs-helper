# Exercises:
## 1 -> test assumptions about normality, homogeneity of variance and use MANOVA to compare means + post-hog test

## 2 -> test assumptions about normality, and use MANOVA to compare means + post-hog test

## 3 -> test assumptions about normality, homogeneity of variance and use MANOVA to compare means

## 4 -> test assumptions about normality, use MANOVA to compare means

#########################################################################################################################################################
# Exercise 1 -> test assumptions about normality, homogeneity of variance and use MANOVA to compare means + post-hog test
library(HSAUR2)
library(MVN)
library(heplots)

for (i in unique(skulls$epoch)){
  subset_skulls = subset(skulls, epoch == i)
  subset_skulls = subset_skulls[, -1]
  print(mvn(as.matrix(subset_skulls), mvnTest="mardia")$multivariateNormality)
}
# Multivariate normality confirmed

labels = skulls[, 1]
feats = skulls[, -1]
boxM(feats, labels) # p-value is bigger than alpha, so we do not reject H0, hence, they are equal

m = manova(as.matrix(feats)~labels)
summary(m, test="Wilks")
summary(m, test="Roy")
summary(m, test="Pillai")
summary(m, test="Hotelling-Lawley") # p-value is smaller than alpha, so we reject H0, they are not equal

summary.aov(m) #bl contributes the most to rejecting H0

# Same as above but manually:
feats = as.matrix(feats)
for (feat in 1:4){
  an = anova(lm(feats[, feat] ~ labels))
  print(paste("feat", feat))
  print(an)
}


#########################################################################################################################################################
# Exercise 2 -> test assumptions about normality, and use MANOVA to compare means + post-hog test
setwd("C:/Users/mateu/OneDrive/Pulpit/R_labs_Master_Degree_Studies")
risk = read.csv("risk.csv", sep=";")
risk

for (i in unique(risk$method)){
  subset_risk = subset(risk, method == i)
  subset_risk = subset_risk[, -3]
  print(mvn(as.matrix(subset_risk), mvnTest="mardia")$multivariateNormality)
}
# Multivariate normality confirmed

manovaTest = manova(as.matrix(risk[, -3]) ~ risk$method)  
summary(manovaTest, test="Wilks")
summary(manovaTest, test="Roy")
summary(manovaTest, test="Pillai")
summary(manovaTest, test="Hotelling-Lawley")  # p-value is smaller than alpha, so we reject H0, they are not equal

summary.aov(manovaTest) # Both variables contribute more-or-less equally


#########################################################################################################################################################
# Exercise 3 -> test assumptions about normality, homogeneity of variance and use MANOVA to compare means
setwd("C:/Users/mateu/OneDrive/Pulpit/R_labs_Master_Degree_Studies")
pottery = read.csv("pottery.csv", sep=",")
pottery

for (i in unique(pottery$site)){
  subset_pottery = subset(pottery, site == i)
  subset_pottery = subset_pottery[, -1]
  if (dim(subset_pottery)[1] < 7){
    next
  }
  print(mvn(as.matrix(subset_pottery), mvnTest="mardia")$multivariateNormality)
}

manovaTest = manova(as.matrix(pottery[, -1]) ~ pottery$site) 
summary(manovaTest, test="Wilks")   # p-value is smaller than alpha, so we reject H0, they are not equal
summary.aov(manovaTest) # All variables contribute to separation


#########################################################################################################################################################
# Exercise 4 -> test assumptions about normality, use MANOVA to compare means
iris

for (i in unique(iris$Species)){
  subset_iris = subset(iris, Species == i)
  subset_iris = subset_iris[, -5]
  print(mvn(as.matrix(subset_iris), mvnTest="mardia")$multivariateNormality)
}

manovaTest = manova(as.matrix(iris[, -5]) ~ iris$Species) 
summary(manovaTest, test="Wilks")   # p-value is smaller than alpha, so we reject H0, they are not equal
summary.aov(manovaTest) # All variables contribute to separation
