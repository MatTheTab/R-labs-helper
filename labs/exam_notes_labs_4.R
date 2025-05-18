# Exercise 1

## Mean
setwd("C:/Users/mateu/OneDrive/Pulpit/R_labs_Master_Degree_Studies")
data = read.csv("calcium_data.csv", sep=";")
data

data$y3[is.na(data$y3)] <- mean(data$y3, na.rm = TRUE)
y3_mean = data[2, ]$y3
data

linear1 = lm(y3 ~ y1 + y2, data=data)
predict(linear1, data.frame(y1= 35, y2= 7.588889))
missing_val = predict(linear1, data.frame(y1= 35, y2= 4.9))
data[1, ]$y2 <- missing_val
data

library(mice)

## Norm
setwd("C:/Users/mateu/OneDrive/Pulpit/R_labs_Master_Degree_Studies")
data = read.csv("calcium_data.csv", sep=";")
data
data = complete(mice(data, method="norm.predict"))
data
y3_norm = data[2, ]$y3

## PMM
setwd("C:/Users/mateu/OneDrive/Pulpit/R_labs_Master_Degree_Studies")
data = read.csv("calcium_data.csv", sep=";")
data = complete(mice(data, method="pmm"))
data
y3_pmm = data[2, ]$y3

## Cart
setwd("C:/Users/mateu/OneDrive/Pulpit/R_labs_Master_Degree_Studies")
data = read.csv("calcium_data.csv", sep=";")
data = complete(mice(data, method="cart"))
data
y3_cart = data[2, ]$y3

## Lasso
setwd("C:/Users/mateu/OneDrive/Pulpit/R_labs_Master_Degree_Studies")
data = read.csv("calcium_data.csv", sep=";")
data = complete(mice(data, method="lasso.norm"))
data
y3_lasso = data[2, ]$y3

calculated = c(y3_mean, y3_manual, y3_norm, y3_pmm, y3_cart, y3_lasso)
calculated

# Exercise 2

library(VIM)
library(dataset)

md.pattern(airquality)

airquality$Ozone = complete(mice(airquality, method="cart"))$Ozone 
airquality = complete(mice(airquality, method="norm"))
airquality

food$Sweetener = complete(mice(food, method = "cart"))$Sweetener
food = complete(mice(food, method = "norm"))
md.pattern(airquality)

# Exercise 3

library(mnormt)
mu <- rep(0, 5)  # Mean vector of length 5 with all zeros
sigma <- diag(c(5, 4, 2, 4, 1))  # Diagonal covariance matrix
sample_data <- rmnorm(n = 100, mean = mu, varcov = sigma)
sample_data

sample_mean = mean(sample_data)
sample_cov = cov(sample_data)

n=100
mean = 1/n * t(sample_data) %*% rep(1, n)
mean

Q = diag(n) - 1/n * matrix(rep(1, n^2), nrow=n)
covar = 1/(n-1) * t(sample_data) %*% Q %*% sample_data
covar

library(nortest)

p <- 5
alpha <- 0.05  # significance level

for (i in 1:p) {
  cat("Column", i, "\n")
  
  pval <- lillie.test(sample_data[, i])$p.value
  cat("Lilliefors (Kolmogorov-Smirnov) test p-value:", pval, "\n")
  cat(ifelse(pval > alpha, "→ Confirms normality\n\n", "→ Rejects normality\n\n"))
  
  pval <- sf.test(sample_data[, i])$p.value
  cat("Shapiro-Francia test p-value:", pval, "\n")
  cat(ifelse(pval > alpha, "→ Confirms normality\n\n", "→ Rejects normality\n\n"))
  
  pval <- shapiro.test(sample_data[, i])$p.value
  cat("Shapiro-Wilk test p-value:", pval, "\n")
  cat(ifelse(pval > alpha, "→ Confirms normality\n\n", "→ Rejects normality\n\n"))
  
  pval <- cvm.test(sample_data[, i])$p.value
  cat("Cramer-von Mises test p-value:", pval, "\n")
  cat(ifelse(pval > alpha, "→ Confirms normality\n\n", "→ Rejects normality\n\n"))
  
  pval <- ad.test(sample_data[, i])$p.value
  cat("Anderson-Darling test p-value:", pval, "\n")
  cat(ifelse(pval > alpha, "→ Confirms normality\n\n", "→ Rejects normality\n\n"))
}


lillie.test(sample_data)
sf.test(sample_data)
shapiro.test(sample_data)
cvm.test(sample_data)
ad.test(sample_data)

library(car)

par(mfrow = c(2, 3))
for (i in 1:p){
  qqPlot(sample_data[, i])
}

par(mfrow = c(2, 3))
for (i in 1:p){
  Di = (sample_data[, i] - mu[, i])%*%solve(covar)%*%(sample_data[, i] - mu[, i])
  ui = n*Di/((n-1)^2)
  qqPlot(ui, "beta", shape1 = p/2, shape2 = (n - p - 1)/2)
}

library(MVN)
mvn(sample_data, mvnTest="mardia")$multivariateNormality

library(mvnormalTest)
mardia(sample_data, T)

# Exercise 4
library(HSAUR2)
mvn(meteo[-1], mvnTest="mardia")$multivariateNormality
mvn(phosphate[-1], mvnTest="mardia")$multivariateNormality
mvn(plasma[-3], mvnTest="mardia")$multivariateNormality
mvn(skulls[-1], mvnTest="mardia")$multivariateNormality
mvn(iris[-5], mvnTest="mardia")$multivariateNormality
mvn(heptathlon, mvnTest="mardia")$multivariateNormality
mvn(iris[-5], mvnTest="mardia")$multivariateNormality

