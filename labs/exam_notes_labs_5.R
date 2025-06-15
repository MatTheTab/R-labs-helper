# Exercises:
## 1
### A) -> testing mu=mu0, unknown sigma, seperate variable test
### B) -> testing sphericity
### C) -> testing compound symmetry

## 2
### A) -> testing mu=mu0, unknown sigma
### B) -> testing sigma = sigma0
### C) -> testing sphericity
### D) -> testing compound symmetry

## 3 -> Testing sphericity

# Additional
## AD: 1 -> Testing mu=mu0, known sigma
## AD: 2 -> Testing sigma = sigma0, correction for moderate n
## AD: 3 -> Testing sphericity, correction for moderate n
## AD: 4 -> Testing compound symmetry, correction for moderate n

### Moderate examples, usually 30 to 100, but depends

#########################################################################################################################################################
# Exercise 1
setwd("C:/Users/mateu/OneDrive/Pulpit/R_labs_Master_Degree_Studies")
data = read.csv("probeWord.csv", sep=";")
data

#a) We want to test if expected_mu = (30, 25, 40, 25, 30)
# H0 -> expected_mu = mu
# H1 -> expected_mu != mu

# We do not know sigma, and we try to test mu
# let's test the assumption about normality first
library(MVN)
mvn(data, mvnTest="mardia")$multivariateNormality # The test confirms multivariate normality of the data

expected_mu = c(30, 25, 40, 25, 30)
Y = as.matrix(data[, -1])
n = nrow(Y) # Number of examples
p = ncol(Y) # Number of features
Y_bar = colMeans(Y)
S_ml = cov(Y) * (n-1)/n # Estimated sigma, but for maximum likelihood method, hence (n-1)/n
T_sq = (n-1)*(t(Y_bar-expected_mu))%*%solve(S_ml)%*%(Y_bar-expected_mu)# The test statistic
alpha = 0.05
crit_region = p*(n-1)/(n-p) * qf(1-alpha, p, n-p)
# Test statistic is within the critical region, so with confidence level of 95% we reject H0, true mu is different than the expected mu

# By using library tests
library(MVTests)
hotel1=OneSampleHT2(Y, expected_mu)
hotel1$HT2
hotel1$p.value
# p-value is smaller than alpha, so we reject H0, the mus are different

library(DescTools)
hotel2=HotellingsT2Test(Y, mu=expected_mu)
hotel2$statistic
hotel2$p.value
# p-value is smaller than alpha, so we reject H0, the mus are different

# Testing each variable
for (i in 1:p){
  hotel = OneSampleHT2(Y[, i], expected_mu[i]) # For ith column and ith element of mu
  print(paste("var ", i, " ", hotel$p.value))
}

# Variable 1 and 3 -> smallest p-value, so biggest differences

#b) We want to test sphericity of data
# H0 -> sigma is spherical
# H1 -> sigma is not spherical

# Testing assumption about normality
library(MVN)
mvn(data, mvnTest="mardia")$multivariateNormality # The test confirms normality of the data

Y = as.matrix(data[, -1])
n = nrow(Y) # Number of examples
p = ncol(Y) # Number of features
S_ml = cov(Y) * (n-1)/n # Estimated sigma, but for maximum likelihood method, hence (n-1)/n
lambda = (((p^p)*det(S_ml))/((sum(diag(S_ml)))^p))^(n/2)
stat = -2 * log(lambda)
alpha = 0.05
crit_region = qchisq(1-alpha, p*(p+1)/2-1)
# Statistic belongs to the critical region, so with the confidence level 95% we reject H0, the covariance is not spherical

# c) We want to test compound symmetry, in other words, we want to test if on the diagonal we have equal values but on the off-diagonal we have something different
# H0 -> sigma has compound symmetry
# H1 -> sigma does not have compound symmetry

# Testing assumption about normality
library(MVN)
mvn(data, mvnTest="mardia")$multivariateNormality # The test confirms normality of the data

Y = as.matrix(data[, -1])
n = nrow(Y) # Number of examples
p = ncol(Y) # Number of features
S_ml = cov(Y) * (n-1)/n # Estimated sigma, but for maximum likelihood method, hence (n-1)/n

ssg = sum(diag(S_ml))/p
ro = 1/(p-1) * (t(rep(1, p)) %*% S_ml %*% rep(1, p) - sum(diag(S_ml)))/(sum(diag(S_ml)))
test_statistic = -n * log(det(S_ml)/((ssg^p) * ((1-ro)^(p-1))*(1 + (p-1)*ro)))
crit = qchisq(1-alpha, p*(p+1)/2-2)

# The test statistic is not in the critical region, so we do not reject the H0 hypothesis, hence, sigma has compound symmetry

#########################################################################################################################################################
# Exercise 2
ramus_data = read.csv("ramus.csv", sep=";")

#a) We want to test if expected_mu = (48,49,50,51)
# H0 -> expected_mu = mu
# H1 -> expected_mu != mu

# We do not know sigma, and we try to test mu
# let's test the assumption about normality first
library(MVN)
mvn(ramus_data, mvnTest="mardia")$multivariateNormality # The test confirms multivariate normality of the data

expected_mu = c(48,49,50,51)
Y = as.matrix(ramus_data[, -1])
n = nrow(Y) # Number of examples
p = ncol(Y) # Number of features
Y_bar = colMeans(Y)
S_ml = cov(Y) * (n-1)/n # Estimated sigma, but for maximum likelihood method, hence (n-1)/n
T_sq = (n-1)*(t(Y_bar-expected_mu))%*%solve(S_ml)%*%(Y_bar-expected_mu)# The test statistic
alpha = 0.05
crit_region = p*(n-1)/(n-p) * qf(1-alpha, p, n-p)
# Test statistic does not belong to the critical region, so we do not reject H0, hence the true mu is equal to the expected mu with the confidence level of 95%

# b) We want to test if sigma = sigma0
# H0 -> sigma = sigma0
# H1 -> sigma != sigma0

Sigma0 = matrix(c(6.0, 5.9, 5.5, 5.2, 5.9, 6.1, 5.9, 5.6, 5.5, 5.9, 6.6, 6.6, 5.2, 5.6, 6.6, 7.0), nrow=4)
Y = as.matrix(ramus_data[, -1])
n = nrow(Y) # Number of examples
p = ncol(Y) # Number of features
S_ml = cov(Y) * (n-1)/n
test_stat = n*(sum(diag(solve(Sigma0)%*%S_ml)) - log(det(solve(Sigma0) %*% S_ml)) - p)
crit = qchisq(1-alpha, p*(p+1)/2)
# Test statistic does not belong to the critical region, so we do not reject H0, hence the true sigma is equal to the sigma0 with the confidence level of 95%


# c) We want to test sphericity of the data
# H0 -> sigma is spherical
# H1 -> sigma is not spherical
library(MVN)
mvn(ramus_data, mvnTest="mardia")$multivariateNormality # The test confirms normality of the data

Y = as.matrix(ramus_data[, -1])
n = nrow(Y) # Number of examples
p = ncol(Y) # Number of features
S_ml = cov(Y) * (n-1)/n # Estimated sigma, but for maximum likelihood method, hence (n-1)/n
lambda = (((p^p)*det(S_ml))/((sum(diag(S_ml)))^p))^(n/2)
stat = -2 * log(lambda)
alpha = 0.05
crit_region = qchisq(1-alpha, p*(p+1)/2-1)

# The statistic is within the critical region, so we reject H0, hence, with confidence level of 95% sigma is not spherical

# d) We want to test compound symmetry, in other words, we want to test if on the diagonal we have equal values but on the off-diagonal we have something different
# H0 -> sigma has compound symmetry
# H1 -> sigma does not have compound symmetry

# Testing assumption about normality
library(MVN)
mvn(ramus_data, mvnTest="mardia")$multivariateNormality # The test confirms normality of the data

Y = as.matrix(ramus_data[, -1])
n = nrow(Y) # Number of examples
p = ncol(Y) # Number of features
S_ml = cov(Y) * (n-1)/n # Estimated sigma, but for maximum likelihood method, hence (n-1)/n

ssg = sum(diag(S_ml))/p
ro = 1/(p-1) * (t(rep(1, p)) %*% S_ml %*% rep(1, p) - sum(diag(S_ml)))/(sum(diag(S_ml)))
test_statistic = -n * log(det(S_ml)/((ssg^p) * ((1-ro)^(p-1))*(1 + (p-1)*ro)))
crit = qchisq(1-alpha, p*(p+1)/2-2)

# The test statistic is not in the critical region, so we do not reject H0, with confidence level of 95% sigma has compound symmetry

#########################################################################################################################################################
# Exercise 3 Testing independence -> testing sphericity
# H0 -> is spherical
# H1 -> is not spherical
library(HSAUR2)
meteo

data <- meteo[2: 4]
data <- data[-2]
Y = as.matrix(data)
n = nrow(Y)
p = ncol(Y)

library(MVN)
mvn(data, mvnTest="mardia")$multivariateNormality

Q = diag(n) - 1/n * rep(1, n*n, nrow=n)
S_ml = cov(Y) * (n-1)/n
lambda = (((p^p)*det(S_ml))/((sum(diag(S_ml)))^p))^(n/2)
stat = -2 * log(lambda)

critval = qchisq(1-alpha, p*(p+1)/2-1)
# The statistic is not within the critical region, so we do not reject H0, hence, with confidence level of 95% sigma is spherical

#########################################################################################################################################################
# AD: 1 -> Testing mu=mu0, known sigma
# H0: mu=mu_0
# H1: mu!=mu_0
setwd("C:/Users/mateu/OneDrive/Pulpit/R_labs_Master_Degree_Studies")
ramus_data = read.csv("ramus.csv", sep=";")
mu_0 = c(48,49,50,51)

library(MVN)
mvn(ramus_data, mvnTest="mardia")$multivariateNormality # The test confirms multivariate normality of the data

Y = as.matrix(ramus_data[, -1])
n = nrow(Y)
p = ncol(Y)
# Here I will assume the true sigma is equal to cov(ramus_data) but in reality just put here the real sigma
Sigma = cov(Y)
Y_bar = colMeans(Y)
test_stat = n*(t(Y_bar - mu_0)) %*% solve(Sigma) %*% (Y_bar - mu_0)
alpha = 0.05
crit_region = qchisq(1-alpha, p)

# The test statistic is not within the critical region, so we do not reject H0, hence, with condifence level of 95% the true mean is equal to the expected mean (mu = mu_0)

#########################################################################################################################################################
# AD: 2 -> Testing sigma = sigma0, correction for moderate n
# H0: sigma=sigma_0
# H1: sigma!=sigma_0
setwd("C:/Users/mateu/OneDrive/Pulpit/R_labs_Master_Degree_Studies")
ramus_data = read.csv("ramus.csv", sep=";")
Sigma0 = matrix(c(6.0, 5.9, 5.5, 5.2, 5.9, 6.1, 5.9, 5.6, 5.5, 5.9, 6.6, 6.6, 5.2, 5.6, 6.6, 7.0), nrow=4)

Y = as.matrix(ramus_data[, -1])
n = nrow(Y) # Number of examples
p = ncol(Y) # Number of features
S_ml = cov(Y) * (n-1)/n
almost_lambda = n*(sum(diag(solve(Sigma0)%*%S_ml)) - log(det(solve(Sigma0) %*% S_ml)) - p)
ln_lambda = almost_lambda/(-2)
test_statisic = -((2*(n-1))/n) * (1 - ((1/(6*n-7)) * (2*p + 1 - (2/(p+1))))) * ln_lambda
crit = qchisq(1-alpha, p*(p+1)/2)

# The test statistic is not within critical region, so we do not reject H0, hence, the true sigma is equal to sigma0

#########################################################################################################################################################
## AD: 3 -> Testing sphericity, correction for moderate n
# H0 -> sigma is spherical
# H1 -> sigma is not spherical
setwd("C:/Users/mateu/OneDrive/Pulpit/R_labs_Master_Degree_Studies")
ramus_data = read.csv("ramus.csv", sep=";")

library(MVN)
mvn(ramus_data, mvnTest="mardia")$multivariateNormality # The test confirms normality of the data

Y = as.matrix(ramus_data[, -1])
n = nrow(Y) # Number of examples
p = ncol(Y) # Number of features
S_ml = cov(Y) * (n-1)/n # Estimated sigma, but for maximum likelihood method, hence (n-1)/n
lambda = (((p^p)*det(S_ml))/((sum(diag(S_ml)))^p))^(n/2)
ln_lambda = log(lambda)
stat = -(2/n)*(n-1-((2*p*p + p + 2)/(6*p)))*ln_lambda
alpha = 0.05
crit_region = qchisq(1-alpha, p*(p+1)/2-1)

# The statistic is within the critical region, so we reject H0, hence, with confidence level of 95% sigma is not spherical

#########################################################################################################################################################
## AD: 4 -> Testing compound symmetry, correction for moderate n
# H0 -> sigma has compound symmetry
# H1 -> sigma does not have compound symmetry
setwd("C:/Users/mateu/OneDrive/Pulpit/R_labs_Master_Degree_Studies")
ramus_data = read.csv("ramus.csv", sep=";")

library(MVN)
mvn(ramus_data, mvnTest="mardia")$multivariateNormality # The test confirms normality of the data

Y = as.matrix(ramus_data[, -1])
n = nrow(Y) # Number of examples
p = ncol(Y) # Number of features
S_ml = cov(Y) * (n-1)/n # Estimated sigma, but for maximum likelihood method, hence (n-1)/n

ssg = sum(diag(S_ml))/p
ro = 1/(p-1) * (t(rep(1, p)) %*% S_ml %*% rep(1, p) - sum(diag(S_ml)))/(sum(diag(S_ml)))
almost_lambda = -n * log(det(S_ml)/((ssg^p) * ((1-ro)^(p-1))*(1 + (p-1)*ro)))
ln_lambda = almost_lambda/(-2)
test_stat = -(2/n) * (n-1-((p*((p+1)^2)*(2*p-3))/(6*(p-1)*(p*p+p-4))))*ln_lambda
crit = qchisq(1-alpha, p*(p+1)/2-2)
# The test statistic is in the critical region, so we reject H0, hence, with confidence level of 95% sigma does not have compound symmetry