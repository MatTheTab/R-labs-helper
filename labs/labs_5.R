# Exercise 1
setwd("C:/Users/mateu/OneDrive/Pulpit/R_labs_Master_Degree_Studies")
data = read.csv("probeWord.csv", sep=";")
data

# a)
mu0 = c(30, 25, 40, 25, 30)

library(MVN)
mvn(data, mvnTest="mardia")$multivariateNormality
Y = as.matrix(data[-1])
n = nrow(Y)
p = ncol(Y)
ybar = colMeans(Y)
Q = diag(n) - 1/n * rep(1, n*n, nrow=n)
S_ml = 1/n * t(Y) %*% Q %*% Y
S_ml_other = cov(Y) * (n-1)/n
all.equal(S_ml, S_ml_other)
t_sq = (n-1) * t(ybar - mu0) %*% solve(S_ml) %*% (ybar-mu0)
alpha = 0.05
crit_val = p*(n-1)/(n-p) * qf(0.95, p, n-p)

# test statistic (85) belongs to the critical region (36), so we reject the H0, so they are different

library(MVTests)
hotel1=OneSampleHT2(Y, mu0)
hotel1$HT2
hotel1$p.value

library(DescTools)
hotel2=HotellingsT2Test(Y, mu=mu0)
hotel2$statistic
hotel2$p.value

for (i in 1:p){
  hotel = OneSampleHT2(Y[, i], mu0[i])
  print(paste("var ", i, " ", hotel$p.value))
}

for (i in 1:p){
  hotel = HotellingsT2Test(as.matrix(Y[, i]), mu=mu0[i])
  print(paste("var ", i, " ", hotel$p.value))
}

# b)
library(MVN)
mvn(data, mvnTest="mardia")$multivariateNormality

lambda = ((p^p*det(S_ml))/((sum(diag(S_ml)))^p))^(n/2)
stat = -2 * log(lambda)

critval = qchisq(1-alpha, p*(p+1)/2-1)
# It is in critical region, so we eject null hypothesis, so the thing is not equal to the diagonal

#c) compound symmetric matrix means that on the diagonal we have equal values but on the off-diagonal we have something different
mvn(data, mvnTest="mardia")$multivariateNormality

# H0 -> Sigma has compound symmetric structure
ssg = mean(diag(S_ml))
ro = 1/(p-1) * (t(rep(1, p)) %*% S_ml %*% rep(1, p) - sum(diag(S_ml)))/(sum(diag(S_ml)))
test_statistic = -n * log(det(S_ml)/((ssg^p) * (1-ro)^(p-1)*(1 + (p-1)*ro)))
crit = qchisq(1-alpha, p*(p+1)/2-2)

# The test statistic does NOT belong to the critical region, so we accept the null hypothesis
ramus_data = read.csv("ramus.csv", sep=";")

# Exercise 2
# a)
mu0 = c(48, 49, 50, 51)
Y = as.matrix(ramus_data[-1])
n = nrow(Y)
p = ncol(Y)
ybar = colMeans(Y)
Q = diag(n) - 1/n * rep(1, n*n, nrow=n)
S_ml = 1/n * t(Y) %*% Q %*% Y
S_ml_other = cov(Y) * (n-1)/n
all.equal(S_ml, S_ml_other)
t_sq = (n-1) * t(ybar - mu0) %*% solve(S_ml) %*% (ybar-mu0)
alpha = 0.05
crit_val = p*(n-1)/(n-p) * qf(0.95, p, n-p)

# Does not belong to critical region, so accept H0

# b)
Sigma0 = matrix(c(6.0, 5.9, 5.5, 5.2, 5.9, 6.1, 5.9, 5.6, 5.5, 5.9, 6.6, 6.6, 5.2, 5.6, 6.6, 7.0), nrow=4)
n*(sum(diag(solve(Sigma0)%*%S_ml)) - log(det(solve(Sigma0) %*% S_ml)) - p)
crit = qchisq(1-alpha, p*(p+1)/2)

# The test statistic belongs to the critical region, so we reject the null hypothesis

# Exercise 3
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
S_ml = 1/n * t(Y) %*% Q %*% Y
S_ml_other = cov(Y) * (n-1)/n
all.equal(S_ml, S_ml_other)

lambda = ((p^p*det(S_ml))/((sum(diag(S_ml)))^p))^(n/2)
stat = -2 * log(lambda)

critval = qchisq(1-alpha, p*(p+1)/2-1)
# It is not in critical region, so we accept the null hypothesis