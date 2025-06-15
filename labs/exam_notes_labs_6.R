# Exercises:
## 1 -> testing sigmas, testing means for same sigmas, testing means for different sigmas (manually and with package)

## 2 -> testing sigmas, testing means (with packages) for same sigmas, post-hog test

## 3 -> testing sigma (manually)

## 4 -> testing means for same sigmas, post-hog test

#########################################################################################################################################################
# Exercise 1 -> generate two datasets, compare means and covariances, produce some plots (Here I tested for 3 datasets)

## Creating the Data
set.seed(151945)
n = 10
p = 5
mu1 = rep(0, p)
mu2 = rep(0, p)
mu3 = rep(0, p)

sigma1 = diag(rep(3, p))
sigma2 = matrix(rep(0.5, p*p), nrow=p) + diag(p)/2
sigma3 = diag(rep(3, p))

library(mnormt)
pop1 = rmnorm(n=n, mean=mu1, varcov=sigma1)
pop1 = as.data.frame(pop1)
pop2 = rmnorm(n=n, mean=mu2, varcov=sigma2)
pop2 = as.data.frame(pop2)
pop3 = rmnorm(n=n, mean=mu3, varcov=sigma3)
pop3 = as.data.frame(pop3)

## Testing Normality
library(MVN)
mvn(pop1, mvnTest="mardia")$multivariateNormality  # The test confirms normality of the data
mvn(pop2, mvnTest="mardia")$multivariateNormality  # The test confirms normality of the data
mvn(pop3, mvnTest="mardia")$multivariateNormality  # The test confirms normality of the data

## Testing if sigmas covariance matrices are the same (Here we test 1-vs-2 AND 1-vs-3)
### Manually
n1 = nrow(pop1)
n2 = nrow(pop2)
n3 = nrow(pop3)
p = ncol(pop1)

Y1 = as.matrix(pop1)
Y2 = as.matrix(pop2)
Y3 = as.matrix(pop3)

Q1 = diag(n1) - 1/n1 * rep(1, n1*n1, nrow=n1)
Q2 = diag(n2) - 1/n2 * rep(1, n2*n2, nrow=n2)
Q3 = diag(n3) - 1/n3 * rep(1, n3*n3, nrow=n3)

S1 = (1/(n1-1)) * t(Y1) %*% Q1 %*% Y1
S2 = (1/(n2-1)) * t(Y2) %*% Q2 %*% Y2
S3 = (1/(n3-1)) * t(Y3) %*% Q3 %*% Y3

S12 = (1/(n1+n2-2)) * ((n1-1) * S1 + (n2-1) * S2) # Pooled covariance
S13 = (1/(n1+n3-2)) * ((n1-1) * S1 + (n3-1) * S3) # Pooled covariance

M12 = ((det(S1)/det(S12))^((n1-1)/2)) * ((det(S2)/det(S12))^((n2-1)/2))
M13 = ((det(S1)/det(S13))^((n1-1)/2)) * ((det(S3)/det(S13))^((n3-1)/2))

c12 = ((1/(n1-1)) + (1/(n2-1)) - (1/(n1+n2-2))) * ((2*p*p + 3*p -1)/(6*(p+1)))
c13 = ((1/(n1-1)) + (1/(n3-1)) - (1/(n1+n3-2))) * ((2*p*p + 3*p -1)/(6*(p+1)))

test_stat12 = -2*(1-c12)*log(M12)
test_stat13 = -2*(1-c13)*log(M13)

# H0: sigma1 = sigma2
# H1: sigma1 != sigma2
alpha=0.05
crit_region12 = qchisq(1-alpha, p*(p+1)/2)
crit_region13 = qchisq(1-alpha, p*(p+1)/2)
# For 1-vs-2: the test statistic lies within the critical region, so we reject H0, hence, sigmas are different
# For 1-vs-3: the test statistic does not lie within the critical region, so we do not reject H0, hence, sigmas are the same

### From package
# H0: sigma1 = sigma2
# H1: sigma1 != sigma2
library(heplots)
boxM(rbind(pop1, pop2), group = rep(1:2, each=n1)) # P-val smaller than alpha, so we reject H0, hence, the sigmas are not equal
boxM(rbind(pop1, pop3), group = rep(1:2, each=n1)) # P-val greater than alpha, so we do not reject H0, hence, the sigmas are equal

## Testing if means are the same (Here we test 1-vs-2 AND 1-vs-3)
### Manually for DIFFERENT sigmas (1-vs-2) -> Might require a double-check
# H0: mu1 = mu2
# H1: mu1 != mu2
n1 = nrow(pop1)
n2 = nrow(pop2)
p = ncol(pop1)

Y1 = as.matrix(pop1)
Y2 = as.matrix(pop2)

Q1 = diag(n1) - 1/n1 * rep(1, n1*n1, nrow=n1)
Q2 = diag(n2) - 1/n2 * rep(1, n2*n2, nrow=n2)

S1 = (1/n1) * t(Y1) %*% Q1 %*% Y1
S2 = (1/n2) * t(Y2) %*% Q2 %*% Y2

Y1_bar = colMeans(Y1)
Y2_bar = colMeans(Y2)

test_stat = (t(Y1_bar - Y2_bar)) %*% (solve((1/(n1-1))*S1 + (1/(n2-1))*S2)) %*% (Y1_bar - Y2_bar)
alpha=0.05
crit_region = qchisq(1-alpha, p)
# The test statistic is not within the critical region, we do not reject H0, hence, the means are the same

### Manually for SAME sigmas (1-vs-3)
# H0: mu1 = mu2
# H1: mu1 != mu2
n1 = nrow(pop1)
n3 = nrow(pop3)
p = ncol(pop1)

Y1 = as.matrix(pop1)
Y3 = as.matrix(pop3)

Q1 = diag(n1) - 1/n1 * rep(1, n1*n1, nrow=n1)
Q3 = diag(n3) - 1/n3 * rep(1, n3*n3, nrow=n3)

S1 = (1/n1) * t(Y1) %*% Q1 %*% Y1
S3 = (1/n3) * t(Y3) %*% Q3 %*% Y3

Y1_bar = colMeans(Y1)
Y3_bar = colMeans(Y3)

S = (1/(n1+n3-2)) * (n1*S1 + n3*S3)
test_stat = ((n1*n3)/(n1+n3))*(t(Y1_bar-Y3_bar))%*%(solve(S))%*%(Y1_bar - Y3_bar)
alpha=0.05
crit_region = ((p*(n1+n3-2))/(n1+n3-p-1))*qf(1-alpha, p, n1+n3-p-1)
# The test statistic is not within the critical region, we do not reject H0, hence, the means are the same

### From package
# H0: mu1 = mu2
# H1: mu1 != mu2
library(Hotelling)
hotelling.test(pop1, pop2, var.equal=F)$pval # P-val greater than alpha, so we do not reject H0, hence, the means are equal
hotelling.test(pop1, pop3, var.equal=T)$pval # P-val greater than alpha, so we do not reject H0, hence, the means are equal

#########################################################################################################################################################
# Exercise 2 -> Examine sigmas and means of water data, plus post-hog test

library(HSAUR2)
water
data = water[, -2]
north = subset(data, location=="North")
south = subset(data, location=="South")
n1 = nrow(north)
n2 = nrow(south)
Y1 = as.matrix(north[3:4])
Y2 = as.matrix(south[3:4])
p = ncol(Y1)

# H0: sigma1 = sigma2
# H1: sigma1 != sigma2
boxM(rbind(Y1, Y2), group = rep(c(1, 2), times = c(n1, n2))) # P-val greater than alpha, so we do not reject H0, hence, the sigmas are equal
hotelling.test(Y1, Y2, var.equal=T)$pval # P-val less than alpha, so we reject H0, hence, the means are not equal

## Post-Hog test
t1 = (mean(Y2[1]) - mean(Y1[, 1]))/(sqrt(((n1+n2)/(n1*n2)) * S[1, 1]))
t2 = (mean(Y2[2]) - mean(Y1[, 2]))/(sqrt(((n1+n2)/(n1*n2)) * S[2, 2]))
alpha=0.05
# They both contribute but feature 2 more so causes the H0 to be rejected

# Critical Region from (-INF, crit_left) (crit_right, INF)
crit_left = -1*qt(1-alpha/2, n1+n2-2) # No Bonferroni correction
crit_right = qt(1-alpha/2, n1+n2-2) # No Bonferroni correction

# Critical Region from (-INF, crit_left) (crit_right, INF)
crit_left = -1*qt(1-alpha/(2*p), n1+n2-2) # WITH Bonferroni correction
crit_right = qt(1-alpha/(2*p), n1+n2-2) # WITH Bonferroni correction

#########################################################################################################################################################
# Exercise 3 -> Examine sigmas and means of water data, plus post-hog test
Y1_bar = c(204.4, 556.6)
Y2_bar = c(130.0, 355.0)
S1 = matrix(c(13825.3, 23823.4, 23823.4, 73107.4), nrow=2)
S2 = matrix(c(8632.0, 19616.7, 19616.7, 55964.5), nrow=2)
n1 = 45
n2 = 55
p = 2

Q1 = diag(n1) - 1/n1 * rep(1, n1*n1, nrow=n1)
Q2 = diag(n2) - 1/n2 * rep(1, n2*n2, nrow=n2)
S12 = (1/(n1+n2-2)) * ((n1-1) * S1 + (n2-1) * S2) # Pooled covariance
M12 = ((det(S1)/det(S12))^((n1-1)/2)) * ((det(S2)/det(S12))^((n2-1)/2))
c12 = ((1/(n1-1)) + (1/(n2-1)) - (1/(n1+n2-2))) * ((2*p*p + 3*p -1)/(6*(p+1)))

test_stat = -2*(1-c12)*log(M12)
# H0: sigma1 = sigma2
# H1: sigma1 != sigma2
alpha=0.05
crit_region12 = qchisq(1-alpha, p*(p+1)/2)
# Stat in critical region, reject H0, so they are equal

#########################################################################################################################################################
# Exercise 4 -> Examine means of psychological evaluations and post-hog test

# H0 -> mu1 = mu2
# H1 -> mu1 != mu2
setwd("C:/Users/mateu/OneDrive/Pulpit/R_labs_Master_Degree_Studies")
data = read.csv("psycho.csv", sep=";")
data
n1 = 32
n2 = 32
data
men = as.matrix(data[, 1:4])
women = as.matrix(data[, 5:8])
hotel = hotelling.test(men, women, ver.equal=T)
hotel$pval
# statistic smaller than p-val, so we reject H0

Q1 = diag(n1) - 1/n1 * rep(1, n1*n1, nrow=n1)
Q2 = diag(n2) - 1/n2 * rep(1, n2*n2, nrow=n2)
S1 = 1/(n1-1) * t(men) %*% Q1 %*% men
S2 = 1/(n2-1) * t(women) %*% Q2 %*% women
S = 1/(n1+n2-2) * ((n1 -1)*S1 + (n2-1)*S2)

t1 = (mean(men[, 1]) - mean(women[, 1]))/(sqrt(((n1+n2)/(n1*n2)) * S[1, 1]))
t2 = (mean(men[, 2]) - mean(women[, 2]))/(sqrt(((n1+n2)/(n1*n2)) * S[2, 2]))
t3 = (mean(men[, 3]) - mean(women[, 3]))/(sqrt(((n1+n2)/(n1*n2)) * S[3, 3]))
t4 = (mean(men[, 4]) - mean(women[, 4]))/(sqrt(((n1+n2)/(n1*n2)) * S[4, 4]))
crit_left = -1*qt(1-alpha/2, n1+n2-2)
crit_right = qt(1-alpha/2, n1+n2-2)

crit_left = -1*qt(1-alpha/(2*p), n1+n2-2)
crit_right = qt(1-alpha/(2*p), n1+n2-2)
