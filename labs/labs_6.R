# Exercise 1
set.seed(42)
n = 5
p = 100
mu1 = rep(0, n)
mu2 = rep(0, n)

sigma1 = diag(rep(3, p))
sigma2 = matrix(rep(0.5, p*p), nrow=p) + diag(p)/2

library(mnormt)
pop1 = t(rmnorm(n=n, mean=mu1, varcov=sigma1))
pop1 = as.data.frame(pop1)
pop2 = t(rmnorm(n=n, mean=mu2, varcov=sigma2))
pop2 = as.data.frame(pop2)

library(heplots)
boxM(rbind(pop1, pop2), group = rep(1:2, each=p))
# P-val smaller than alpha, so we reject H0

library(Hotelling)
hotel = hotelling.test(pop1, pop2, var.equal=F)
hotel$pval


# Exercise 2
library(HSAUR2)
north = subset(water, location=="North")
south = subset(water, location=="South")

n1 = nrow(south)
n2 = nrow(north)
p=2

Q1 = diag(n1) - 1/n1 * rep(1, n1*n1, nrow=n1)
Q2 = diag(n2) - 1/n2 * rep(1, n2*n2, nrow=n2)

south_val <- south[, c(3, 4)]
north_val <- north[, c(3, 4)]
south_val <- as.matrix(south_val)
north_val <- as.matrix(north_val)

S1 = 1/(n1-1) * t(south_val) %*% Q1 %*% south_val
S2 = 1/(n2-1) * t(north_val) %*% Q2 %*% north_val
S = 1/(n1+n2-2) * ((n1 -1)*S1 + (n2-1)*S2)
M = (det(S1)/det(S))^((n1-1)/2) * (det(S2)/det(S))^((n2-1)/2)
c1 = (1/(n1-1) + (1/(n2-1)) - (1/(n1+n2-2)))*((2*p*p + 3*p - 1)/(6*(p+1)))
stat = -2*(1-c1)*log(M)
alpha=0.05
critical = qchisq(1-alpha, p*(p+1)/2)

# The test statistic is not in the critical region, so we accept the H0

hotelling.test(south_val, north_val)$pval

t1 = (mean(south_val) - mean(north_val))/(sqrt(((n1+n2)/(n1*n2)) * S[1, 1]))
t2 = (mean(south_val) - mean(north_val))/(sqrt(((n1+n2)/(n1*n2)) * S[2, 2]))
crit_left = -1*qt(1-alpha/2, n1+n2-2)
crit_right = qt(1-alpha/2, n1+n2-2)

# Exercise 3

Y1_bar = c(204.4, 556.6)
Y2_bar = c(130.0, 355.0)
S1 = matrix(c(13825.3, 23823.4, 23823.4, 73107.4), nrow=2)
S2 = matrix(c(8632.0, 19616.7, 19616.7, 55964.5), nrow=2)
n1 = 45
n2 = 55
p = 2
S = 1/(n1+n2-2) * ((n1 -1)*S1 + (n2-1)*S2)
M = (det(S1)/det(S))^((n1-1)/2) * (det(S2)/det(S))^((n2-1)/2)
c1 = (1/(n1-1) + (1/(n2-1)) - (1/(n1+n2-2)))*((2*p*p + 3*p - 1)/(6*(p+1)))
stat = -2*(1-c1)*log(M)
alpha=0.05
critical = qchisq(1-alpha, p*(p+1)/2)

# Stat in critical region, reject H0, so they are equal

# Exercise 4

# H0 = mu1 = mu2
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

