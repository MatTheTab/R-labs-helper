#Lecture 4 -> Notes

# Plotting multivariate
library(mnormt)

x = seq(-5, 5, 0.25)
y = seq(-5, 5, 0.25)
mu = c(0, 0)
sigma = diag(2)
f = function(x, y) dmnorm(cbind(x, y), mu, sigma)
z = outer(x, y, f)
par(mfrow=c(1,2))
persp(x, y, z, theta = -30, phi = 25, shade = 0.75, expand = 0.5, r = 2, ltheta = 25, col = "gold")
contour(x, y, z)

# Wishart Distribution
# The distribution of sample covariance matrices computed from multivariate normal data; it describes how covariance "wiggles" from sample to sample.

library(MCMCpack)

Sigma <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
df <- 5
W <- rwish(v = df, S = Sigma)

# Hotelling’s T² Distribution
# A multivariate version of the Student's t-distribution, used to test hypotheses about mean vectors when data comes from a multivariate normal distribution.

library(Hotelling)

group1 <- matrix(rnorm(30), ncol = 3)
group2 <- matrix(rnorm(30, mean = 0.5), ncol = 3)
out <- hotelling.test(group1, group2)
out
