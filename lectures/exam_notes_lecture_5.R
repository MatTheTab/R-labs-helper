#Lecture 5 -> Notes

# Total covariance and variance
X <- matrix(rnorm(100 * 3), ncol = 3)
cov_matrix <- cov(X)
total_variance <- sum(diag(cov_matrix))
total_covariance <- sum(cov_matrix)


# Maximum Likelihood Estimation
library(MASS)
set.seed(42)
data <- rnorm(100, mean = 5, sd = 2)
fit <- fitdistr(data, densfun = "normal")
fit$estimate
