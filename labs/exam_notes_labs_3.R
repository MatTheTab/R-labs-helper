# Important note:
# Y=AX	mean = A mu Sigma = A sigma t(A)
# Y = LX + mu mu Lt(L) = Sigma

# Exercise 1
library(MASS)
set.seed(42)
p <- 3
mu <- c(1, 2, 3)
Sigma <- matrix(c(2, 1, 0,
                  1, 2, 1,
                  0, 1, 2), nrow = 3)

A <- matrix(c(1, 2, 3,
              2, 5, 6,
              3, 6, 9), nrow = 3)

A <- (A + t(A)) / 2
n <- 1e5
X <- mvrnorm(n = n, mu = mu, Sigma = Sigma)

xAx_values <- apply(X, 1, function(x) t(x) %*% A %*% x)
empirical_expectation <- mean(xAx_values)

theoretical_expectation <- sum(diag(A %*% Sigma)) + t(mu) %*% A %*% mu
cat("Empirical E[X'AX]:", empirical_expectation, "\n")
cat("Theoretical E[X'AX]:", theoretical_expectation, "\n")

# Exercise 2
mu <- c(1, 1, 1)
Sigma <- matrix(c(2, 0, 1,
                  0, 3, 1,
                  1, 1, 2), nrow = 3, byrow = TRUE)

A <- matrix(c(1, 0, -1,
              1, -1, 1), nrow = 2, byrow = TRUE)

EY <- A %*% mu
CovY <- A %*% Sigma %*% t(A)


# Exercise 3
A = matrix(c(1, 1, 1, -1), nrow=2, byrow=TRUE)
Sigma_X <- diag(2)
mu_Y <- A %*% c(0, 0)
A %*% mu_Y
A %*% Sigma_X %*% A

# In a multivariate normal distribution, components are independent if and only if they are uncorrelated (i.e., the off-diagonal elements of the covariance matrix are zero).
# Since off-diagonals are 0, they are independent

# Exercise 4
mu = c(0, 0)
sigma = matrix(c(1, 0, 0, 1), byrow=T, nrow=2)
target_mu = c(3, 2)
target_sigma = matrix(c(1, -1.5, -1.5, 4), nrow=2)

L <- t(chol(target_sigma)) # chelonsy gives us LtL, but we want LLt, hence transpose

new_mu = mu + target_mu
new_sigma = L %*% sigma %*% t(L)

# Exercise 5
# -1/8 (2x1^2 + 4x2^2 - 2x2x3 + 10x2 + x3^2 + 10x3 + 25)
# Sigma = c(2, 0, 0, 0, 4, -2, 0, -2, 1)
sigma_inv = matrix(c(2, 0, 0, 0, 4, -2, 0, -2, 1), nrow=3, byrow = T)
sigma = ginv(sigma_inv)
mu = c(0, 0, -5)

# Exercise 6
library(mnormt)

par(mfrow=c(2,2))
x = seq(-5, 5, 0.25)
y = seq(-5, 5, 0.25)
mu = c(0, 0)
sigma = diag(2)
f = function(x, y) dmnorm(cbind(x, y), mu, sigma)
z = outer(x, y, f)
persp(x, y, z, theta = -30, phi = 25, shade = 0.75, expand = 0.5, r = 2, ltheta = 25, col = "gold")
contour(x, y, z)

mu2 = c(1, 3)
sigma2 = matrix(c(1, 2, 2, 5), nrow=2)
f2 = function(x, y) dmnorm(cbind(x, y), mu2, sigma2)
z2 = outer(x, y, f2)
persp(x, y, z2, theta = -30, phi = 25, shade = 0.75, expand = 0.5, r = 2, ltheta = 25, col = "gold")
contour(x, y, z2)


# Exercise 7
set.seed(151945)  # For reproducibility

# Parameters
p <- 5           # Dimension
n <- 20          # Sample size
num_samples <- 100

# Store means and covariances
sample_means <- matrix(0, nrow=num_samples, ncol=p)
sample_covariances <- array(0, dim=c(p, p, num_samples))

# Generate and compute
for (i in 1:num_samples) {
  sample <- matrix(rnorm(n * p), nrow=n, ncol=p)  # Each row = observation
  sample_means[i, ] <- colMeans(sample)
  sample_covariances[,,i] <- cov(sample)
}

# 1. Average of sample means
avg_sample_mean <- colMeans(sample_means)

# 2. Covariance of sample means
mean_covariance <- cov(sample_means)

# 3. Average of sample covariance matrices
avg_sample_cov <- apply(sample_covariances, c(1,2), mean)

# Print results
cat("Average of sample means:\n")
print(avg_sample_mean)

cat("\nCovariance of sample means (should be close to I/n):\n")
print(mean_covariance)

cat("\nAverage of sample covariance matrices (should be close to I):\n")
print(avg_sample_cov)

# Theoretical values:
cat("\nTheoretical mean: 0\n")
cat("Theoretical covariance of sample means: Identity / 20\n")
