# Exercise 2
mu <- c(1, 1, 1)
Sigma <- matrix(c(2, 0, 1,
                  0, 3, 1,
                  1, 1, 2), nrow = 3, byrow = TRUE)

A <- matrix(c(1, 0, -1,
              1, -1, 1), nrow = 2, byrow = TRUE)

EY <- A %*% mu
CovY <- A %*% Sigma %*% t(A)

EY
CovY

# Exercise 3
A <- matrix(c(1, 1, 1, -1), nrow = 2, byrow = TRUE)
Sigma_X <- diag(2)

mu_Y <- A %*% c(0, 0)
Sigma_Y <- A %*% Sigma_X %*% t(A)
Sigma_Y

# Exercise 4
mu_Y <- c(3, 2)
Sigma_Y <- matrix(c(1, -1.5, -1.5, 4), nrow = 2)
U <- chol(Sigma_Y)
X <- matrix(rnorm(2 * 10000), nrow = 2)

Y <- U %*% X + matrix(mu_Y, ncol = 10000, nrow = 2)
colMeans(Y)
cov(t(Y))

# Exercise 5
mu = c(0, 0, -5)
sigma_inv = matrix(c(1/2, 0, 0, 0, 1, -1/4, 0, -1/4, 1/4), nrow=3)
solve(sigma_inv)


# Exercise 6
library(mvtnorm)

# Define parameters for two distributions
params <- list(
  list(mu = c(0, 0), Sigma = matrix(c(1, 0.6, 0.6, 1), 2)),
  list(mu = c(1, -2), Sigma = matrix(c(2, -0.8, -0.8, 1), 2))
)

# Grid
x <- seq(-6, 6, length = 100)
y <- seq(-6, 6, length = 100)
grid <- expand.grid(x = x, y = y)

# Setup plotting area: 2 rows, 2 columns
par(mfrow = c(2, 2), mar = c(2, 2, 3, 2))

# Loop over the two parameter sets
for (p in params) {
  mu <- p$mu
  Sigma <- p$Sigma
  
  # Density values for 3D plot
  z <- matrix(dmvnorm(grid, mean = mu, sigma = Sigma), nrow = length(x))
  
  # 3D surface plot
  persp(x, y, z,
        theta = 30, phi = 30, expand = 0.6,
        col = "snow", ticktype = "detailed",
        xlab = "X", ylab = "Y", zlab = "Density",
        main = paste("3D: mu=(", paste(mu, collapse=","), "), rho=", round(Sigma[1,2], 2)))
  
  # Mahalanobis distance for contour ellipses
  mahal <- mahalanobis(grid, center = mu, cov = Sigma)
  mahal_matrix <- matrix(mahal, nrow = length(x))
  levels <- qchisq(c(0.68, 0.95, 0.99), df = 2)
  
  # Contour plot
  contour(x, y, mahal_matrix,
          levels = levels,
          labels = paste0(round(c(68, 95, 99)), "%"),
          xlab = "", ylab = "",
          main = paste("Ellipses: mu=(", paste(mu, collapse=","), "), rho=", round(Sigma[1,2], 2)))
}


# Exercise 7
set.seed(151945)
library(MASS)
p = 5
n = 200
runs = 100
mu = rep(0, p)
Sigma = diag(p)

library(mvtnorm)

results = c()
for (i in 1:runs){
  x = rmvnorm(n, mu, Sigma)
  x_bar = colMeans(x)
  x_cov = cov(x)
  results = rbind(results, x_bar)
}

x_bar = colMeans(results)
Q = diag(runs) - 1/runs * matrix(rep(1, runs^2), runs)
S = 1/(runs-1) * t(results) %*% Q %*% results
round(1/n * Sigma - S, 3)

