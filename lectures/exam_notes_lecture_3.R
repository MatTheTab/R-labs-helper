#Lecture 3 -> Notes

# Uniform Distribution
v_uniform <- runif(10, min = 0, max = 1)
m_uniform <- matrix(runif(25, min = 0, max = 1), nrow = 5, ncol = 5)

# Normal Distribution
v_normal <- rnorm(10, mean = 0, sd = 1)
m_normal <- matrix(rnorm(25, mean = 0, sd = 1), nrow = 5, ncol = 5)

# Multivariate normal distribution
library(MASS)

mu <- c(0, 0)
Sigma <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
m_mvn <- mvrnorm(n = 100, mu = mu, Sigma = Sigma)

# Marginal probabilities
prob_matrix <- matrix(runif(25), nrow = 5)
prob_matrix <- prob_matrix / sum(prob_matrix)
row_marginals <- rowSums(prob_matrix)
col_marginals <- colSums(prob_matrix)

# Covariance and Correlation
x <- rnorm(100)
y <- x + rnorm(100, sd = 0.5)

cov(x, y)
cor(x, y)

data_matrix <- cbind(x, y, rnorm(100))
cov(data_matrix)
cor(data_matrix)

# Moments
library(moments)

z <- rnorm(1000)

mean(z)        # 1st moment (mean)
var(z)         # 2nd central moment (variance)
moment(z, 3)   # 3rd raw moment
moment(z, 3, central = TRUE)  # 3rd central moment
moment(z, 4)   # 4th raw moment
skewness(z)    # Skewness (normalized 3rd central moment)
kurtosis(z)    # Kurtosis (normalized 4th central moment)

