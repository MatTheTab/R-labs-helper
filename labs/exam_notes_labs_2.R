# Exercise 1
x <- seq(2, 20, by=2)
x %*% t(x)
t(x) %*% x
rev(x)
sum(x)
length(x)

# Exercise 2
A = matrix(c(2,1,1,3,1,1,-2,2,-1), nrow=3)
det(A)
sum(diag(A))
A^2
A%*%A
A*A
A %*% diag(A)
a = A[, 3]
b = A[2, ]
a %*% b
a %*% t(b)
round(sum(eigen(A)$values), 5) == round(sum(diag(A)), 5)
round(prod(eigen(A)$values), 5) == round(det(A), 5)
eigen(A%*%t(A)) # The eigenvalues are orthonormal, and all are greater than or equal to 0 (the matrix is positiv semidefinite)
svd(A)
svd_A <- svd(A)
U <- svd_A$u
D <- diag(svd_A$d)
V <- svd_A$v
rank1 <-  D[1,1] * U[, 1] %*% t(V[, 1])
rank2 <-  D[2,2] * U[, 2] %*% t(V[, 2])
rank3 <-  D[3,3] * U[, 3] %*% t(V[, 3])
rank1 + rank2 + rank3
rank(rank1)
rank(rank2)
rank(rank3)

# Exercise 3
B = matrix(c(2, 0, 1, 3, -1, 0, 1, 2, 1), nrow=3)
sum(diag(B))
det(B)
eigen(B)
round(sum(eigen(B)$values), 5) == round(sum(diag(B)), 5)
round(prod(eigen(B)$values), 5) == round(det(B), 5)

# Exercise 4
X = matrix(c(3, 4, 0, 5), nrow=2)
Y = matrix(c(1.8, 4.4, 1.2, 4.6), nrow=2)

plot_linear_transformation <- function(A) {
  if (!all(dim(A) == c(2, 2))) {
    stop("Matrix A must be 2x2.")
  }
  
  square <- matrix(c(0, 0,
                     1, 0,
                     1, 1,
                     0, 1,
                     0, 0), ncol = 2, byrow = TRUE)
  
  transformed_square <- t(A %*% t(square))
  
  e1 <- c(1, 0)
  e2 <- c(0, 1)
  Te1 <- A %*% e1
  Te2 <- A %*% e2
  
  plot(NULL, xlim = c(-1, max(4, transformed_square[,1])),
       ylim = c(-1, max(4, transformed_square[,2])),
       xlab = "X", ylab = "Y", asp = 1,
       main = "Linear Transformation of Unit Square")
  
  lines(square[,1], square[,2], col = "blue", lwd = 2)
  lines(transformed_square[,1], transformed_square[,2], col = "red", lwd = 2)
  
  arrows(0, 0, e1[1], e1[2], col = "blue", lwd = 2)
  arrows(0, 0, e2[1], e2[2], col = "blue", lwd = 2)
  
  arrows(0, 0, Te1[1], Te1[2], col = "red", lwd = 2)
  arrows(0, 0, Te2[1], Te2[2], col = "red", lwd = 2)
  
  legend("topleft", legend = c("Original", "Transformed"),
         col = c("blue", "red"), lwd = 2)
}

plot_linear_transformation(X)
plot_linear_transformation(Y)

svd_X = svd(X)
svd_Y = svd(Y)
X_rank1 <-  svd_X$d[1] * svd_X$u[, 1] %*% t(svd_X$v[, 1])
Y_rank1 <-  svd_Y$d[1] * svd_Y$u[, 1] %*% t(svd_Y$v[, 1])
X - X_rank1
Y - Y_rank1

# Exercise 5
C = matrix(c(3,5,5,9,1,9,3,7,4,2,5,9,1,6,8,3), nrow=4)
svd_C = svd(C)
C_rank1 <-  svd_C$d[1] * svd_C$u[, 1] %*% t(svd_C$v[, 1])
C_rank2 <-  svd_C$d[2] * svd_C$u[, 2] %*% t(svd_C$v[, 2])
C_rank3 <-  svd_C$d[3] * svd_C$u[, 3] %*% t(svd_C$v[, 3])
C_rank4 <-  svd_C$d[4] * svd_C$u[, 4] %*% t(svd_C$v[, 4])
all.equal(C, C_rank1 + C_rank2 + C_rank3 + C_rank4)

# Exercise 6
Y = rbind(A, rep(1, 3))
svd(Y)

library(MASS)
ginv(Y)
ginv_y = ginv(Y)
all.equal(Y %*% ginv_y %*% Y, Y, tolerance=1e-8)
all.equal(t(Y %*% ginv_y), Y %*% ginv_y, tolerance=1e-8)
all.equal(ginv_y %*% Y %*% ginv_y, ginv_y, tolerance=1e-8)
all.equal(t(ginv_y %*% Y), ginv_y %*% Y, tolerance=1e-8)

# D) -> orthonormal projection
all.equal(orth(Y) %*% t(orth(Y)), Y%*%solve(t(Y)%*%Y)%*%t(Y)) # Orthonormal projection

### E) -> Gram Schmidt
library(pracma)
Q = gramSchmidt(Y)$Q

proj = function(x)(x%*%solve(t(x)%*%x)%*%t(x))
projM = function(x)(x%*%ginv(t(x)%*%x)%*%t(x))
Y %*% projM(t(Y))

x1 <- Y[,1]
x2 <- Y[,2]
x3 <- Y[,3]
v1 <- x1/sqrt(sum(x1^2))
v2 <- x2 - proj(v1)%*%x2
v2 <- v2/sqrt(sum(v2^2))
v3 <- x3 - proj(v1)%*%x3 - proj(v2)%*%x3
v3 <- v3/sqrt(sum(v3^2))
proj(Y) %*% Y
newQ <- cbind(v1, v2, v3)

proj_gSch <- function(u, v) {
  (sum(u * v) / sum(u * u)) * u
}

gram_schmidt <- function(Y) {
  n <- ncol(Y)
  m <- nrow(Y)
  Q <- matrix(0, nrow = m, ncol = n)
  
  for (i in 1:n) {
    vi <- Y[, i]
    
    if (i > 1) {
      for (j in 1:(i - 1)) {
        vi <- vi - proj_gSch(Q[, j], Y[, i])
      }
    }
    
    norm_vi <- sqrt(sum(vi^2))
    if (norm_vi < 1e-10) stop("Linearly dependent or zero vector detected.")
    
    Q[, i] <- vi / norm_vi
  }
  
  return(Q)
}

gram_schmidt(Y)

## F) -> Projection into column space
PY <- Y %*% solve(t(Y) %*% Y) %*% t(Y)
PY %*% Y[,1]
PY %*% Y[,2]
PY %*% Y[,3]
round(PY %*% Y, 5) == round(Y, 5)

## G) H) -> Row space
# Transpose Y so that rows become columns
PY_row <- t(Y) %*% ginv(Y %*% t(Y)) %*% Y
Y_rows <- t(Y)
round(PY_row %*% Y_rows, 5) == round(Y_rows, 5)
