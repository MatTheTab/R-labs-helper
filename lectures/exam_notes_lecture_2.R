#Lecture 2 -> Notes

# Eigenvectors and eigenvalues
a = matrix(c(1,2,3,4,5,6, 7, 8, 9), nrow=3)
eig = eigen(a)
eig$vectors
eig$values

# (Must be symmetric for it to be PD or PSD) Positive Positive definite (PD) -> all eigenvalues > 0; positive semi-definite (PSD) -> all eigenvalues >= 0
eig$values > 0 # Not PD or PSD
isSymmetric(a)

A <- matrix(c(2, -1, -1, 2), nrow = 2)
isSymmetric(A)
eigen(A)$values #PD

# Transformations
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

horizontal <- matrix(c(2, 0, 0, 1), nrow=2)
vertical <- matrix(c(1, 0, 0, 2), nrow=2)
rotation <- matrix(c(cos(pi/4), sin(pi/4), -sin(pi/4), cos(pi/4)), nrow=2)

plot_linear_transformation(horizontal)
plot_linear_transformation(vertical)
plot_linear_transformation(rotation)
plot_linear_transformation(matrix(c(3,4,0,5), nrow=2))

# SVD (Singular Value Decomposition) -> U (Rotation), D (Scaling), V' (Rotation)
svd_a = svd(a)
all.equal(svd_a$u %*% diag(svd_a$d) %*% t(svd_a$v), a)


# Orthogonal projection
# <<<<>>>>
# Projects y on x
project_vector <- function(y, x) {
  y <- as.matrix(y)
  x <- as.matrix(x)
  
  if (length(y) != length(x)) {
    stop("Vectors y and x must have the same length.")
  }

  scalar <- sum(y * x) / sum(x * x)
  proj <- scalar * x
  
  return(proj)
}

y = c(1, 4, 2)
x = c(0, 0, 1)
project_vector(y, x)

y = c(1, 4, 2)
x = c(2, 3, 1)
project_vector(y, x)


# Gram-SChmidt orthogonalization
library(pracma)
gramSchmidt(A)$Q

x1 <- c(1, 2, 3, 4, 5)
x2 <- c(2, 1, 0, -1, -2)
x3 <- c(3, 3, 3, 3, 3)
x4 <- c(1, -1, 1, 0, 1)

A <- cbind(x1, x2, x3, x4)
Q <- qr.Q(qr(A))
round(t(Q) %*% Q, 1)


# Orthocomplement space -> All vectors in R^n n that are orthogonal (at right angles) to every vector in 𝑉
library(MASS)

A <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3)
V_orthocomplement <- Null(t(A))
t(A) %*% V_orthocomplement

# Projection to column space
library(pracma)
library(MASS)
orth(A)

# all.equal(orth(Y) %*% t(orth(Y)), Y%*%solve(t(Y)%*%Y)%*%t(Y)) <- orthonormal projection

get_orthogonal_projection <- function(A){
  proj = A %*% solve(t(A)%*%A) %*% t(A)
  return (proj)
}

get_orthogonal_projection_pseudoinverse <- function(A){
  proj = A %*% ginv(t(A)%*%A) %*% t(A)
  return (proj)
}

get_projection_onto_the_orthocomplement_space<- function(A){
  I <- diag(nrow(A))
  proj = I - (A %*% solve(t(A)%*%A) %*% t(A))
  return (proj)
}

get_projection_onto_the_orthocomplement_space_pseudoinverse<- function(A){
  I <- diag(nrow(A))
  proj = I - (A %*% ginv(t(A)%*%A) %*% t(A))
  return (proj)
}

get_orthogonal_projection_pseudoinverse(A)
get_projection_onto_the_orthocomplement_space_pseudoinverse(A)

# Testing functions: 
P = get_orthogonal_projection_pseudoinverse(A)
P_perp = get_projection_onto_the_orthocomplement_space_pseudoinverse(A)
proj_b <- P %*% A
proj_b_perp <- P_perp %*% A
all.equal(A, proj_b + proj_b_perp)


# Kroneker Delta
A <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3)
B <- matrix(c(10, 20, 30, 40, 50, 60, 70, 80, 90), nrow = 3)
kronecker(A,B)

