#Lecture 1 -> Notes

# Defining a matrix
a = matrix(c(1,2,3,4,5,6), nrow=2)
b = matrix(c(7,8,9,10,11,12), nrow=2)
c = matrix(c(13, 14, 15, 16, 17, 18), nrow=2)

# Matrix multiplication and transpose
(a+b)%*%t(c) == a%*%t(c) + b%*%t(c)

# Defining a vector of ones
ones <- rep(1, times = 10)

# Defining a vector of zeros
zeros <- rep(0, times = 10)

# Unit vector
unit_vec <- rep(0, times = 10)
unit_vec[3] <- 1

# Diagonal matrix
diag_mat = diag(c(1, 2, 3, 4, 5))

# Trace
sum(diag(diag_mat))
sum(diag(a)) #not square, so the last column is not considered

# Determinant
det(diag_mat)
det(t(diag_mat))

# Singular matrix
# if determinant is 0, then matrix is singular (A singular matrix is a square matrix that cannot be inverted. -> Its rows or columns are linearly dependent)
A <- matrix(c(1, 2, 2, 4), nrow = 2)
det(A)
#solve(A) -> will give error -> Błąd w poleceniu 'solve.default(A)': procedura Lapack dgesv: system jest dokładnie osobliwy: U[2,2] = 0

# Rank -> maximum number of linearly independent rows (columns)
library(Matrix)
rankMatrix(A) # 1 indepenent row
rankMatrix(a) # 2 independent rows


# Plotting linear transformation
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

A = matrix(c(2, 0, 1, 3), nrow=2)
plot_linear_transformation(A)

# Matrix Inverse
solve(A)

# Generalized inverse / pseudo-inverse (Moore Penrose inverse) -> used if the matrix is not square or singular
library(MASS)
ginv(A)
ginv(a)

# Checking correctness of pseudo-inverse
ginv_a = ginv(a)
all.equal(a %*% ginv_a %*% a, a, tolerance=1e-8)
all.equal(t(a %*% ginv_a), a %*% ginv_a, tolerance=1e-8)
all.equal(ginv_a %*% a %*% ginv_a, ginv_a, tolerance=1e-8)
all.equal(t(ginv_a %*% a), ginv_a %*% a, tolerance=1e-8)

