vect <- seq(2, 20, by=1)
x <- vect %*% t(vect)
t_x <- t(vect) %*% vect
rev(vect)
sum(vect)
length(vect)
A <- cbind(c(2, 1, 1), c(3, 1, 1), c(-2, 2, -1))
det(A)
sum(diag(A))
A^2
A%*%A
A*A
A%*%diag(A)
solve(A)
a <- A[,3]
b <- A[2, ]
a%*%b # Scalar
a%*%t(b) # Matrix
vals <- eigen(A)$values
sum(vals)
vals[1] * vals[2] * vals[3]
eigen_A <- eigen(A%*%t(A))
eigen_A$values
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

B <- cbind(c(2,0,1), c(3, -1, 0), c(1, 2, 1))
sum(diag(B))
det(B)
eigen_B <- eigen(B)
eigen_vals <- eigen_B$values
sum(eigen_vals)
prod(eigen_vals)

X <- cbind(c(3, 4), c(0, 5))
Y <- cbind(c(1.8, 4.4), c(1.2, 4.6))

plot_matrix_transform <- function(M, col = "blue", title = "Transformation") {
  # Define unit square
  square <- matrix(c(0, 0,
                     1, 0,
                     1, 1,
                     0, 1,
                     0, 0), byrow = TRUE, ncol = 2)
  
  # Apply transformation
  transformed <- t(M %*% t(square))
  
  # Set plot limits dynamically
  xlim <- range(c(square[,1], transformed[,1]))
  ylim <- range(c(square[,2], transformed[,2]))
  
  plot(0, 0, type = "n", asp = 1, xlab = "X", ylab = "Y", main = title,
       xlim = xlim * 1.1, ylim = ylim * 1.1)
  
  # Original unit square (dashed)
  polygon(square, border = "gray", lty = 2)
  text(0.5, 0.5, "Unit Square", col = "gray40")
  
  # Transformed square
  polygon(transformed, border = col, col = adjustcolor(col, alpha.f = 0.3), lwd = 2)
  text(transformed[3,1], transformed[3,2], "Transformed", col = col)
}

# Matrices
X <- cbind(c(3, 4), c(0, 5))
Y <- cbind(c(1.8, 4.4), c(1.2, 4.6))

# Plot both side by side
par(mfrow = c(1, 2))
plot_matrix_transform(X, col = "blue", title = "Transformation X")
plot_matrix_transform(Y, col = "red", title = "Transformation Y")
X.svd <- svd(X)
Y.svd <- svd(Y)

X.svd.rank1 <-  X.svd$d[1] * X.svd$u[, 1] %*% t(X.svd$v[, 1])
X.svd.rank2 <-  X.svd$d[2] * X.svd$u[, 2] %*% t(X.svd$v[, 2])

Y.svd.rank1 <-  Y.svd$d[1] * Y.svd$u[, 1] %*% t(Y.svd$v[, 1])
Y.svd.rank2 <-  Y.svd$d[2] * Y.svd$u[, 2] %*% t(Y.svd$v[, 2])

X.svd.rank1 + X.svd.rank2
Y.svd.rank1 + Y.svd.rank2


# Exercise 4

X <- cbind(c(3,4), c(0, 5))
Y <- cbind(c(1.8, 4.4), c(1.2, 4.6))
# -------------X---------------
X_svd = svd(X)
Xd <- diag(X_svd$d) #Scaling
Xu <- X_svd$u #Rotation
Xv <- X_svd$v #Rotation
atan(t(Xv)[2, 1] / t(Xv)[1, 1])*180/pi
h = matrix(c(-1, 0, 0, 1), ncol=2)
Xu %*% h %*% Xd %*% h %*% t(Xv)
Xv %*% h
atan(Xu[2, 1] / Xu[1, 1])*180/pi

# -------------Y---------------

Y_svd = svd(Y)
Yd <- diag(Y_svd$d) #Scaling
Yu <- Y_svd$u #Rotation
Yv <- Y_svd$v #Rotation
atan(t(Yv)[2, 1] / t(Yv)[1, 1])*180/pi
h = matrix(c(-1, 0, 0, 1), ncol=2)
Yu %*% h %*% Yd %*% h %*% t(Yv)
Yv %*% h
atan(Yu[2, 1] / Yu[1, 1])*180/pi

# 4 C)

X.svd.rank1 <-  Xd[1, 1] * Xu[, 1] %*% t(Xv[, 1])
Y.svd.rank1 <-  Yd[1, 1] * Yu[, 1] %*% t(Yv[, 1])

X.svd.rank2 <-  Xd[2, 2] * Xu[, 2] %*% t(Xv[, 2])
Y.svd.rank2 <-  Yd[2, 2] * Yu[, 2] %*% t(Yv[, 2])

X.svd.rank2 <- X.svd.rank1 + X.svd.rank2
Y.svd.rank2 <- Y.svd.rank1 + Y.svd.rank2

library(Matrix)
rankMatrix(X)
rankMatrix(X.svd.rank1)
rankMatrix(X.svd.rank2)

# 5

Z <- cbind(c(3, 5, 5, 9), c(1, 9, 3, 7), c(4, 2, 5, 9), c(1, 6, 8, 3))
Z.svd <- svd(Z)
Zu <- Z.svd$u
Zv <- Z.svd$v
Zd <- diag(Z.svd$d)

Z.svd.rank1 <-  Zd[1, 1] * Zu[, 1] %*% t(Zv[, 1])
Z.svd.rank2 <-  Z.svd.rank1 + Zd[2, 2] * Zu[, 2] %*% t(Zv[, 2])
Z.svd.rank3 <-  Z.svd.rank2 + Zd[3, 3] * Zu[, 3] %*% t(Zv[, 3])
Z.svd.rank4 <-  Z.svd.rank3 + Zd[4, 4] * Zu[, 4] %*% t(Zv[, 4])

# 6
A <- cbind(c(2, 1, 1), c(3, 1, 1), c(-2, 2, -1))
A <- rbind(A, c(1, 1, 1))
A.svd <- svd(A)
A.svd$d
A.svd$u
A.svd$v
A.eigen <- eigen(A) # Does not work -> as intended
ginv(A)
ginv(ginv(A))

A - A%*%ginv(A)%*%A
ginv(A) - ginv(A)%*%A%*%ginv(A)
A%*%ginv(A) - A%*%ginv(A)
t(ginv(A)%*%A) - ginv(A)%*%A

library(pracma)
orth(A) %*% t(orth(A)) - 
A%*%solve(t(A)%*%A)%*%t(A)


A.gram <- gramSchmidt(A)
A.Q <- A.gram$Q
A.R <- A.gram$R

proj = function(x)(x%*%solve(t(x)%*%x)%*%t(x))

x1 <- A[,1]
x2 <- A[,2]
x3 <- A[,3]
v1 <- x1/sqrt(sum(x1^2))
v2 <- x2 - proj(v1)%*%x2
v2 <- v2/sqrt(sum(v2^2))
v3 <- x3 - proj(v1)%*%x3 - proj(v2)%*%x3
v3 <- v3/sqrt(sum(v3^2))

proj(A) %*% A

projM = function(x)(x%*%ginv(t(x)%*%x)%*%t(x))
A %*% projM(t(A))
