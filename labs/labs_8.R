# Exercise 1

library(car)

setwd("C:/Users/mateu/OneDrive/Pulpit/R_labs_Master_Degree_Studies")
data = read.csv("Reg_time_temp.csv", sep=";")
data

cov(data)
cor(data)

plot(data$operation.time, data$temperature, pch=0)

model1 <- lm(data$temperature ~ operation.time, data=data)
model2 <- lm(data$temperature ~ poly(operation.time, 2), data=data)
model3 <- lm(data$temperature ~ poly(operation.time, 3), data=data)
model4 <- lm(data$temperature ~ poly(operation.time, 4), data=data)

plot(data$operation.time, data$temperature, pch=0)

sorted_idx <- order(data$operation.time)
x_sorted <- data$operation.time[sorted_idx]

# Plotting predictions
plot(data$operation.time, data$temperature, pch = 0, col = "black",
     main = "Polynomial Regression Fits", xlab = "Operation Time", ylab = "Temperature")

lines(x_sorted, predict(model1, newdata = data.frame(operation.time = x_sorted)), col = "red", lwd = 2)
lines(x_sorted, predict(model2, newdata = data.frame(operation.time = x_sorted)), col = "blue", lwd = 2)
lines(x_sorted, predict(model3, newdata = data.frame(operation.time = x_sorted)), col = "green", lwd = 2)
lines(x_sorted, predict(model4, newdata = data.frame(operation.time = x_sorted)), col = "orange", lwd = 2)

legend("topright", legend = c("Linear", "Quadratic", "Cubic", "Tetratic"),
       col = c("red", "blue", "green", "orange"), lwd = 2)

summary(model1)
summary(model2)
summary(model3)
summary(model4)

# smooth plots
# Base scatter plot
plot(data$operation.time, data$temperature, pch = 0, col = "black",
     main = "Polynomial Regression Fits", xlab = "Operation Time", ylab = "Temperature")

# Generate smooth sequence of x values
x_seq <- seq(min(data$operation.time), max(data$operation.time), length.out = 1000)

# Add regression lines for each model
lines(x_seq, predict(model1, newdata = data.frame(data.operation.time = x_seq)), col = "red", lwd = 2)
lines(x_seq, predict(model2, newdata = data.frame(operation.time = x_seq)), col = "blue", lwd = 2)
lines(x_seq, predict(model3, newdata = data.frame(operation.time = x_seq)), col = "green", lwd = 2)
lines(x_seq, predict(model4, newdata = data.frame(operation.time = x_seq)), col = "orange", lwd = 2)

# Add legend
legend("topright", legend = c("Linear", "Quadratic", "Cubic", "Tetratic"),
       col = c("red", "blue", "green", "orange"), lwd = 2)

Anova(model2)
Anova(model3) # Good fit and all coefficients are significant -> best model
Anova(model4) # Good fit and not all coefficients are significant -> not best model

model_fancy <- lm(data$temperature ~ sin(I(operation.time)) + cos(I(operation.time)) + I(operation.time), data=data)
predict(model3, newdata = data.frame(operation.time = c(15, 48)))
summary(model3)



# Exercise 6

#A)
setwd("C:/Users/mateu/OneDrive/Pulpit/R_labs_Master_Degree_Studies")
data = read.csv("RegMM_chemical.csv", sep=";")
data

# Response matrix (n x p)
Y <- as.matrix(data[, c("y1", "y2", "y3")])

# Design matrix with intercept and predictors (n x m)
X <- as.matrix(cbind(1, data[, c("x1", "x2", "x3")]))

B_hat <- solve(t(X) %*% X) %*% t(X) %*% Y

colnames(B_hat) <- c("y1", "y2", "y3")
rownames(B_hat) <- c("Intercept", "Temperature", "Concentration", "Time")
print(B_hat)

#B)

# Residuals
E <- Y - X %*% B_hat

# Dimensions
n <- nrow(X)
m <- ncol(X)

# Unbiased estimate of Sigma
Sigma_hat <- t(E) %*% E / (n - m - 1)
print(Sigma_hat)

# C)
library(car)
full = lm(y1 ~ x1 + x2 + x3, data=data)
linearHypothesis(full, hypothesis.matrix = c("x1=0", "x2=0", "x3=0"))

# D)
x12 = data$x1^2
x22 = data$x2^2
x32 = data$x3^2
x1x2 = data$x1*data$x2
x1x3 = data$x1*data$x3
x2x3 = data$x2*data$x3
y1 = data$y1

model = lm(y1 ~ x12 + x22 + x32 + x1x2 + x1x3 + x2x3)
linearHypothesis(model, hypothesis.matrix = c("x12=0", "x22=0", "x32=0", "x1x2=0", "x1x3=0"))
Anova(model)


# Exercise 7
setwd("C:/Users/mateu/OneDrive/Pulpit/R_labs_Master_Degree_Studies")
data = read.csv("RegMM_temp.csv", sep=";")
data

library(MASS)
library(car)
library(leaps)
data
X = data[, 1:9]
y = data[, 9:11]
y1 = Y[, 1]
y2 = Y[, 2]
y3 = Y[, 3]

full1 = lm(y1 ~ ., data=X)
full2 = lm(y2 ~ ., data=X)
full3 = lm(y3 ~ ., data=X)

step.model = stepAIC(full1, direction="both", trace=FALSE)
summary(step.model)

step.model = stepAIC(full2, direction="both", trace=FALSE)
summary(step.model)

step.model = stepAIC(full3, direction="both", trace=FALSE)
summary(step.model)
