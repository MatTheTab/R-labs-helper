# Exercises:
## 1 -> Single variable polynomial regression

## 2 -> Same as Exercise 1

## 3 -> Multiple variables polynomial regression

## 4 -> Same as Exercise 3

## 5 -> Same as Exercise 3

## 6 -> Manual estimation of parameters

## 7 -> step-wise regression

#########################################################################################################################################################
# Exercise 1 -> Single variable polynomial regression

library(car)

setwd("C:/Users/mateu/OneDrive/Pulpit/R_labs_Master_Degree_Studies")
reg_time = read.csv("Reg_time_temp.csv", sep=";")
reg_time

## A) Covariance and Correlation
cov(reg_time) # Covariance smaller than inter-feature variance
cor(reg_time) # Positive correlation, the higher the operation time, the higher the tempreture

## B) Scatter plot
plot(reg_time$operation.time, reg_time$temperature, pch=0)

## C) Polynomial regressions
model1 <- lm(temperature ~ operation.time, data=reg_time)
model2 <- lm(temperature ~ operation.time + I(operation.time^2), data=reg_time)
model3 <- lm(temperature ~ poly(operation.time, 3), data=reg_time)
model4 <- lm(temperature ~ poly(operation.time, 9), data=reg_time)

## D) Plotting functions
plot(reg_time$operation.time, reg_time$temperature, pch = 0, col = "black",
     main = "Polynomial Regression Fits", xlab = "Operation Time", ylab = "Temperature")

sorted_idx <- order(reg_time$operation.time)
x_sorted <- reg_time$operation.time[sorted_idx]

lines(x_sorted, predict(model1, newdata = data.frame(operation.time = x_sorted)), col = "red", lwd = 2)
lines(x_sorted, predict(model2, newdata = data.frame(operation.time = x_sorted)), col = "blue", lwd = 2)
lines(x_sorted, predict(model3, newdata = data.frame(operation.time = x_sorted)), col = "green", lwd = 2)
lines(x_sorted, predict(model4, newdata = data.frame(operation.time = x_sorted)), col = "orange", lwd = 2)

legend("topright", legend = c("Linear", "Quadratic", "Cubic", "Tetratic"),
       col = c("red", "blue", "green", "orange"), lwd = 2)

## E)
summary(model1)
summary(model2)
summary(model3) # Intercept and Order 3 are both significant (p-value lower than alpha)
summary(model4) # Overfitted model returns NAs

Anova(model1)
Anova(model2)
Anova(model3)
# Anova(model4) -> Error, overfitted

## F)
model = model3
predict(model, newdata = data.frame(operation.time = c(15, 48)))

## G)
summary(model) # Just look at R-squared (coefficent of determination)

#########################################################################################################################################################
# Exercise 2 -> Same as Exercise 1

# Exercise 3 -> Multiple variables polynomial regression
boss_baby = read.csv("RegM_infant.csv", sep=";")
par(mfrow=c(2, 2))
boss_baby

## A)
plot(boss_baby$Age, boss_baby$Height, pch=0)
plot(boss_baby$Birth.height, boss_baby$Height, pch=0)
plot(boss_baby$Birth.weight, boss_baby$Height, pch=0)

## B)
cor(boss_baby)
cov(boss_baby)

## C)
model1 <- lm(Height ~ Age + Birth.height + Birth.weight, data=boss_baby)

## D)
summary(model1)
Anova(model1)

## E)
predict(model1, newdata = data.frame(Birth.height = c(67.5),
                                     Birth.weight = c(4),
                                     Age = c(75)))
## F)
summary(model1)

## G)
model1 <- lm(Height ~ Age + Birth.weight, data=boss_baby)
summary(model1)

#########################################################################################################################################################
# Exercise 4 -> Same as Exercise 3
# Exercise 5 -> Same as Exercise 3

#########################################################################################################################################################
# Exercise 6 -> Estimate stuff manually

#A)
setwd("C:/Users/mateu/OneDrive/Pulpit/R_labs_Master_Degree_Studies")
chemical = read.csv("RegMM_chemical.csv", sep=";")
chemical

# Response matrix (n x p)
Y <- as.matrix(chemical[, c("y1", "y2", "y3")])

# Design matrix with intercept and predictors (n x m)
X <- as.matrix(cbind(1, chemical[, c("x1", "x2", "x3")]))

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
full = lm(y1 ~ x1 + x2 + x3, data=chemical)
linearHypothesis(full, hypothesis.matrix = c("x1=0", "x2=0", "x3=0"))

# D)
x12 = chemical$x1^2
x22 = chemical$x2^2
x32 = chemical$x3^2
x1x2 = chemical$x1*chemical$x2
x1x3 = chemical$x1*chemical$x3
x2x3 = chemical$x2*chemical$x3
y1 = chemical$y1

model = lm(y1 ~ x12 + x22 + x32 + x1x2 + x1x3 + x2x3)
linearHypothesis(model, hypothesis.matrix = c("x12=0", "x22=0", "x32=0", "x1x2=0", "x1x3=0"))
Anova(model)

#########################################################################################################################################################
# Exercise 7 -> step-wise regression
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

