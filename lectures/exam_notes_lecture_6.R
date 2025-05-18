#Lecture 6 -> Notes

setwd("C:/Users/mateu/OneDrive/Pulpit/R_labs_Master_Degree_Studies")
data = read.csv("calcium_data.csv", sep=";")
data

# Data imputation
library(mice)
data = complete(mice(data, method = "norm.predict"))
         
# Linear model
X1=data[1:2,]
X2=data[3:10,]
line1=lm(X2[,2] ~ X2[,1]+X2[,3])
b10=line1$coefficients[1]
b11=line1$coefficients[2]
b13=line1$coefficients[3]

y12=b10+b11*X1[1,1]+b13*X1[1,3]
y12

# Multivariate linear model -> predicting y3 on y1, y2
lin = lm(y3 ~ y1 + y2, data=data)
sum((predict(lin, data) - data$y3)^2)/length(data) # Mean-squared error

# normality tests
library(nortest)
pearson.test(data$y1)
lillie.test(data$y1)
shapiro.test(data$y1)
ad.test(data$y1)
sf.test(data$y1)

check_normality <- function(column, test = "shapiro") {
  test <- tolower(test)
  result <- switch(test,
                   "shapiro" = shapiro.test(column),
                   "pearson" = pearson.test(column),
                   "lillie"  = lillie.test(column),
                   "ad"      = ad.test(column),
                   "sf"      = sf.test(column),
                   stop("Unknown test. Use: 'shapiro', 'pearson', 'lillie', 'ad', or 'sf'")
  )
  
  cat("Test used:", test, "\n")
  cat("P-value:", result$p.value, "\n")
  if (result$p.value > 0.05) {
    cat("✅ The distribution appears to be normal (p > 0.05)\n")
  } else {
    cat("❌ The distribution does NOT appear to be normal (p ≤ 0.05)\n")
  }
}

check_normality(data$y1)


# qq plots
library(car)
qqnorm(data$y1)
qqline(data$y1)
qqPlot(data$y1)

# multivariate normal tests
library(MVN)
mvn(data, mvnTest = "mardia")$multivariateNormality
