setwd("C:/Users/mateu/OneDrive/Pulpit/R_labs_Master_Degree_Studies")
psycho = read.csv("psycho.csv", sep=";")
psycho

male_data = psycho[, 1:4]
female_data = psycho[, 5:8]
p = ncol(psycho)

n_male = nrow(male_data)
n_female = nrow(female_data)
S_male = (n_male-1)/n_male*cov(male_data)
S_female = (n_female-1)/n_female*cov(female_data)
S = 1/(n_male + n_female - 2)*(n_male*S_male + n_female*S_female)
Y1bar = as.matrix(colMeans(male_data))
Y2bar = as.matrix(colMeans(female_data))
df = n_male+n_female-2
Tsq = (n_male*n_female)/(n_male+n_female)*t(Y1bar - Y2bar)%*%solve(S)%*%(Y1bar - Y2bar)
alpha = 0.05
p*df/(df-p+1)*qf(1-alpha,p,df-p+1)

#Data confirms that the mean vectors of psychological features differ significantly between men and women

hotel = hotelling.test(male_data, female_data, var.equal = T)
hotel$stats
hotel$pval

#alpha=0.05
#p-val = 0
#alpha > p-val, so reject H0 -> data confirms

# Because we run the test 4 times, the 5% uncertainty (error), it sums up to 20%, so the actual error is greater
# Look -> post-hog analysis (Beefaroni Correction)
t.test(male_data$M1, female_data$F1, var.equal = T)$p.value #p.val < alpha; they differ
t.test(male_data$M2, female_data$F2, var.equal = T)$p.value #p.val < alpha; they differ
t.test(male_data$M3, female_data$F3, var.equal = T)$p.value #p.val < alpha; they differ
t.test(male_data$M4, female_data$F4, var.equal = T)$p.value #p.val > alpha; they do not differ
