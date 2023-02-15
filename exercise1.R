library(dplyr)
df <- read.csv('Data/birthweight.txt') # reading data
#normality check with Shapiro-wilk test and qqpplot
shapiro.test(df$birthweight)
qqnorm(df$birthweight)


#bounded 96%-CI for mean of given birthweights sample
#EXAMPLE (continued) An (asymptotic) 95%-confidence interval for µ is the interval [X¯ − m, X¯ + m], where m = 1.96s/√n
n = nrow(df)
mu = mean(df$birthweight)
variance = var(df$birthweight)
s = sd(df$birthweight)
z_98p = 2.05 # value from z score table for 98th percentile
m = z_98p*s/sqrt(n) #m = 1.96s/√n

bounded_CI = c(mu - m, mu + m) #bounded 96% CI for mu
bounded_CI

# calculating sample size for 100 length CI

get_m <- function(n) {
  s = sd(df$birthweight)
  z_98p = 2.05 # value from z score table for 98th percentile
  m = z_98p*s/sqrt(n)
  
  return(m)
}


for (sample_size in 1:1000) {
  lower_bound = mu - get_m(sample_size)
  upper_bound = mu + get_m(sample_size)
  CI_length <- upper_bound - lower_bound
  if (CI_length <= 100) {
    break
  }
}
sample_size
CI_length

# bootstrap 96%-CI:
B = 1000
Tstar = 1:B
for (i in 1:B){
  Xstar = sample(df$birthweight, replace=TRUE)
  Tstar[i] = mean(Xstar)
}
Tstar20 = quantile(Tstar, 0.020)
Tstar980 = quantile(Tstar, 0.980)
sum(Tstar<Tstar20)

bootstrap_CI = c(2*mu-Tstar980,2*mu-Tstar20)
bootstrap_CI

# H0 mean <= 2800
t.test(df$birthweight, mu=2800, alt="g")
# p value 0.01357 means that H0 has to be rejected in favor of h1
# which means that true mean is greater than 2800

# sign test
birtweight_results = df[,1]; birtweight_results
binom.test(sum(birtweight_results > 2800), length(birtweight_results), alt='l')[3]
# p value = 0.97567

# power of t-test and sign test
B = 1000
psign = numeric(B)
pttest = numeric(B)
n = 50
for(i in 1:B) {
  x = sample(df$birthweight, n)
  psign[i] = binom.test(sum(x>2800), n, alt='g')[[3]]
  pttest[i] = t.test(x, mu=2800, alt='g')[[3]]
}
power_sign = sum(psign<0.05)/B; power_sign
power_ttest = sum(pttest<0.05)/B; power_ttest
# t-test power (probability of rejecting H0) is bigger, because t-test works better for normal data
hist(sample(df$birthweight, 100))
# histogram shows that data is distributed due to normal distribution

# this is unfinsihed and I am not sure about the results bc z_alpha is very very strange
# let's get 100 samples from df$birthweight
n = 100
sample_probabilities = numeric(n)
for(i in 1:n){
  x = sample(df$birthweight, n)
  sample_probabilities[i] = sum(x < 2600)/n
}
s = sd(sample_probabilities)
p_estimate = mean(sample_probabilities)
# Using asymptotic normality, the expert computed the left end  =0.25
# of the confidence interval for p
# so we know that p_l = p_estimate - m = 0.25
# we also know that m = z_alpha * s/sqrt(n) and m = p_estimate - 0.25
m = p_estimate - 0.25
z_alpha = m/(s/sqrt(n)); z_alpha
p_r = p_estimate + m; p_r
s/sqrt(n)
