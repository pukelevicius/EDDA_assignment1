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

# h0 mean <= 2800
t.test(df$birthweight, mu=2800, alt="g")
# p value 0.01357 means that h0 has to be rejected in favor of h1
# which means that true mean is greater than 2800

# sign test
birtweight_results = df[,1]; birtweight_results
num_of_results_gt_than_2800 = sum(birtweight_results > 2800)
binom.test(num_of_results_gt_than_2800, length(birtweight_results))
# probability of success 0.569