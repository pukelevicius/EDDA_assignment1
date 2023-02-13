library(dplyr)

df <- read.csv('C:/Users/Domantas/Desktop/VU AI/EDDA/assignment 1/birthweight.txt') # reading data
print(weights)
#normality check with Shapiro-wilk test and qqpplot
shapiro.test(df$birthweight)
qqnorm(df$birthweight)


#bounded 96%-CI for mean of given birthweigts sample
#EXAMPLE (continued) An (asymptotic) 95%-confidence interval for µ is the interval [X¯ − m, X¯ + m], where m = 1.96s/√n
n = nrow(df)
mu = mean(df$birthweight)
variance = var(df$birthweight)
s = sd(df$birthweight)
z_98p = 2.05 # value from z score table for 98th percentile
m = z_98p*s/sqrt(n) #m = 1.96s/√n

bounded_CI = c(mu - m, mu + m) #bounded 96% CI for mu
