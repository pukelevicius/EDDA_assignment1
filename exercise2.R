library(dplyr)
library(ggplot2)

#setwd('C:/Users/Domantas/Desktop/VU AI/EDDA/EDDA_assignment1')
setwd("C:/Users/doman/OneDrive/files/VU AI/EDDA/EDDA_assignment1")

df = read.csv('Data/cholesterol.txt', header = TRUE, sep = "")

#a)  Make some relevant plots of this data set, comment on normality. Are there any inconsistencies in the data?
#   Investigate whether the columns Before and After8weeks are correlated.
########################################################################################

#qqplots imply that data samples follow normal distribution
qqnorm(df$Before)
qqline(df$Before)

qqnorm(df$After8weeks)
qqline(df$After8weeks)

#histograms follow normal dist shape more or less
par(mfrow=c(1,2)); hist(df$Before, main = 'Cholesterol before', xlab = 'mmol/L');hist(df$After8weeks, main = 'Cholesterol after 8 weeks', xlab = 'mmol/L')

#boxplots:
boxplot(df$Before,df$After8weeks, ylab = 'mmol/L')

#b)  Apply two relevant tests (cf. Lectures 2, 3) to verify whether the diet with low fat margarine has an effect
#   (argue whether the data are paired or not). Is a permutation test applicable?
########################################################################################

#paired t-test:
t.test(df$Before, df$After8weeks, paired=TRUE)
#equivalent one sample t-test:
t.test(df$Before - df$After8weeks)
#Wilcox signed rank test (we can use it since both samples are symmetric more or less)
sum(rank(abs(df$Before-df$After8weeks))[df$Before-df$After8weeks > 0])#value test statistics
wilcox.test(df$Before, df$After8weeks, paired = TRUE)


#permutation test: (not sure regarding this because p = 0.000)
mean_diff = function(x,y) {mean(x-y)}
B=1000;
tstar=numeric(B)
for (i in 1:B) {
dfstar = t(apply(cbind(df[,1],df[,2]),1,sample))
tstar[i]=mean_diff(dfstar[,1],dfstar[,2]) }
myt=mean_diff(df[,1],df[,2])

myt

hist(tstar)
pl=sum(tstar<myt)/B
pr=sum(tstar>myt)/B
p=2*min(pl,pr)
p

#
#c)  Let x be the column After8weeks. Assume x ~ unif(3,)


#create empty vector to hold sample means
sample_maxs <- c()
#take 1,000 random samples of size n=nrow(df)
n = 1000
for (i in 1:n){
  sample_maxs[i] = max(sample(df$After8weeks, nrow(df), replace=TRUE))
}

#first way to estimate CI:
estimated_upper_limit = mean(sample_maxs)
s = sd(sample_maxs)
z_95p = 1.96 # value from z score table for 97.5th percentile
m = z_95p*s/sqrt(nrow(df)) #m = 1.96s/√n

bounded_CI = c(estimated_upper_limit - m, estimated_upper_limit + m) #bounded 96% CI for mu
bounded_CI

#second way to estimate CI (i am more confident in this)
cat("ci", quantile(sample_maxs, c(0.025,0.975 )))

#d)

theta = 3.00 # theta from 3 to 12
t=max(df$After8weeks)
counter = 1
B=1000
tstar=numeric(B);
p_values = c()
thetas = c()
while (theta <= 12) {
  
  for (i in 1:B){
    xstar = runif(n=nrow(df), min=3, max=theta)
  
    tstar[i]=max(xstar)
    }
  p_values
  
  pl=sum(tstar<t)/B
  pr=sum(tstar>t)/B
  
  p_values[counter]= 2*min(pl,pr)
  thetas[counter] = theta
  
  counter = counter + 1
  theta = theta + 0.01 #increment theta by  0.01
  
}

p_values[1]

plot(x=thetas, y = p_values, type = "S")
abline(a=0.05,b=0, col='red')

#d)
