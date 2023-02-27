library(dplyr)
library(multcomp)


data <- read.csv("Data/diet.txt",header = TRUE, sep = "")

# a)
# Test the assumptions
# Check the normality of the data
hist(data$weight6weeks - data$preweight, breaks=10)

# Test normality of the data
aovdata = lm(preweight~weight6weeks, data=data)

qqnorm(residuals(aovdata))
plot(fitted(aovdata),residuals(aovdata))

# Conduct a t-test to test the claim that diet affects weight loss
# p-value is small so we can reject H0 claiming that mean difference is zero.
t.test(data$preweight, data$weight6weeks, paired=TRUE)


# b)
# Apply one-way ANOVA to test whether type of diet has an effect on the lost weight.

# - If normality assumption is not met, Kruskal-wallis can be used
#data$weight6weeks = as.factor(data$weight6weeks)
model_b <- aov(preweight - weight6weeks ~ diet, data=data)
summary(model_b)

# Do all three types diets lead to weight loss?
# p-value is < 0.05 which indicates there is a significant mean difference
# From this we could say that on average there is a significant weight loss

# Which diet was the best for losing weight? 
diet_1 = filter(data, diet==1) 
diet_2 = filter(data, diet==2) 
diet_3 = filter(data, diet==3)

mean(diet_1$preweight) - mean(diet_1$weight6weeks)
mean(diet_2$preweight) - mean(diet_2$weight6weeks)
mean(diet_3$preweight) - mean(diet_3$weight6weeks)

# Create a boxplot of weight6weeks by diet to compare the weight loss between the diets
boxplot((preweight-weight6weeks) ~ diet, data = data, xlab = "Diet", ylab = "Weight loss", main = "Boxplot of weightloss by Diet")
# The best diet is diet 3 because on average the weight has decreased the most

# Can the Kruskal-Wallis test be applied for this situation?
# Kruskal-Wallis can test whether medians are different for multiple groups.
# The outcome would describe whether the effects of the three diets are
# the same or not on the weight lost.
# Kruskal can be applied but won't provide significant data to tell which
# diet was best for losing weight.

# c)
# Fit a two-way ANOVA model
lostweights = as.factor(data$preweight - data$weight6weeks)
data$gender = as.factor(data$gender)

model_c <- aov(diet ~ lostweights * gender, data = data)
summary(model_c)

# p-value of lost weights:gender is > 0.05 which concludes that lost weight and gender have interaction
# given the diet. It shows that diet has an effect on lost weight and gender.


# e)



# For the "A" diet, the predicted lost weight would be:
# Mean lost weight for diet A + (Effect of gender on lost weight for diet A)
# For the "B" diet, the predicted lost weight would be:
# Mean lost weight for diet B + (Effect of gender on lost weight for diet B)
# For the "C" diet, the predicted lost weight would be:
# Mean lost weight for diet C + (Effect of gender on lost weight for diet C)
