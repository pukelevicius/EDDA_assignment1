library(dplyr)
library(multcomp)


data <- read.csv("Data/diet.txt",header = TRUE, sep = "")

# a)
# Test the assumptions
# Check the normality of the data
hist(data$preweight - data$weight6weeks, breaks=10)

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
model_b <- lm(preweight - weight6weeks ~ diet, data=data)
anova(model_b)
# summary(model_b)

# Do all three types diets lead to weight loss?
# p-value is < 0.05 which indicates there is a significant mean difference
# From this we could say that on average there is a significant weight loss

# Which diet was the best for losing weight? 
# Create a boxplot of weight6weeks by diet to compare the weight loss between the diets
boxplot((preweight-weight6weeks) ~ diet, data = data, xlab = "Diet", ylab = "Weight loss", main = "Boxplot of weightloss by Diet")
# The best diet is diet 3 because on average the weight has decreased the most

# c) Fit a two-way ANOVA model
data$gender = as.factor(data$gender)
data$diet =  as.factor(data$diet)

model_c <- lm(preweight - weight6weeks ~ diet * gender, data = data)
anova(model_c)

# Interaction of gender and diet gives a p-value < 0.05 which 
# shows that diet and gender have an effect on the lost weight.

# e) two-way anova is preferred
diet_1 = filter(data, diet==1) 
diet_2 = filter(data, diet==2) 
diet_3 = filter(data, diet==3)

avg_loss_diet_1 = mean(diet_1$preweight) - mean(diet_1$weight6weeks)
avg_loss_diet_2 = mean(diet_2$preweight) - mean(diet_2$weight6weeks)
avg_loss_diet_3 = mean(diet_3$preweight) - mean(diet_3$weight6weeks)

diet_1_predicted = mean(diet_1$preweight) - avg_loss_diet_1; diet_1_predicted
diet_2_predicted = mean(diet_2$preweight) - avg_loss_diet_2; diet_2_predicted
diet_3_predicted = mean(diet_3$preweight) - avg_loss_diet_3; diet_3_predicted

