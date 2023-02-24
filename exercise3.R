library(dplyr)
library(multcomp)


data <- read.csv("Data/diet.txt",header = TRUE, sep = "")
data$weight.lost = data$preweight - data$weight6weeks 

# a)
# Scatterplot to visualize the relationship between Preweight and Weight6weeks
plot(data$preweight, data$weight6weeks, xlab="Preweight", ylab="Weight6weeks", main="Scatterplot of Preweight vs. Weight6weeks")

# Create a boxplot of weight6weeks by diet to compare the weight loss between the two diets
boxplot(weight6weeks ~ diet, data = data, xlab = "Diet", ylab = "Weight6weeks", main = "Boxplot of Weight6weeks by Diet")
boxplot(preweight - weight6weeks ~ diet, data = data, main = 'Boxplots of Weight Lost by Diet') #added boxplots for difference - Dom
# Test the assumptions
# Check the normality of the data
hist(data$weight6weeks - data$preweight, breaks=10)

# Test normality of the data
qqnorm(data$preweight - data$weight6weeks)
qqline(data$preweight - data$weight6weeks)

# Create a scatter plot of the differences against the pre-weight measurements
plot(data$preweight, data$weight6weeks - data$preweight, 
     xlab = "Pre-weight", ylab = "Weight loss", main = "Scatter plot of weight loss vs. pre-weight")


# Conduct a t-test to test the claim that diet affects weight loss
# p-value is small so we can reject H0 claiming that mean difference is zero.
t.test(data$preweight, data$weight6weeks, paired=TRUE)


# b)




model <- aov((data$weight6weeks-data$preweight) ~ data$diet, data=data)
model
summary(model)

# - p-value is < 0.05 which indicates there is a significant mean difference
# - If normality assumption is not met, Kruskal-wallis can be used



# c)
anova(lm(weight.lost ~ diet * gender, data = data))

# Fit a two-way ANOVA model
model <- aov(weight6weeks - preweight ~ diet * gender, data = data); model

# Print the ANOVA table
summary(model)

# p-value is > 0.05 which concludes that there is not enough evidence to reject 
# H0 which says that there is no interaction between diet and gender



# d)

model <- aov(weight6weeks-preweight ~ diet * height, data = data)
aov
summary(model)
# p-value is > 0.05 and suggests that the effect of height is different across the three types of diets



# e)



