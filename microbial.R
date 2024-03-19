# microbial project


# load libraries
library(readr)
library(tidyverse)
library(ggplot2)
library(psych)
library(car)
library(multcomp)
library(grid)
library(gridExtra)

microbial_plant <- read.csv("/Users/christopherwagner/Downloads/doi_10-7/microbial_plant.csv")
# clean up the data: remove irrelevant columns for this analysis
cleaned_microbial_plant <- microbial_plant[,-c(1,6,7,8,9,10,11,13)]
# Transform data
log_bnc <- log(cleaned_microbial_plant$BNC.ug.g.)
transformed_microbial_plant <- cleaned_microbial_plant
transformed_microbial_plant$BNC.ug.g. <- log_bnc

# Visualize
par(mfrow=c(1,2))
hist(transformed_microbial_plant$BNC.ug.g., main="Distribution of Log BNC", xlab="Log BNC (μg/g)")
qqPlot(transformed_microbial_plant$BNC.ug.g., main = "Quantile-Quantile Plot of Log BNC", xlab="Normal Quantities", ylab="Log BNC (μg/g)")
par(mfrow=c(1,2))
plot(transformed_microbial_plant$pH, transformed_microbial_plant$BNC.ug.g., main = "pH vs Log BNC", xlab="pH",
     ylab = "Log BNC (ug)")
boxplot(transformed_microbial_plant$BNC.ug.g. ~ transformed_microbial_plant$Soil.layer..cm., main="Boxplot of BNC Across Soil Layers", xlab="Soil Layer", ylab="Log BNC (μg/g)")

# ANOVA
anova_result <- aov(BNC.ug.g. ~ Soil.layer..cm., data = transformed_microbial_plant)
summary(anova_result)
# Tukey test
TukeyHSD(anova_result, conf.level=.95)


# Linear model
par(mfrow=c(2,2))
bnc_pca <- prcomp(cleaned_microbial_plant[,-c(1,2,6)], center=TRUE, scale=TRUE)
transformed_predictors <- bnc_pca$x[,-3]
new_lm <- lm(transformed_microbial_plant$BNC.ug.g. ~ transformed_predictors+
               transformed_microbial_plant$Soil.layer..cm.+transformed_microbial_plant$Treatment) 
plot(new_lm)

# Results
par(mfrow=c(1,3))
# pH
plot(transformed_microbial_plant$pH, transformed_microbial_plant$BNC.ug.g., main = "pH vs Log BNC", xlab="pH",
     ylab = "Log BNC (ug)")
abline(lm(transformed_microbial_plant$BNC.ug.g.~transformed_microbial_plant$pH), col="red")
# SM
plot(transformed_microbial_plant$SM...., transformed_microbial_plant$BNC.ug.g., main = "Soil Moisture(SM) vs Log BNC", xlab="SM",
     ylab = "Log BNC (ug)")
abline(lm(transformed_microbial_plant$BNC.ug.g.~transformed_microbial_plant$SM....), col="red")
# STN
plot(microbial_plant$STN..g.kg.1., transformed_microbial_plant$BNC.ug.g., main = "Soil Total Nitrogen (STN) vs Log BNC", xlab="pH",
     ylab = "Log BNC (ug)")
abline(lm(transformed_microbial_plant$BNC.ug.g.~microbial_plant$STN..g.kg.1.), col="red")

predicted <- predict(new_lm)
plot(transformed_microbial_plant$BNC.ug.g., predicted, main = "Observed vs. Predicted BNC", xlab = "Observed", ylab = "Predicted")
abline(a = 0, b = 1, col = "red")