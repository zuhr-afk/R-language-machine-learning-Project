library(readr)
waterdata=read.csv("https://raw.githubusercontent.com/DanikaLipman/DATA603/main/water.csv",header = TRUE )
library(car)

model=lm(USAGE~PROD+TEMP+HOUR+DAYS, data=waterdata)
intermodel=lm(USAGE~(PROD+TEMP+HOUR)^2, data=waterdata)
finalintermodel=lm(USAGE~PROD+TEMP+HOUR+PROD*TEMP+PROD*HOUR, data=waterdata)
summary(finalintermodel)

# Question 1
# a Calculate VIF for each predictor
vif_values <- vif(finalintermodel)

# Check for high correlation (VIF > 10)
high_vif <- vif_values[vif_values > 10]

# Print predictors with high VIF
if (length(high_vif) > 0) {
  cat("High VIF values for predictors:", names(high_vif), "\n")
  cat("There is a problem with multicollinearity assumption.\n")
} else {
  cat("No high VIF values detected.\n")
  cat("Multicollinearity assumption is not violated.\n")
}
#b.# Install and load the necessary library if not already installed
if (!requireNamespace("lmtest", quietly = TRUE)) {
  install.packages("lmtest")
}

# Load the lmtest library
library(lmtest)

# Fit the final model
finalintermodel <- lm(USAGE ~ PROD + TEMP + HOUR + PROD * TEMP + PROD * HOUR, data = waterdata)

# Breusch-Pagan test for heteroscedasticity
bp_test <- bptest(finalintermodel)

# Print the test results
print(bp_test)

# Residual plot
plot(fitted(finalintermodel), residuals(finalintermodel),
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residual Plot")
abline(h = 0, col = "red", lty = 2)
#  In hypothesis testing, a higher p-value indicates weaker evidence against the null hypothesis. In this case, the null hypothesis for the Breusch-Pagan test is that there is homoscedasticity (constant variance).Since the p-value is high (0.8484), there is not enough evidence to reject the null hypothesis. Therefore, based on this test, there is no significant indication of heteroscedasticity (non-constant variance). This suggests that the homoscedasticity assumption may hold for the model.

# c. # Histogram for residuals
hist(residuals(finalintermodel), main = "Histogram of Residuals", xlab = "Residuals")

# Normal Q-Q plot
qqnorm(residuals(finalintermodel))
qqline(residuals(finalintermodel), col = 2)

# Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(residuals(finalintermodel))
print(shapiro_test)
## The Shapiro-Wilk test for normality has a very low p-value (< 2.2e-16), indicating that there is strong evidence to reject the null hypothesis. The null hypothesis of the Shapiro-Wilk test is that the data follows a normal distribution. Since the p-value is extremely small, we reject the null hypothesis, suggesting that the residuals are not normally distributed.

#d.# Residuals vs Predicted values plot
plot(finalintermodel$fitted.values, residuals(finalintermodel),
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted values")
abline(h = 0, col = "red", lty = 2)
# the residuals appear to be increasing as the fitted values increase. This suggests that the relationship between the predictor variables and the response variable is not linear.

#e. # Cook's distance
cook_distance <- cooks.distance(finalintermodel)

# Identify outliers using Cook's distance
outliers <- which(cook_distance > 1)

# Residual vs Leverage plot
plot(hatvalues(finalintermodel), residuals(finalintermodel),
     xlab = "Leverage", ylab = "Residuals",
     main = "Residuals vs Leverage")
abline(h = 0, col = "red", lty = 2)

# Highlight outliers on the plot
points(hatvalues(finalintermodel)[outliers], residuals(finalintermodel)[outliers],
       col = "red", pch = 19)
# f. the model appears to meet this assumption well and no specific improvements or adjustments are immediately necessary in this regard.

#Question two
KBI=read.csv("https://raw.githubusercontent.com/DanikaLipman/DATA603/main/KBI.csv", header = TRUE)
model=lm(BURDEN~(CGDUR+ MEM +SOCIALSU) , data=KBI)
#summary(model) 
interactionmodel=lm(BURDEN~(CGDUR+ MEM +SOCIALSU)^2 , data=KBI)
#summary(interactionmodel) 
# a. Fit the model
model <- lm(BURDEN ~ CGDUR + MEM + SOCIALSU, data = KBI)

# Residuals vs Fitted values plot for linearity
plot(model$fitted.values, residuals(model),
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted values")
abline(h = 0, col = "red", lty = 2)

# Homoscedasticity (optional): Residuals vs Fitted values plot
plot(model$fitted.values, sqrt(abs(residuals(model))),
     xlab = "Fitted values", ylab = "sqrt(|Residuals|)",
     main = "Scale-Location Plot")
abline(h = 0, col = "red", lty = 2)

# Normality: Q-Q plot
qqnorm(residuals(model))
qqline(residuals(model), col = 2)

# Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(residuals(model))
print(shapiro_test)

#b. # Leverage values
leverage_values <- hatvalues(model)

# Identify outliers using leverage values (e.g., leverage > 2 * (p + 1) / n)
outliers <- which(leverage_values > 2 * (length(coef(model)) + 1) / length(model$residuals))

# Print the outliers
if (length(outliers) > 0) {
  cat("Outliers detected. Observations:", outliers, "\n")
  
  # Create a new dataset without outliers
  new_KBI <- KBI[-outliers, ]
  cat("New dataset created without outliers.\n")
} else {
  cat("No outliers detected.\n")
  # Use the original dataset
  new_KBI <- KBI
}

# Check the dimensions of the new dataset
cat("Original dataset dimensions:", dim(KBI), "\n")
cat("New dataset dimensions:", dim(new_KBI), "\n")

#c. # Fit the model with the new dataset without outliers
new_model <- lm(BURDEN ~ MEM + SOCIALSU + CGDUR, data = new_KBI)

# Compare results
summary(model)
summary(new_model)
##In comparing two linear regression models fitted on different datasets, key differences arise in the coefficients and statistical significance of the variable CGDUR. In the original model, the coefficient is marginally significant, while in the model fitted on a new dataset without outliers, it becomes statistically significant. Adjusted R-squared values indicate similar overall explanatory power and the residual standard errors are comparable but slightly different. Notably, the removal of outliers in the new dataset affects the degrees of freedom for residuals

#Question three
# Load the data
butterfat <- read.csv("https://raw.githubusercontent.com/DanikaLipman/DATA603/main/butterfat.csv", header = TRUE)
View(butterfat)
# a. Box-plots
# Box-plot against breed
boxplot(Butterfat ~ Breed, data = butterfat, main = "Butterfat Content by Breed", xlab = "Breed", ylab = "Butterfat Content")

# Box-plot against age
boxplot(Butterfat ~ Age, data = butterfat, main = "Butterfat Content by Age", xlab = "Age", ylab = "Butterfat Content")
library(dplyr)
# Compare the variability: Interquartile range (IQR) among breed's levels
butterfat %>%
  group_by(Breed) %>%
  summarise(IQR = IQR(Butterfat))

# Compare the variability: Interquartile range (IQR) among age's levels
butterfat %>%
  group_by(Age) %>%
  summarise(IQR = IQR(Butterfat))
# b. Linear model
model <- lm(Butterfat ~ Age + Breed, data = butterfat)
summary(model)
##  The coefficient for the Age (Mature) variable is not statistically significant (p-value = 0.20937), suggesting that it does not have a significant impact on Butterfat levels. Considering this, we may choose to exclude the Age variable from the model.

# c. Diagnostics
# Residuals vs Fitted plot
plot(model, which = 1)

# Normal Q-Q plot
qqnorm(residuals(model))
qqline(residuals(model))

# Test for constant variance
ncvTest(model)

# Shapiro-Wilk test for normality
shapiro.test(residuals(model))
# the linear regression model have issues with both constant variance and normality assumptions, indicating the need for caution in interpreting results.

#d. # Check for missing values
any(is.na(butterfat$Butterfat) | is.na(butterfat$Age) | is.na(butterfat$Breed))

# Remove any rows with missing values
butterfat <- na.omit(butterfat)

# Fit a linear regression model with Box-Cox transformation
lambda <- boxcox(lm(Butterfat ~ Age + Breed, data = butterfat))
optimal_lambda <- lambda$x[which.max(lambda$y)]

# Transform the response variable using the optimal lambda
transformed_response <- ifelse(optimal_lambda == 0, log(butterfat$Butterfat), ((butterfat$Butterfat^optimal_lambda) - 1) / optimal_lambda)

# Create a new dataset with transformed response
transformed_data <- cbind(transformed_response, butterfat[, c("Age", "Breed")])

# Fit the linear regression model with the transformed response
transformed_model <- lm(transformed_response ~ Age + Breed, data = transformed_data)

# Display the summary of the transformed model
summary(transformed_model)
# The Box-Cox transformed linear regression model was fitted to the butterfat content data, revealing substantial improvements in model performance compared to the original model. The transformed model exhibited nearly zero residuals indicating an excellent fit. Coefficients for age and breed variables were effectively zero suggesting that the Box-Cox transformation effectively addressed issues related to constant variance.
#e # Diagnostic analysis for the transformed model
# Assuming 'transformed_model' is the fitted linear regression model

# Residuals vs Fitted Values Plot
plot(transformed_model, which = 1)

# Scale-Location (Square Root of Standardized Residuals) vs Fitted Values Plot
plot(transformed_model, which = 3)

# Normal Q-Q Plot
qqnorm(residuals(transformed_model))
qqline(residuals(transformed_model), col = 2)

# Check for constant variance using NCV Test
ncvTest(transformed_model)

# Shapiro-Wilk Test for Normality
shapiro.test(residuals(transformed_model))
## there are indications of potential non-constant variance and non-normality in the residuals.

#Question four
#a. In the given study on motor vibration, the response variable is the "amount of motor vibration," measured in microns. Each motor's vibration level is recorded, making it the response variable of interest.

#The experimental unit is each individual motor that is part of the study. In this case, the study involves 30 motors and each motor is considered a separate experimental unit. The variation in the response variable (motor vibration) is observed across different motors and conclusions are drawn based on these individual units.

# b. The treatment in this experiment is the "brand of bearing." There are five treatment levels corresponding to the five different motor bearing brands.

# c. Hypothesis Testing:

#Null Hypothesis (H0): The average amount of motor vibrations is the same across all five bearing brands.
#Alternative Hypothesis (H1): There is a significant difference in the average amount of motor vibrations among the bearing brands.
#Test: ANOVA (Analysis of Variance) test to compare means across multiple groups.
#If the p-value is less than 0.05, we reject the null hypothesis suggesting that there is a significant difference in the average motor vibrations among the bearing brands. If it is greater than or equal to 0.05, we fail to reject the null hypothesis.

#d. # Assuming the 'vibration' dataset is loaded
# Fit an ANOVA model
vibration=read.csv("https://raw.githubusercontent.com/DanikaLipman/DATA603/main/vibration.csv", header = TRUE)

model <- aov(vibration ~ brand, data = vibration)

# Display the ANOVA table
summary(model)

#e. # Assuming the 'vibration' dataset is loaded
library(ggplot2)

# Boxplots
ggplot(vibration, aes(x = brand, y = vibration)) +
  geom_boxplot() +
  labs(title = "Motor Vibration by Bearing Brand", x = "Bearing Brand", y = "Vibration (microns)")
# yes, some influential outliers were detected

#f. # Assuming the 'vibration' dataset is loaded
#install.packages("multcomp")
#install.packages("TukeyC")
#install.packages("lsmeans")

library(multcomp)
library(TukeyC)
library(lsmeans)
# Pairwise t-tests (unadjusted p-values)
pairwise_tukey <- pairwise.t.test(vibration$vibration, vibration$brand, p.adj = "none")

# Pairwise t-tests (adjusted p-values)
pairwise_tukey_adjusted <- pairwise.t.test(vibration$vibration, vibration$brand, p.adj = "bonferroni")

# Tukey HSD test
tukey_hsd <- TukeyHSD(aov(vibration ~ brand, data = vibration))

# Newman-Keuls test
install.packages("agricolae")
library(agricolae)
crd <- (aov(vibration ~ brand, data = vibration))
summary(crd)
newman_keuls<-print(SNK.test(crd, "brand", group = TRUE))

# Scheffe test
scheffe<-scheffe.test(crd, "brand", group = TRUE, console = TRUE)

# Display the results
print("Pairwise t-tests (Unadjusted P-values):")
print(pairwise_tukey$p.value)

print("Pairwise t-tests (Adjusted P-values):")
print(pairwise_tukey_adjusted$p.value)

print("Tukey HSD Test:")
print((tukey_hsd))

print("Newman-Keuls Test:")
print((newman_keuls))

print("Scheffe Test:")
print((scheffe))
## According to the Tukey HSD test, brand2 has a significant difference compared to brand1, and brand3 has a significant difference compared to brand2.
#Newman-Keuls test confirms these differences and identifies additional significant differences.
#Scheffe test shows significant differences between brand2 and brand1, and brand4 and brand2.

# Visualizations
# Boxplot for Tukey HSD Test
boxplot(vibration$vibration ~ vibration$brand, main = "Tukey HSD Test")

# Bar plot for Newman-Keuls Test with distinct colors for groups
barplot(
  newman_keuls$statistics$Mean,
  names.arg = rownames(newman_keuls$statistics),
  col = c("red", "blue", "green", "purple", "orange"),  # Assign distinct colors for each group
  main = "Newman-Keuls Test"
)

# Bar plot for Scheffe Test with distinct colors for groups
barplot(
  scheffe$statistics$Mean,
  names.arg = rownames(scheffe$statistics),
  fill = c("red", "blue", "green", "purple", "orange"),  # Assign distinct colors for each group
  main = "Scheffe Test"
)
#g. # Histogram of residuals
hist(residuals(crd), main = "Histogram of Residuals")

# Q-Q plot of residuals
qqnorm(residuals(crd))
qqline(residuals(crd))

# Shapiro-Wilk test for normality
shapiro.test(residuals(crd))

# Plot of residuals vs fitted values
plot(fitted(crd), residuals(crd), main = "Residuals vs Fitted Values", xlab = "Fitted Values", ylab = "Residuals")

#Bartlett's test for homogeneity of variances
bptest(crd)

# Individual histograms for each group
par(mfrow=c(2,3))  # Adjust the layout
for (i in unique(vibration$brand)) {
  hist(subset(vibration, brand == i)$vibration, main = paste("Histogram of Brand", i))
}
par(mfrow=c(1,1))  # Reset the layout

# The p-value of 0.3344 indicates that there is no significant evidence to reject the null hypothesis.
#With a p-value of 0.3091, there is no significant evidence to reject the null hypothesis of normality.
#The basic assumptions of homoscedasticity and normality for the residuals in the CRD model appear to be satisfied based on the conducted tests

