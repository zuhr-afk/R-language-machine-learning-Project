# Install and load the required packages
if (!requireNamespace("resampledata", quietly = TRUE)) {
  install.packages("resampledata")
}
library(resampledata)

# Load the NCBirths2004 dataset
data(NCBirths2004)

# Extract relevant variables
weight <- NCBirths2004$Weight
smoker <- NCBirths2004$Smoker

# Set the seed for reproducibility
set.seed(123)

# Define the number of bootstrap samples
n_bootstraps <- 10000

# (a) Create the bootstrap distribution for XNonSmoker - XSmoker
bootstrap_diff <- replicate(n_bootstraps, {
  sample_non_smoker <- weight[smoker == "No"]
  sample_smoker <- weight[smoker == "Yes"]
  mean_non_smoker <- mean(sample_non_smoker)
  mean_smoker <- mean(sample_smoker)
  mean_non_smoker - mean_smoker
})

# (b) Compute the 95% confidence interval for µNonSmoker - µSmoker using percentile bootstrap
lower_percentile <- quantile(bootstrap_diff, 0.025)
upper_percentile <- quantile(bootstrap_diff, 0.975)
cat("95% Confidence Interval (Bootstrap): [", lower_percentile, ",", upper_percentile , "]\n")

# (c) Compute the 95% confidence interval for µNonSmoker - µSmoker using the t-version
# Calculate the standard error of the difference
n_non_smoker <- sum(smoker == "No")
n_smoker <- sum(smoker == "Yes")
sd_non_smoker <- sd(weight[smoker == "No"])
sd_smoker <- sd(weight[smoker == "Yes"])
se_diff <- sqrt((sd_non_smoker^2 / n_non_smoker) + (sd_smoker^2 / n_smoker))

# Calculate the t-statistic and degrees of freedom
t_stat <- (mean(weight[smoker == "No"]) - mean(weight[smoker == "Yes"])) / se_diff
df <- n_non_smoker + n_smoker - 2  # degrees of freedom

# Calculate the t-distribution quantiles for the confidence interval
t_quantile <- qt(c(0.025, 0.975), df)
lower_t <- t_stat + t_quantile[1] * se_diff
upper_t <- t_stat + t_quantile[2] * se_diff
# Display the results
cat("95% Confidence Interval (t-version): [", lower_t, ",", upper_t, "]\n")

# (d) Interpretation
cat("Bootstrap 95% Confidence Interval (µNonSmoker - µSmoker): [", lower_percentile, ", ", upper_percentile, "]\n")
cat("T-distribution 95% Confidence Interval (µNonSmoker - µSmoker): [", lower_t, ", ", upper_t, "]\n")

# Compare the two intervals
if (lower_percentile > upper_t) {
  cat("Children born to birth mothers who did not smoke during pregnancy weigh more on average than babies born to birth mothers who did smoke during pregnancy.\n")
} else if (upper_percentile < lower_t) {
  cat("Children born to birth mothers who did smoke during pregnancy weigh more on average than babies born to birth mothers who did not smoke during pregnancy.\n")
} else {
  cat("There is no clear evidence to conclude whether there is a significant difference in birth weight between the two groups.\n")
}

##Quetion 2
# (a) Create a distribution of the bootstrap statistic SSmoker - SNonSmoker
n_replications <- 1000

# Create empty vectors to store the bootstrap statistics
bootstrap_stats_smoker <- numeric(n_replications)
bootstrap_stats_non_smoker <- numeric(n_replications)

# Perform bootstrapping to calculate SSmoker and SNonSmoker
for (i in 1:n_replications) {
  # Sample with replacement from smokers
  sample_smoker <- sample(weight[smoker == "Yes"], replace = TRUE)
  # Sample with replacement from non-smokers
  sample_non_smoker <- sample(weight[smoker == "No"], replace = TRUE)
  # Calculate standard deviation for both groups
  sd_smoker <- sd(sample_smoker)
  sd_non_smoker <- sd(sample_non_smoker)
  # Store the bootstrap statistics
  bootstrap_stats_smoker[i] <- sd_smoker
  bootstrap_stats_non_smoker[i] <- sd_non_smoker
}

# Visualization of the bootstrap distribution
par(mfrow = c(2, 1))  # Create a 2x1 grid of plots
hist(bootstrap_stats_smoker, main = "Bootstrap Distribution of σSmoker",
     xlab = "Standard Deviation (grams)", col = "lightblue")
hist(bootstrap_stats_non_smoker, main = "Bootstrap Distribution of σNonSmoker",
     xlab = "Standard Deviation (grams)", col = "lightblue")

#(b) Create a Normal Probability Plot for both bootstrap statistics
qqnorm(bootstrap_stats_smoker, main = "Normal Probability Plot of σSmoker")
qqline(bootstrap_stats_smoker, col = 2)
qqnorm(bootstrap_stats_non_smoker, main = "Normal Probability Plot of σNonSmoker")
qqline(bootstrap_stats_non_smoker, col = 2)

# (c) Compute the 95% bootstrap percentile interval for σSmoker and σNonSmoker
lower_percentile_smoker <- quantile(bootstrap_stats_smoker, 0.025)
upper_percentile_smoker <- quantile(bootstrap_stats_smoker, 0.975)
lower_percentile_non_smoker <- quantile(bootstrap_stats_non_smoker, 0.025)
upper_percentile_non_smoker <- quantile(bootstrap_stats_non_smoker, 0.975)

cat("95% Bootstrap Percentile Interval for σSmoker: [", lower_percentile_smoker, ",", upper_percentile_smoker, "]\n")
cat("95% Bootstrap Percentile Interval for σNonSmoker: [", lower_percentile_non_smoker, ",", upper_percentile_non_smoker, "]\n")

# (d) Explain the practical meaning of the result in part (c) with respect to the variable Weight
cat("The 95% bootstrap percentile interval for σSmoker and σNonSmoker provides an estimate of the population standard deviations of birth weights for babies born to smoking and non-smoking mothers, respectively. This interval represents the range of likely values for the population standard deviations based on the available data.\n\n")

cat("For σSmoker, the interval [", lower_percentile_smoker, ",", upper_percentile_smoker, "] suggests that the population standard deviation of birth weights for babies born to smoking mothers is expected to fall within this range with 95% confidence. Similarly, for σNonSmoker, the interval [", lower_percentile_non_smoker, ",", upper_percentile_non_smoker, "] suggests the same for babies born to non-smoking mothers.\n\n")

cat("The practical significance of this result is that it provides a measure of the variability in birth weights within each group. A wider interval indicates greater uncertainty in estimating the population standard deviation, suggesting that birth weights vary more within that group. In contrast, a narrower interval indicates more certainty in estimating the population standard deviation, suggesting that birth weights are more consistent within that group.")

##Question three

mercury_levels <- c(1.2, 1.1, 1.0, 1.0, 1.1, 1.0, 1.0, 1.0, 0.9, 1.1, 1.1, 1.2, 1.0, 1.1, 1.0, 1.1, 1.0, 0.9, 1.0, 1.0, 1.1, 1.0, 1.0, 1.1, 1.2, 1.0, 1.1, 1.0, 1.0, 1.2, 1.1)
n <- length(mercury_levels)

# (a) Establish statistical hypotheses
# Null Hypothesis (H0): Mean mercury level is less than or equal to 1 ppm (µ ≤ 1).
# Alternative Hypothesis (H1): Mean mercury level exceeds 1 ppm (µ > 1).

# (b) Type I and Type II Errors:
# - Type I Error (False Positive): Rejecting H0 when it's true, i.e., concluding that mercury levels exceed 1 ppm when they don't.
# - Type II Error (False Negative): Failing to reject H0 when it's false, i.e., not concluding that mercury levels exceed 1 ppm when they do.

#(c) Visualize data with a boxplot
boxplot(mercury_levels, main = "Mercury Levels in Walleye", ylab = "Mercury Level (ppm)")

# Commenting on the distribution:
# The boxplot shows that most of the data points are clustered around 1 ppm, but there are a few outliers above 1 ppm.

# (d) Hypothesis Test and Interpretation
# Performing a one-sample t-test
test_result <- t.test(mercury_levels, mu = 1, alternative = "greater")

# Print the p-value
cat("P-value:", test_result$p.value, "\n")

# Interpret the result
if (test_result$p.value < 0.05) {
  cat("The p-value is less than 0.05. We reject the null hypothesis.\n")
  cat("These data suggest that Health Canada should consider a moratorium on commercial walleye fishing downstream of Whitecourt due to elevated mercury levels.\n")
  
  # Calculate a 95% confidence interval for the mean mercury level
  lower_ci <- test_result$conf.int[1]
  upper_ci <- test_result$conf.int[2]
  cat("95% Confidence Interval for Mean Mercury Level (ppm): [", lower_ci, ",", upper_ci, "]\n")
} else {
  cat("The p-value is greater than or equal to 0.05. We fail to reject the null hypothesis.\n")
  cat("There is not enough evidence to suggest a moratorium on commercial walleye fishing based on these data.\n")
}

## Question four

# Given data
sample_size <- 845
certified_count <- 475
transitioning_count <- 75
hypothetical_proportion <- 0.60  # 60%

# Calculate the sample proportion
sample_proportion <- (certified_count + transitioning_count) / sample_size

# Perform a one-sided hypothesis test
# Null Hypothesis (H0): Proportion of certified or transitioning coffee growers is <= 60%
# Alternative Hypothesis (H1): Proportion of certified or transitioning coffee growers is > 60%
test_result <- prop.test(x = certified_count + transitioning_count, n = sample_size, p = hypothetical_proportion, alternative = "greater")

# Displaying the test result
cat("Sample Proportion:", sample_proportion, "\n")
cat("Test Statistic:", test_result$statistic, "\n")
cat("P-value:", test_result$p.value, "\n")

# Interpreingt the result
if (test_result$p.value < 0.05) {
  cat("There is ample statistical evidence to confirm that the proportion of certified coffee growers in Southern Mexico who are either certified or in the process of being certified is more than 60%.\n")
} else {
  cat("There is not enough statistical evidence to confirm that the proportion of certified coffee growers exceeds 60% in Southern Mexico.\n")
}

# qUESTIO five
# Given data for the recent poll
recent_poll_sample_size <- 399
recent_poll_proportions <- c(128, 96, 104) / recent_poll_sample_size  # Proportions for Conservative, Liberal, and NDP
recent_poll_prop_conservative <- recent_poll_proportions[1]

# Proportion from July
prop_july <- 0.36  # 36%

# Perform a z-test for proportions
# Null Hypothesis (H0): Proportion in the recent poll is the same as in July (p_recent = p_july)
# Alternative Hypothesis (H1): Proportion in the recent poll is greater than in July (p_recent > p_july)

# Calculate the standard error for the difference in proportions
se_diff <- sqrt((prop_july * (1 - prop_july)) / recent_poll_sample_size)

# Calculate the z-score
z_score <- (recent_poll_prop_conservative - prop_july) / se_diff

# Calculate the p-value for a one-sided test
p_value <- 1 - pnorm(z_score)

# Display the results
cat("Z-Score:", z_score, "\n")
cat("P-value:", p_value, "\n")

# Interpret the result
alpha <- 0.05  # Significance level
if (p_value < alpha) {
  cat("There is evidence to suggest that the proportion of Gen Z Canadians who would vote for their Progressive Conservative constituency candidate has increased since July.\n")
} else {
  cat("There is not enough evidence to suggest that the proportion has increased since July.\n")
}

##Question Six
# Sample size
n <- 50

# (a) State the statistical hypotheses
# Null Hypothesis (H0): The candidate will receive less than 45% of the vote (p < 0.45).
# Alternative Hypothesis (H1): The candidate will receive at least 45% of the vote (p >= 0.45).

# (b) Compute the value of α used in your derivation of the decision rule
alpha <- 1 - pbinom(19, size = n, prob = 0.45)
alpha
# (c) Probability that candidate will run for office if p = 0.42
p_run <- 1 - pbinom(19, size = n, prob = 0.42)
cat("Probability to run for office if p = 0.42:", p_run, "\n")

#(d) Probability for different values of p and create a plot
p_values <- c(0.41, 0.40, 0.39, 0.38, 0.35, 0.30)
probabilities <- 1 - pbinom(19, size = n, prob = p_values)

# Plot
plot(p_values, probabilities, type = "b", ylim = c(0, 1), xlab = "p (Proportion of Support)", ylab = "Probability to Run for Office",
     main = "Probability vs. Proportion of Support")
abline(h = alpha, col = "red", lty = 2)
legend("topright", legend = paste("α =", round(alpha, 2)), col = "red", lty = 2, cex = 0.7)

# (e) Interpretation and suggestions
# The plot shows the relationship between the proportion of support (p) and the probability to run for office.
# When p is close to or greater than 0.45, the probability to run for office increases.

# Suggestions for improving the test:
# 1. Increase the sample size (n): A larger sample size provides more reliable results and smaller confidence intervals.
# 2. Adjust the significance level (α): Depending on the candidate's risk tolerance, α can be set differently to control Type I error.
# 3. Consider a two-sided test: Instead of only checking if support is at least 45%, consider a two-sided test to detect changes in both directions.

## Question seven
# Given data for 2012
n_2012 <- 1010
support_2012 <- 601

# Given data for 2019
n_2019 <- 1000
support_2019 <- 561

# Proportions
p_2012 <- support_2012 / n_2012
p_2019 <- support_2019 / n_2019

# (a) Compute a 95% confidence interval for p2019 - p2012
# Calculate the standard error for the difference in proportions
se_diff <- sqrt((p_2012 * (1 - p_2012) / n_2012) + (p_2019 * (1 - p_2019) / n_2019))

# Calculate the z-score for a 95% confidence interval (two-tailed)
z <- qnorm(0.975)

# Calculate the margin of error
margin_of_error <- z * se_diff

# Calculate the confidence interval
lower_ci <- (p_2019 - p_2012) - margin_of_error
upper_ci <- (p_2019 - p_2012) + margin_of_error

# Display the confidence interval
cat("95% Confidence Interval for p2019 - p2012: [", lower_ci, ",", upper_ci, "]\n")

# (b) Determining statistical significance
# To determine if there is a statistically significant difference, we check if the confidence interval contains zero.

if (lower_ci > 0 || upper_ci < 0) {
  cat("There is a statistically significant difference between p2019 and p2012.\n")
} else {
  cat("There is no statistically significant difference between p2019 and p2012.\n")
}
 # Question eight
# Given data (weights of cereal in grams)
cereal_weights <- c(497.2, 499.9, 495.8, 514.2, 490.0, 498.3, 495.1, 486.7)

# Assumptions:
# 1. The cereal weights are normally distributed.
# 2. The cereal weights have constant variance.
# 3. The samples are independent.

# (a) Perform a one-sample t-test
# Null Hypothesis (H0): The mean cereal weight is 500 grams (μ = 500)
# Alternative Hypothesis (H1): The mean cereal weight is different from 500 grams (μ ≠ 500)

# Calculate the sample mean and standard error of the mean (SEM)
sample_mean <- mean(cereal_weights)
sem <- sd(cereal_weights) / sqrt(length(cereal_weights))

# Perform the t-test
t_stat <- (sample_mean - 500) / sem

# Degrees of freedom
df <- length(cereal_weights) - 1

# Two-tailed test
p_value <- 2 * pt(abs(t_stat), df)

# Display the results
cat("Sample Mean Cereal Weight:", sample_mean, "grams\n")
cat("t-statistic:", t_stat, "\n")
cat("Degrees of Freedom:", df, "\n")
cat("P-value:", p_value, "\n")

#(b) Interpretation
alpha <- 0.05  # Significance level
if (p_value < alpha) {
  cat("There is statistically significant evidence to suggest that the mean cereal weight is different from 500 grams.\n")
} else {
  cat("There is no statistically significant evidence to suggest that the mean cereal weight is different from 500 grams.\n")
}

#Question nine
# Given data
sample_data <- c(2.95, 2.21, 2.43, 2.11, 2.77, 3.12)
n <- length(sample_data)
alpha <- 0.05  # Significance level (given in part a)

# (a) What value of α was used in the derivation of this statistical test?
cat("Alpha (α) used in the derivation of the statistical test:", alpha, "\n")

#(b) Does this sample support the null hypothesis above?

# Sort the data in ascending order
sorted_data <- sort(sample_data)

# Calculate the minimum value in the sample
x_min <- min(sorted_data)

# Check if we reject the null hypothesis based on x_min
reject_null <- x_min < 2.018

cat("Minimum value in the sample (x_min):", x_min, "\n")
if (reject_null) {
  cat("The sample does not support the null hypothesis (HA is favored).\n")
} else {
  cat("The sample supports the null hypothesis (H0 is not rejected).\n")
}

#(c) Compute the probability of concluding that β = 2 when β = 1.8

# Calculate the probability of rejecting H0 when β = 1.8
p_reject_h0 <- pexp(2.018 - 1.8, rate = n)

cat("Probability of concluding β = 2 when β = 1.8:", p_reject_h0, "\n")

#(d) Value(s) of x_min that result in a rejection of the null hypothesis with α = 0.20

# Calculate the critical value(s) for a significance level of 0.20
critical_value <- qexp(0.20, rate = n)

cat("Critical value(s) for α = 0.20:", critical_value, "\n")

##Question ten
# Load the required library
library(readr)

# (a) Import the data and perform a hypothesis test
# Read the data from the  CSV file
book_prices <- read_csv("C:/Users/turningpointKS/Downloads/bookprices.csv")

# Check the structure of the data
str(book_prices)

# Perform a hypothesis test
# Null Hypothesis (H0): Mean price at U of Calgary Bookstore <= Mean price on Amazon.ca
# Alternative Hypothesis (HA): Mean price at U of Calgary Bookstore > Mean price on Amazon.ca

# Assuming the data is normally distributed, we can use a one-sample t-test
t_test_result <- t.test(book_prices$UsedBkStore, book_prices$UsedAmazon, alternative = "greater")

# Extract the p-value from the test result
p_value <- t_test_result$p.value

# Display the results
cat("P-value:", p_value, "\n")

if (p_value < 0.05) {
  cat("Based on the p-value, we can conclude that the price of a used textbook at the University of Calgary Bookstore is significantly higher than the price on Amazon.ca.\n")
} else {
  cat("There is no significant evidence to conclude that the price of a used textbook at the University of Calgary Bookstore is higher than the price on Amazon.ca.\n")
}

#(b) Creating a visualization to check the assumption/condition (e.g., normality)

# You can create a histogram or a Q-Q plot to check the normality assumption
hist(book_prices$UsedBkStore, main = "Histogram of U of Calgary Bookstore Prices", xlab = "Price")
hist(book_prices$UsedAmazon, main = "Histogram of Amazon.ca Prices", xlab = "Price")

# (c) Compute a 95% confidence interval for the mean difference

# Calculate the mean difference and standard error
mean_diff <- mean(book_prices$UsedBkStore) - mean(book_prices$UsedAmazon)
sem_diff <- sqrt(var(book_prices$UsedBkStore)/length(book_prices$UsedAmazon) + var(book_prices$UsedAmazon)/length(book_prices$UsedAmazon))

# Calculate the confidence interval
conf_interval <- mean_diff + c(-1, 1) * qt(0.975, df = length(book_prices$UsedBkStore) + length(book_prices$UsedAmazon) - 2) * sem_diff

# Display the confidence interval
cat("95% Confidence Interval for Mean Difference:", conf_interval, "\n")

# Interpreting the meaning of the interval
cat("The 95% confidence interval for the mean difference in used book prices suggests that the mean price at the University of Calgary Bookstore is estimated to be between", conf_interval[1], "and", conf_interval[2], "dollars higher than the mean price on Amazon.ca, on average, based on this sample.\n")

library(tinytex)
library(tinytex)
install_tinytex()
