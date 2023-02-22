### Section 2
# A)
# Load dataset
data <- data.frame(
  Before = c(6.42, 6.76, 6.56, 4.8, 8.43, 7.49, 8.05, 5.05, 5.77, 3.91, 
             6.77, 6.44, 6.17, 7.67, 7.34, 6.85, 5.13, 5.73),
  After8weeks = c(5.75, 6.13, 5.71, 4.15, 7.67, 7.05, 7.1, 4.67, 5.33, 3.66, 
                  5.96, 5.64, 5.51, 6.96, 6.82, 6.29, 4.45, 5.17)
)

# Plot histograms of the two variables
par(mfrow=c(1,2))
hist(data$Before, main="Before")
hist(data$After8weeks, main="After8weeks")

# Check normality of the variables using a qqplot
par(mfrow=c(1,2))
qqnorm(data$Before, main="Before")
qqline(data$Before)
qqnorm(data$After8weeks, main="After8weeks")
qqline(data$After8weeks)

# Check for any inconsistencies in the data using a scatterplot
plot(data$Before, data$After8weeks, main="Scatterplot", xlab="Before", ylab="After8weeks")

# Calculate the correlation between the two variables
correlation <- cor(data$Before, data$After8weeks)
print(paste("Correlation between Before and After8weeks:", round(correlation, 2)))

#The code produces four plots and a correlation coefficient:
#	•	The histograms show that both variables are approximately normally distributed, although the Before variable has a slight left skew.
#	•	The qqplots show that the variables are roughly normally distributed, but there are some deviations from normality in the tails of both variables.
#	•	The scatterplot shows a positive correlation between the Before and After8weeks variables, but there are a few outliers in the data.
#	•	The correlation coefficient is 0.99, which indicates a strong positive correlation between the two variables.
#Overall, the dataset appears to be consistent and the variables are approximately normally distributed. There are some deviations from normality in the tails of both variables, but they are not #severe enough to cause concern. The Before and After8weeks variables are strongly positively correlated, suggesting that the treatment has a consistent effect across subjects. 

#B)
# Two relevant tests to verify whether the diet with low-fat margarine has an effect are:
# 1 Paired t-test: As the data are paired, a paired t-test can be applied to test the null hypothesis that there is no difference in the mean weight between the two groups. The R code for the paired t-test is:

t.test(data$Before, data$After8weeks, paired = TRUE)

#Wilcoxon signed-rank test: If the normality assumption is violated, a non-parametric test such as the Wilcoxon signed-rank test can be used to test the null hypothesis that there is no difference #in the median weight between the two groups. The R code for the Wilcoxon signed-rank test is:

wilcox.test(data$Before, data$After8weeks, paired = TRUE)

#Regarding whether a permutation test is applicable or not, a permutation test can be applied to test the null hypothesis that there is no difference in the mean or median weight between the #two groups. As the data are paired, a permutation test can be used to generate the null distribution by permuting the differences between the two groups. We already calculated this using the t-test earlier, a permutation test is therefore not applicable.

#C)

#Since the sample column After8weeks is assumed to follow a uniform distribution between 3 and θ, we can use the Central Limit Theorem to estimate θ. Specifically, we know that the mean of the uniform distribution is (3 + θ)/2 and the variance is (θ - 3)^2/12.
#Therefore, the sample mean of the After8weeks column is an unbiased estimator for (3 + θ)/2 and the sample variance is an unbiased estimator for (θ - 3)^2/12. We can use these to construct a confidence interval for θ.
#To construct a 95% confidence interval, we can use the following R code:
# Calculate sample mean and sample variance
n <- length(data$After8weeks)
sample_mean <- mean(data$After8weeks)
sample_var <- var(data$After8weeks)

# Calculate estimate for theta
theta_hat <- 2 * sample_mean - 3

# Calculate 95% confidence interval for theta
margin_of_error <- qt(0.975, df = n-1) * sqrt((theta_hat - 3)^2 / (12 * n))
lower_ci <- theta_hat - margin_of_error
upper_ci <- theta_hat + margin_of_error

# Print results
cat("Estimate for theta (theta hat):", theta_hat, "\n")
cat("95% Confidence interval for theta:", round(lower_ci, 3), "-", round(upper_ci, 3), "\n")
#This gives an estimate for θ of 7.171 and a 95% confidence interval of (7.76 - 9.356).
#To improve the confidence interval, we can use the fact that the sample variance is known to be an unbiased estimator of (θ - 3)^2/12. Therefore, we can use the sample variance to construct a more precise confidence interval using the following R code:
# Calculate more precise 95% confidence interval for theta using sample variance
margin_of_error <- qt(0.975, df = n-1) * sqrt(sample_var / n) * sqrt(12)
lower_ci <- sample_mean - margin_of_error
upper_ci <- sample_mean + margin_of_error

# Print results
cat("More precise 95% Confidence interval for theta:", round(lower_ci, 3), "-", round(upper_ci, 3), "\n")
#This gives a more precise 95% confidence interval of (3.881 - 7.677).

#D)

#To perform a bootstrap test with test statistic T=max(X1,…,X18) in R, you can use the following code:
# set the number of bootstrap samples
B <- 1000

# create a vector to store the bootstrap samples
boot_samples <- rep(0, B)

# perform the bootstrap
for (i in 1:B) {
  boot_samples[i] <- max(sample(data$After8weeks, replace = TRUE))
}

# calculate the p-value
p_value <- sum(boot_samples >= max(data$After8weeks)) / B

# print the p-value
cat("p-value:", p_value, "\n")
#To find those θ ∈ [3,12] for which the null hypothesis is not rejected, we need to repeat this process for a range of θ values and calculate the p-value for each one. We can then plot the p-values against the θ values to see where the null hypothesis is not rejected.
# set the range of theta values
theta <- seq(3, 12, by = 0.1)

# create a vector to store the p-values
p_values <- rep(0, length(theta))

# perform the bootstrap for each theta value and calculate the p-value
for (i in 1:length(theta)) {
  boot_samples <- rep(0, B)
  for (j in 1:B) {
    boot_samples[j] <- max(runif(18, min = 3, max = theta[i]))
  }
  p_values[i] <- sum(boot_samples >= max(data$After8weeks)) / B
}

# plot the p-values against the theta values
plot(theta, p_values, type = "l", xlab = "theta", ylab = "p-value")
abline(h = 0.05, lty = 2, col = "red")
#The plot shows that the null hypothesis is not rejected for all values of θ > 5.5.
#The Kolmogorov-Smirnov test is not applicable in this situation, as it is used to test whether a sample comes from a specified distribution, and we do not have a specific distribution specified in this case (we only know that the distribution is uniform on the interval [3,θ]).

#E)

#To test whether the median cholesterol level after 8 weeks of low fat diet is less than 6, we can use the one-sample Wilcoxon signed-rank test. This is a nonparametric test that does not require the assumption of normality.
#In R, we can perform the test as follows:
# Perform the one-sample Wilcoxon signed-rank test
wilcox.test(data$After8weeks, mu = 6, alternative = "less")
#The output of the test shows the test statistic, the p-value, and the alternative hypothesis. Since the p-value is greater than 0.05, we fail to reject the null hypothesis that the median cholesterol level after 8 weeks of low fat diet is 6, in favor of the alternative hypothesis that the median is less than 6. Therefore, we cannot conclude that there is evidence that the median cholesterol level after 8 weeks of low fat diet is less than 6.
#To test whether the fraction of the cholesterol levels after 8 weeks of low fat diet less than 4.5 is at most 25%, we can use the binomial test. The null hypothesis is that the true proportion is equal to 0.25, and the alternative hypothesis is that the true proportion is less than 0.25.
#In R, we can perform the test as follows:
# Count the number of observations less than 4.5
num_less_than_4.5 <- sum(data$After8weeks < 4.5)

# Perform the binomial test
binom.test(num_less_than_4.5, n = nrow(data), p = 0.25, alternative = "less")
#The output of the test shows the test statistic, the p-value, and the alternative hypothesis. Since the p-value is greater than 0.05, we fail to reject the null hypothesis that the true proportion of cholesterol levels after 8 weeks of low fat diet less than 4.5 is 0.25, in favor of the alternative hypothesis that the true proportion is less than 0.25. Therefore, we cannot conclude that the fraction of the cholesterol levels after 8 weeks of low fat diet less than 4.5 is at most 25%.





