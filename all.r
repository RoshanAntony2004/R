
1.PROGRAM: 
# Load the iris dataset 
data <- iris 
# Calculate descriptive statistics 
summary_stats <- summary(data) 
mean_values <- sapply(data[, 1:4],mean)  # Exclude the Species column from mean calculations 
median_values <- sapply(data[, 1:4], median) 
variance_values <- sapply(data[, 1:4], var) 
sd_values <- sapply(data[, 1:4], sd) 
# Display the results 
cat("Summary Statistics:\n", summary_stats, "\n\n") 
cat("Mean Values:\n", mean_values, "\n\n") 
cat("Median Values:\n", median_values, "\n\n") 
cat("Variance Values:\n", variance_values, "\n\n") 
cat("Standard Deviation Values:\n", sd_values, "\n") 

_____________________________________________________________________________________________________________

2. Prg:
set.seed(123) 
sample_data <- rnorm(20, mean = 10, sd = 2) 
hypothesized_mean <- 10 
alpha <- 0.05 
ttest_res <- t.test(sample_data, mu = hypothesized_mean) 
print("One sample t-test Result") 
print(ttest_res) 
print(paste("Mean of sample:", mean(sample_data))) 
print(paste("Hypothesized mean:", hypothesized_mean)) 
print(paste("T-statistic:", ttest_res$statistic)) 
print(paste("P-value:", ttest_res$p.value)) 
if (ttest_res$p.value < alpha) { 
print("Conclusion: Reject null hypothesis") 
} else { 
print("Conclusion: Accept null hypothesis") 
}

_______________________________________________________________________________________________________________
3.Prg:

alpha <- 0.05 
group1 <- c(13.3, 6.0, 20.0, 8.0, 14.0, 19.0, 0.25, 0.0, 16.0, 24.0, 15.0, 1.0, 15.0, 18.0, 25.0, 16.0, 24.0) 
group2 <- c(22.0, 16.0, 21.7, 21.0, 30.0, 26.0, 19.0, 23.9, 28.0, 23.0) 
t_test_result <- t.test(group1, group2, var.equal = TRUE) 
cat("Standard Two Sample t-test result for Equal Variance:\n") 
print(t_test_result) 
print(paste("p-value:", t_test_result$p.value)) 
if (t_test_result$p.value < alpha) { 
print("Conclusion: Reject the Null hypothesis") 
} else { 
print("Conclusion: Fail to reject Null hypothesis") 
} 

______________________________________________________________________________________________________________

4.PROGRAM: 
# Step 1: Set the significance level 
alpha <- 0.05 
# Step 2: Create a contingency table 
data <- matrix(c(40, 30, 30, 50), nrow = 2) 
#data <- matrix(c(25, 15, 10, 30), nrow = 2) 
rownames(data) <- c("Male", "Female") 
colnames(data) <- c("Bev A", "Bev B") 
# Step 3: Display the contingency table 
cat("Contingency Table:\n") 
print(data) 
# Step 4: Perform the chi-square test 
result <- chisq.test(data) 
# Step 5: Display the chi-square test result 
cat("\nChi-Square Test Result:\n") 
print(result) 
# Step 6: Calculate degrees of freedom 
df <- (nrow(data) - 1) * (ncol(data) - 1) 
# Step 7: Calculate critical value 
critical_value <- qchisq(1 - alpha, df) 
# Step 8: Display critical value 
cat("\nCritical Value (", alpha, "):", critical_value, "\n") 
# Step 9: Compare critical value with test statistic and interpret the results 
if (result$statistic > critical_value) { 
cat("\nReject the null hypothesis. There is a significant association between the variables.\n") 
} else { 
cat("\nFail to reject the null hypothesis. There is no significant association between the variables.\n") 
} 

___________________________________________________________________________________________________________________
