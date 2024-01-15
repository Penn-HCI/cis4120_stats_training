# Install necessary packages
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")

# Load the packages
library(readxl)
library(ggplot2)

# Read the Excel file
time_data <- read_excel("data/usability_study_data.xlsx")

# Boxplot
ggplot(time_data, aes(x = Version, y = Time)) +
  geom_boxplot() +
  labs(title = "Boxplot of Task Completion Time by App Version") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Density plot
ggplot(time_data, aes(x = Time, fill = Version)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Task Completion Time by App Version") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Compute summary statistics for each version
summary_stats_time <- time_data %>%
  group_by(Version) %>%
  summarize(
    Count = n(),
    Mean = mean(Time),
    Median = median(Time),
    Standard_Deviation = sd(Time),
    Min = min(Time),
    Max = max(Time)
  )

# Print the summary statistics
print(summary_stats_time)

# Perform a two-sample two-tailed t-test
t_test_result <- t.test(Time ~ Version, data = time_data)

# Print the results
print(t_test_result)

# Perform a two-sample one-tailed t-test
# Halve the p-value if the test statistic is in the direction of the hypothesis
if (t_test_result$statistic > 0) {
    p_value_one_tailed <- t_test_result$p.value / 2
} else {
    p_value_one_tailed <- 1 - (t_test_result$p.value / 2)
}