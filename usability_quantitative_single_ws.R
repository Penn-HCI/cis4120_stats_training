# Install necessary packages
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("tinytex")

# Load the packages
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tinytex)

# Read the Excel file
time_data <- read_excel("data/usability_study_quantitative_data_ws.xlsx")

# Boxplot
ggplot(time_data, aes(x = Version, y = Time_Taken)) +
  geom_boxplot() +
  labs(title = "Boxplot of Task Completion Time by App Version",
       x = "Version",
       y = "Time Taken") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Density plot
ggplot(time_data, aes(x = Time_Taken, fill = Version)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Task Completion Time by App Version",
       x = "Time Taken",
       y = "Density") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Compute summary statistics for each version
summary_stats_time <- time_data %>%
  group_by(Version) %>%
  summarize(
    Count = n(),
    Mean = mean(Time_Taken),
    Median = median(Time_Taken),
    Standard_Deviation = sd(Time_Taken),
    Min = min(Time_Taken),
    Max = max(Time_Taken)
  )

# Print the summary statistics
print(summary_stats_time)

# Reshaping the data to wide format
time_data_wide <- time_data %>%
  pivot_wider(names_from = Version, values_from = Time_Taken)

# Perform the paired t-test
t_test_result <-
  t.test(time_data_wide$A, time_data_wide$B, paired = TRUE)

# Print the results
print(t_test_result)