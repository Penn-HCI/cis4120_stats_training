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
data <- read_excel("Lab/sample_data_ws.xlsx")

# Bar chart showing the frequency of each Likert rating for each version
ggplot(data, aes(x = Likert, fill = Version)) +
  geom_bar(position = "dodge") +
  labs(title = "Frequency of Likert Ratings by App Version",
       x = "Likert Rating",
       y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Boxplot
ggplot(data, aes(x = Version, y = Quant)) +
  geom_boxplot() +
  labs(title = "Boxplot of Task Completion Time by App Version",
       x = "Version",
       y = "Time Taken") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Density plot
ggplot(data, aes(x = Quant, fill = Version)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Task Completion Time by App Version",
       x = "Time Taken",
       y = "Density") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Stacked bar chart showing the frequency of each Likert rating for each version
ggplot(data, aes(x = Version, fill = factor(Likert))) +
  geom_bar() +
  scale_fill_discrete(name = "Likert Rating") +
  labs(title = "Stacked Bar Chart of Likert Ratings by App Version",
       x = "Version",
       y = "Count") +
  theme(plot.title = element_text(hjust = -0.25, face = "bold"))

# Compute summary statistics Likert for each version
summary_stats_likert <- data %>%
  group_by(Version) %>%
  summarize(
    Count = n(),
    Mean = mean(Likert),
    Median = median(Likert),
    Standard_Deviation = sd(Likert),
    Min = min(Likert),
    Max = max(Likert)
  )

# Compute summary statistics Quantitative for each version
summary_stats_time <- data %>%
  group_by(Version) %>%
  summarize(
    Count = n(),
    Mean = mean(Quant),
    Median = median(Quant),
    Standard_Deviation = sd(Quant),
    Min = min(Quant),
    Max = max(Quant)
  )

# Print the summary statistics
print(summary_stats_likert)
print(summary_stats_time)

# Wilcoxon rank sum test (Mean-Whitney U test)
# Assumes this is a between-subjects design
# Assuming data is paired, reshape the data

# Perform the Wilcoxon rank sum test (Mann-Whitney U test)
wilcox_test_result <- wilcox.test(Likert ~ Version, data = data)

# Print the results
print(wilcox_test_result)

# Reshaping the data to wide format
time_data_wide <- data %>%
  pivot_wider(names_from = Version, values_from = Quant)

# Perform the paired t-test
t_test_result <-
  t.test(time_data_wide$A, time_data_wide$B, paired = TRUE)

# Print the results
print(t_test_result)