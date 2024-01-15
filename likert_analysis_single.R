# Install necessary packages
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")

# Load the packages
library(readxl)
library(ggplot2)

# Read the Excel file
likert_data <- read_excel("usability_study_likert_data.xlsx")

# Bar chart showing the frequency of each Likert rating for each version
ggplot(likert_data, aes(x = Likert_Rating, fill = Version)) +
  geom_bar(position = "dodge") +
  labs(title = "Frequency of Likert Ratings by App Version",
       x = "Likert Rating",
       y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Stacked bar chart showing the frequency of each Likert rating for each version
ggplot(likert_data, aes(x = Version, fill = factor(Likert_Rating))) +
  geom_bar() +
  scale_fill_discrete(name = "Likert Rating") +
  labs(title = "Stacked Bar Chart of Likert Ratings by App Version",
       x = "Version",
       y = "Count") +
  theme(plot.title = element_text(hjust = -0.25, face = "bold"))

# Compute summary statistics for each version
summary_stats_likert <- likert_data %>%
  group_by(Version) %>%
  summarize(
    Count = n(),
    Mean = mean(Likert_Rating),
    Median = median(Likert_Rating),
    Standard_Deviation = sd(Likert_Rating),
    Min = min(Likert_Rating),
    Max = max(Likert_Rating)
  )

# Print the summary statistics
print(summary_stats_likert)

# Wilcoxon rank sum test (Mean-Whitney U test)
# Assumes this is a between-subjects design
# Assuming data is paired, reshape the data

# Perform the Wilcoxon rank sum test (Mann-Whitney U test)
wilcox_test_result <- wilcox.test(Likert_Rating ~ Version, data = likert_data)

# Print the results
print(wilcox_test_result)

#H_a: There is a difference in the median Likert ratings between the two versions.