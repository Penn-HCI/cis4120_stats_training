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
likert_data_multiple <- read_excel("data/usability_study_likert_data_bs.xlsx")

# Reshape the data to long format
likert_data_long <- pivot_longer(likert_data_multiple,
                                 cols = starts_with("Question"),
                                 names_to = "Question",
                                 values_to = "Likert_Rating")

# Function to create a bar chart for a given question
plot_question <- function(question) {
  filter(likert_data_long, Question == question) %>%
    ggplot(aes(x = Likert_Rating, fill = Version)) +
    geom_bar(position = "dodge") +
    labs(title = paste("Frequency of Likert Ratings for", question, "by App Version"),
         x = "Likert Rating",
         y = "Frequency") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
}

# Create and view plots for each question
for (q in unique(likert_data_long$Question)) {
  print(plot_question(q))
}

# Compute summary statistics for each version and question
summary_stats_likert_multiple <- likert_data_long %>%
  group_by(Question, Version) %>%
  summarise(
    Count = n(),
    Mean = mean(Likert_Rating),
    Median = median(Likert_Rating),
    Standard_Deviation = sd(Likert_Rating),
    Min = min(Likert_Rating),
    Max = max(Likert_Rating)
  )

# Print the summary statistics
options(dplyr.print_max = Inf)
print(summary_stats_likert_multiple)

# Wilcoxon rank sum test (Mann-Whitney U test)
# Initialize a data frame to store the test results
wilcoxon_ranked_sum_multiple <- data.frame(Question = character(),
                                           W_statistic = numeric(),
                                           p_value = numeric(),
                                           stringsAsFactors = FALSE)

# Loop through each question and perform the Wilcoxon Rank Sum test
for (q in unique(likert_data_long$Question)) {
  test_data <- filter(likert_data_long, Question == q)
  test_result <- wilcox.test(Likert_Rating ~ Version, data = test_data)

  # Store the results
  wilcoxon_ranked_sum_multiple <- rbind(wilcoxon_ranked_sum_multiple, data.frame(Question = q,
                                                                                 W_statistic = test_result$statistic,
                                                                                 p_value = test_result$p.value))
}

# Print the test results
print(wilcoxon_ranked_sum_multiple)
