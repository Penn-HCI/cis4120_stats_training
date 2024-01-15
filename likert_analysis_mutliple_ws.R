# Install necessary packages
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tinytex")

# Load the packages
library(readxl)
library(ggplot2)
library(tidyr)
library(tinytex)

# Read the Excel file
likert_data_multiple_ws <- read_excel("data/usability_study_likert_within_subjects_data.xlsx")

# Reshape the data to long format
data_long <- pivot_longer(likert_data_multiple_ws,
                          cols = starts_with("Question"),
                          names_to = "Question",
                          values_to = "Likert_Rating")

# Function to create a bar chart for a given question
plot_question_versions <- function(question) {
  filtered_data <- filter(data_long, Question == question)

  ggplot(filtered_data, aes(x = as.factor(Likert_Rating), fill = Version)) +
    geom_bar(position = position_dodge()) +
    scale_fill_brewer(palette = "Set1", name = "Version") +
    labs(title = paste("Bar Chart for", question),
         x = "Likert Rating",
         y = "Frequency") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
}

# Get unique questions
unique_questions <- unique(data_long$Question)

# Loop through each question and plot
for (question in unique_questions) {
  print(plot_question_versions(question))
}

# Compute summary statistics for each version and question
summary_stats_likert_multiple <- data_long %>%
  group_by(Question, Version) %>%
  summarize(
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

# Wilcoxon signed rank test
# Initializing a dataframe to store the results
test_results <- data.frame(Question = character(),
                           Wilcoxon_Statistic = numeric(),
                           P_Value = numeric(),
                           stringsAsFactors = FALSE)

data_wide <- data_long %>%
  pivot_wider(names_from = Version, values_from = Likert_Rating)

# Looping through each question to perform the test
for (q in unique(data_long$Question)) {
  # Extracting the paired responses for each version
  responses <- filter(data_wide, Question == q)

  # Performing the Wilcoxon signed-rank test
  test <- wilcox.test(responses$`A`, responses$`B`, paired = TRUE)

  # Storing the results
  test_results <- rbind(test_results,
                        data.frame(Question = q,
                                   Wilcoxon_Statistic = test$statistic,
                                   P_Value = test$p.value))
}

# Displaying the test results
print(test_results)
