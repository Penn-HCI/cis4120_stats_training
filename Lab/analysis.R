# Install necessary packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("lme4")

# Load the packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)

# Read the CSV file
data <- read.csv("Lab/sample_data.csv")

# Function to create a bar chart for a given Likert question
plot_question_versions <- function(question) {
  # Create a bar chart comparing the number of responses for each rating
  ggplot(filter(data, grepl(question, Likert)), aes(x = Likert_Rating, fill = Version)) +
    geom_bar(position = "dodge") +
    labs(title = paste("Frequency of Ratings for", question, "by App Version"),
         x = "Rating",
         y = "Frequency") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
}

# Loop through each question and plot
for (question in unique(data$Likert)) {
  print(plot_question_versions(question))
}

# Density plot of task completion time by app version
ggplot(data, aes(x = Time_Taken, fill = Version)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Task Completion Time by App Version",
       x = "Time Taken",
       y = "Density") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Compute summary statistics for Likert_Rating for each version and Likert question
summary_stats_likert <- data %>%
  group_by(Version, Likert) %>%
  summarize(
    Count = n(),
    Mean = mean(Likert_Rating),
    Median = median(Likert_Rating),
    Standard_Deviation = sd(Likert_Rating),
    Min = min(Likert_Rating),
    Max = max(Likert_Rating),
    .groups = "drop"
  )

# Remove duplicates as data is in wide format
unique_time_data <- data %>%
  group_by(Participant_ID, Version) %>%
  distinct(Time_Taken, .keep_all = TRUE)

# Compute summary statistics for Time_Taken for each version
summary_stats_time <- unique_time_data %>%
  group_by(Version) %>%
  summarize(
    Count = n(),
    Mean = mean(Time_Taken),
    Median = median(Time_Taken),
    Standard_Deviation = sd(Time_Taken),
    Min = min(Time_Taken),
    Max = max(Time_Taken),
    .groups = "drop"
  )

# Print the summary statistics
print(summary_stats_likert)
print(summary_stats_time)

# Initialize an empty data frame to store the results
wilcox_test_results <- data.frame(
  Question = character(),
  W = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each unique Likert question
for (question in unique(data$Likert)) {
  # Subset the data to only include rows related to the current Likert question
  question_data <- filter(data, Likert == question)

  # Perform the Wilcoxon signed rank test
  wilcox_test_result <- wilcox.test(Likert_Rating ~ Version, data = question_data)

  # Store the results in the data frame
  wilcox_test_results <- rbind(wilcox_test_results, data.frame(
    Question = question,
    W = wilcox_test_result$statistic,
    P_Value = wilcox_test_result$p.value
  ))
}

# Print the data frame
print(wilcox_test_results)

# Subset the data for each version and perform the unpaired t-test
t_test_result <- t.test(filter(unique_time_data, Version == "A")$Time_Taken,
                        filter(unique_time_data, Version == "B")$Time_Taken)

# Print the results
print(t_test_result)

# Convert the Expertise column to a factor with ordered levels
data$Expertise <- factor(data$Expertise, ordered = TRUE, levels = c("Beginner", "Intermediate", "Advanced"))

# Fit the model
model <- lmer(Time_Taken ~ Expertise + Version * Task + (1|Participant_ID), data = data)

# Print the model summary
summary(model)