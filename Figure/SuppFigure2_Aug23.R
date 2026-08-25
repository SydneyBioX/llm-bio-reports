# Load required libraries
library(ggplot2)
library(dplyr)
library(stringr)

# Read and process data (keeping only what's needed for this figure)
url <- "https://docs.google.com/spreadsheets/d/1yFDcz5VEJg4iws9exABzG9eMF46wLGS38pWxalxQC-k/export?format=csv&gid=339476217"
mcq_data <- read.csv(url, check.names = F)
mcq_data[, "qid"] <- paste0(mcq_data$CaseStudy_ID, "_", mcq_data$Question_ID)

url <- 'https://docs.google.com/spreadsheets/d/1yFDcz5VEJg4iws9exABzG9eMF46wLGS38pWxalxQC-k/export?format=csv&gid=652035055'
case_data_raw <- read.csv(url, stringsAsFactors = FALSE, check.names = F)
case_data <- case_data_raw

# Read strategy data files
strategy1_old <- read.csv("data/APRIL15_MCQ_result_strategy1.csv", check.names = F)
strategy2_old <- read.csv("data/APRIL16_MCQ_result_strategy2_cleaned.csv", check.names = F)

strategy1_new <- read.csv("../../20260820-new_MCQ_analysis-LY/out/AUG11_MCQ_result_strategy1_merged_clean.csv", check.names = F)
strategy2_new <- read.csv("../../20260820-new_MCQ_analysis-LY/out/AUG18_MCQ_result_strategy2_merged_clean.csv", check.names = F)

colnames(strategy1_old) <- gsub("\\.", " ", colnames(strategy1_old))
colnames(strategy2_old) <- gsub("\\.", " ", colnames(strategy2_old))
colnames(strategy1_new) <- gsub("\\.", " ", colnames(strategy1_new))
colnames(strategy2_new) <- gsub("\\.", " ", colnames(strategy2_new))

strategy1 <- bind_rows(strategy1_old, strategy1_new)
strategy2 <- bind_rows(strategy2_old, strategy2_new)

# Process strategy1 data
strategy1$Info_check <- gsub("\\.", "", strategy1$Info_check)
strategy1 <- strategy1 |>
  filter(Info_check %in% c("Yes", "No"))
strategy1$correct <- as.character(strategy1$Answer) == as.character(strategy1$Model_return)
strategy1[, "qid"] <- paste0(strategy1$CaseStudy_ID, "_", strategy1$Question_ID)

# Merge strategy1 with case data
case_data_clean <- case_data[, !names(case_data) %in% setdiff(intersect(names(strategy1), names(case_data)), "Sample_ID")]
strategy1_case <- merge(strategy1, case_data_clean, by = "Sample_ID", all.x = TRUE)
mcq_data_clean <- mcq_data[, !names(mcq_data) %in% setdiff(intersect(names(strategy1_case), names(mcq_data)), "qid")]
strategy1_full <- merge(strategy1_case, mcq_data_clean, by = "qid", all.x = TRUE)

# Extract report model name
strategy1_full[, "report_model"] <- str_extract(strategy1_full$Report_name, "(?<=\\.)[^.]+(?:\\.[^.]+)*(?=\\.txt)")

# Process strategy2 data
strategy2$Info_check <- gsub("\\.", "", strategy2$Info_check)
strategy2 <- strategy2 |>
  filter(Info_check %in% c("Yes", "No"))
strategy2$correct <- as.character(strategy2$Answer) == as.character(strategy2$Model_return)
strategy2[, "qid"] <- paste0(strategy2$CaseStudy_ID, "_", strategy2$Question_ID)

# Merge strategy2 with case data
case_data_clean <- case_data[, !names(case_data) %in% setdiff(intersect(names(strategy2), names(case_data)), "Sample_ID")]
strategy2_case <- merge(strategy2, case_data_clean, by = "Sample_ID", all.x = TRUE)
mcq_data_clean <- mcq_data[, !names(mcq_data) %in% setdiff(intersect(names(strategy2_case), names(mcq_data)), "qid")]
strategy2_full <- merge(strategy2_case, mcq_data_clean, by = "qid", all.x = TRUE)

# Create summary data for the plot
summary1 <- strategy1_full |>
  group_by(Model_name = report_model) |>
  summarise(
    yes_percent = round(100 * sum(Info_check == "Yes") / n(), 1)
  ) |>
  mutate(strategy = "Report model")

summary2 <- strategy2_full |>
  group_by(Model_name) |>
  summarise(
    yes_percent = round(100 * sum(Info_check == "Yes") / n(), 1)
  ) |>
  mutate(strategy = "Baseline")

# Combine both summaries
combined_summary <- bind_rows(summary1, summary2)

# Rename model names for cleaner display
combined_summary <- combined_summary |>
  mutate(Model_name = recode(Model_name,
                             "claude-3-7-sonnet-20250219" = "Claude 3.7",
                             "claude-opus-5" = "Claude Opus 5",
                             "gemini-2.0-flash" = "Gemini 2.0",
                             "gemini-3.6-flash" = "Gemini 3.6",
                             "gpt-4o" = "GPT-4o",
                             "o1-2024-12-17" = "o1",
                             "gpt-5.6-sol" = "GPT-5.6"
  ))

combined_summary$Model_name <- factor(combined_summary$Model_name,
                                      levels = c("Claude 3.7", "Claude Opus 5",
                                                 "Gemini 2.0", "Gemini 3.6",
                                                 "GPT-4o", "o1", "GPT-5.6"))


# Set position dodge
dodge <- position_dodge(width = 0.7)

# Create the plot
sup.f2=ggplot(combined_summary, aes(x = Model_name, y = yes_percent, fill = strategy)) +
  geom_bar(stat = "identity", position = dodge, width = 0.6) +
  geom_text(aes(label = yes_percent),
            position = dodge,
            vjust = -0.5,
            size = 4) +
  labs(
    title = "Information Retrieval Success Rate by Model",
    x = "LLM Model",
    y = "Information check (% Yes)",
    fill = NULL
  ) +
  ylim(0, 105) +  # extra space for text above 100%
  theme_bw(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    panel.grid.major = element_line(size = 0.2),  # thinner major grid lines
    panel.grid.minor = element_line(size = 0.1)   # thinner minor grid lines
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
        axis.text.y = element_text(color = "black"))



ggsave("SuppFigure2_Aug23.pdf",plot=sup.f2, width = 15, height = 12, units = "cm")
