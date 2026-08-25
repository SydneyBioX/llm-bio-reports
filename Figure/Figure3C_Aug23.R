# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(scales)

# Read and process data
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

# Filter for "Code + Data" input type only and create accuracy summaries
accuracy_df2 <- strategy2_full |>
  filter(`Input type` == "Code + Data") |>
  group_by(Model_name, `Google Folder`) |>
  summarise(accuracy = mean(correct), .groups = "drop") |>
  mutate(source = "Baseline")

accuracy_df1 <- strategy1_full |>
  filter(`Input type` == "Code + Data") |>
  group_by(Model_name, `Google Folder`, report_model) |>
  summarise(accuracy = mean(correct), .groups = "drop") |>
  mutate(source = "Report Model")

# Combine accuracy data
accuracy_combined <- accuracy_df1 |>
  left_join(
    accuracy_df2,
    by = c("Model_name", "Google Folder"),
    suffix = c(".report", ".baseline")
  ) |>
  pivot_longer(
    cols = c(accuracy.baseline, accuracy.report),
    names_to = "source",
    values_to = "accuracy"
  ) |>
  mutate(
    source = recode(source,
                    accuracy.baseline = "Baseline",
                    accuracy.report = "Report Model"),
    casestudy_type = ifelse(`Google Folder` %in% c("DE", "Pathway", "CCI"),
                            "biological interpretation", "data interpretation")
  )

# Calculate average accuracy by case study type and source
avg_acc <- accuracy_combined |>
  group_by(report_model, casestudy_type, source) |>
  summarise(avg_accuracy = mean(accuracy, na.rm = TRUE), .groups = "drop")

# Convert to wide format and calculate percentage retained
acc_wide <- avg_acc |>
  pivot_wider(
    names_from = source,
    values_from = avg_accuracy
  ) |>
  mutate(pct_retain = `Report Model` / Baseline)

# Update model names
acc_wide <- acc_wide |>
  mutate(report_model = recode(report_model,
                               "claude-3-7-sonnet-20250219" = "Claude 3.7",
                               "claude-opus-5" = "Claude Opus 5",
                               "gemini-2.0-flash" = "Gemini 2.0",
                               "gemini-3.6-flash" = "Gemini 3.6",
                               "gpt-4o" = "GPT-4o",
                               "o1-2024-12-17" = "o1",
                               "gpt-5.6-sol" = "GPT-5.6"
  ))

acc_wide$report_model <- factor(acc_wide$report_model,
                                levels = c("Claude 3.7", "Claude Opus 5",
                                           "Gemini 2.0", "Gemini 3.6",
                                           "GPT-4o", "o1", "GPT-5.6"))

# Create the plot
fig.3c=ggplot(acc_wide, aes(x = casestudy_type, y = pct_retain, fill = report_model)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.6) +
  geom_text(
    aes(label = round(pct_retain * 100, 1)),  # Convert to %
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    size = 3
  ) +  
  scale_fill_manual(values = c(
    "Claude 3.7" = "#EF6F6A",
    "Claude Opus 5" = "#F4A09C",
    "Gemini 2.0" = "#6388B4",
    "Gemini 3.6" = "#93ACCB",
    "GPT-4o" = "#55AD89",
    "o1" = "#64CDCC",
    "GPT-5.6" = "#E6B655"
  )) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1.2)) +
  labs(
    x = "Case Study Type",
    y = "Percentage Retained",
    fill = "Report Model"
  ) +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(size = 0.2),  # thinner major grid lines
        panel.grid.minor = element_line(size = 0.1) )

ggsave("Figure3C_Aug23.pdf",plot=fig.3c, width = 18, height = 12 , units = "cm")


