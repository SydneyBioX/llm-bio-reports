# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

# Read and process data
url <- "https://docs.google.com/spreadsheets/d/1yFDcz5VEJg4iws9exABzG9eMF46wLGS38pWxalxQC-k/export?format=csv&gid=339476217"
mcq_data <- read.csv(url, check.names = F)
mcq_data[, "qid"] <- paste0(mcq_data$CaseStudy_ID, "_", mcq_data$Question_ID)

url <- 'https://docs.google.com/spreadsheets/d/1yFDcz5VEJg4iws9exABzG9eMF46wLGS38pWxalxQC-k/export?format=csv&gid=652035055'
case_data_raw <- read.csv(url, stringsAsFactors = FALSE, check.names = F)
case_data <- case_data_raw

# Read strategy data files
strategy1 <- read.csv("data/APRIL15_MCQ_result_strategy1.csv", check.names = F)
strategy2 <- read.csv("data/APRIL16_MCQ_result_strategy2_cleaned.csv", check.names = F)

# Process strategy1 data
strategy1$Info_check <- gsub("\\.", "", strategy1$Info_check)
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
strategy2$correct <- as.character(strategy2$Answer) == as.character(strategy2$Model_return)
strategy2[, "qid"] <- paste0(strategy2$CaseStudy_ID, "_", strategy2$Question_ID)

# Merge strategy2 with case data
case_data_clean <- case_data[, !names(case_data) %in% setdiff(intersect(names(strategy2), names(case_data)), "Sample_ID")]
strategy2_case <- merge(strategy2, case_data_clean, by = "Sample_ID", all.x = TRUE)
mcq_data_clean <- mcq_data[, !names(mcq_data) %in% setdiff(intersect(names(strategy2_case), names(mcq_data)), "qid")]
strategy2_full <- merge(strategy2_case, mcq_data_clean, by = "qid", all.x = TRUE)

# Create the comparison data for GPT-4o
strategy1_gpt4o <- strategy1_full %>%
  filter(report_model == "gpt-4o") %>%
  select(qid, `Input type`, Model_name, Model_return, Info_check) %>%
  rename(
    Model_return_gpt4o = Model_return,
    Info_check_gpt4o = Info_check
  )

merged_df <- strategy2_full %>%
  left_join(strategy1_gpt4o, by = c("qid", "Input type", "Model_name"))

classified_df <- merged_df %>%
  mutate(
    Info_pair = paste0(Info_check, "/", Info_check_gpt4o)
  )

accuracy_summary_gpt4o <- classified_df %>%
  group_by(Info_pair) %>%
  summarise(
    n = n(),
    baseline_accuracy = mean(Model_return == Answer, na.rm = TRUE),
    gpt4o_accuracy = mean(Model_return_gpt4o == Answer, na.rm = TRUE)
  ) %>%
  arrange(desc(n))

# Create comparison data for Gemini
strategy1_gemini <- strategy1_full %>%
  filter(report_model == "gemini-2.0-flash") %>%
  select(qid, `Input type`, Model_name, Model_return, Info_check) %>%
  rename(
    Model_return_gemini = Model_return,
    Info_check_gemini = Info_check
  )

merged_df <- strategy2_full %>%
  left_join(strategy1_gemini, by = c("qid", "Input type", "Model_name"))

classified_df <- merged_df %>%
  mutate(
    Info_pair = paste0(Info_check, "/", Info_check_gemini)
  )

accuracy_summary_gemini <- classified_df %>%
  group_by(Info_pair) %>%
  summarise(
    n = n(),
    baseline_accuracy = mean(Model_return == Answer, na.rm = TRUE),
    gemini_accuracy = mean(Model_return_gemini == Answer, na.rm = TRUE)
  ) %>%
  arrange(desc(n))

# Create comparison data for Claude
strategy1_claude <- strategy1_full %>%
  filter(report_model == "claude-3-7-sonnet-20250219") %>%
  select(qid, `Input type`, Model_name, Model_return, Info_check) %>%
  rename(
    Model_return_claude = Model_return,
    Info_check_claude = Info_check
  )

merged_df <- strategy2_full %>%
  left_join(strategy1_claude, by = c("qid", "Input type", "Model_name"))

classified_df <- merged_df %>%
  mutate(
    Info_pair = paste0(Info_check, "/", Info_check_claude)
  )

accuracy_summary_claude <- classified_df %>%
  group_by(Info_pair) %>%
  summarise(
    n = n(),
    baseline_accuracy = mean(Model_return == Answer, na.rm = TRUE),
    claude_accuracy = mean(Model_return_claude == Answer, na.rm = TRUE)
  ) %>%
  arrange(desc(n))

# Combine all accuracy summaries
gemini_long <- accuracy_summary_gemini %>%
  mutate(model = "Gemini 2.0") %>%
  select(Info_pair, n, baseline_accuracy, model_accuracy = gemini_accuracy, model)

gpt4o_long <- accuracy_summary_gpt4o %>%
  mutate(model = "GPT-4o") %>%
  select(Info_pair, n, baseline_accuracy, model_accuracy = gpt4o_accuracy, model)

claude_long <- accuracy_summary_claude %>%
  mutate(model = "Claude 3.7") %>%
  select(Info_pair, n, baseline_accuracy, model_accuracy = claude_accuracy, model)

accuracy_all <- bind_rows(gemini_long, gpt4o_long, claude_long)

accuracy_all$Info_pair <- factor(accuracy_all$Info_pair, levels = c("Yes/Yes", "Yes/No", "No/Yes", "No/No"))

accuracy_long <- accuracy_all %>%
  pivot_longer(
    cols = c(baseline_accuracy, model_accuracy),
    names_to = "type",
    values_to = "accuracy"
  )

accuracy_long$type <- recode(accuracy_long$type,
                             baseline_accuracy = "Baseline",
                             model_accuracy = "Report Model")

# Set position dodge
dodge <- position_dodge(width = 0.7)

# Create the plot
ggplot(accuracy_long, aes(x = model, y = accuracy, fill = type)) +
  scale_fill_manual(values = c(
    "Baseline" = "#D7301F",
    "Report Model" = "#2166AC"
  )) +
  geom_bar(stat = "identity", position = dodge, width = 0.6) +
  geom_text(aes(label = round(accuracy, 2)),  # round to 2 decimal places
            position = dodge,
            vjust = -0.5, size = 3.5) +
  facet_wrap(~ Info_pair, nrow = 1) +
  labs(
    x = "LLM Model",
    y = "Accuracy",
    fill = "Source"
  ) +
  scale_y_continuous(limits = c(0, 0.85)) +  # slightly increase upper limit for space
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(size = 0.2),  # thinner major grid lines
    panel.grid.minor = element_line(size = 0.1)   # thinner minor grid lines
  )




# ggsave("information_coverage_yes_no.pdf", width = 25, height = 12 , units = "cm")
