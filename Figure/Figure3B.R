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

# Create summary data for strategy2 (baseline)
info_check_by_case_input_question_strategy2 <- strategy2_full %>%
  group_by(`Input type`, Model_name) %>%
  summarise(percent_yes = mean(Info_check == "Yes") * 100,
            correct = mean(correct) * 100,
            incorrect = 100 - mean(correct),
            .groups = "drop") %>%
  rename(percent_yes_s2 = percent_yes, correct_s2 = correct, incorrect_s2 = incorrect)

# Create summary data for strategy1 (report model)
info_check_by_case_input_question_strategy1 <- strategy1_full %>%
  group_by(`Input type`, report_model) %>%
  summarise(percent_yes = mean(Info_check == "Yes") * 100,
            correct = mean(correct) * 100,
            incorrect = 100 - mean(correct),
            .groups = "drop") %>%
  rename(percent_yes_s1 = percent_yes, correct_s1 = correct, incorrect_s1 = incorrect)

# Merge the summaries
merged_info <- full_join(info_check_by_case_input_question_strategy1, info_check_by_case_input_question_strategy2,
                         by = c("Input type", "report_model" = "Model_name"))

# Create plot data
plot_data <- merged_info %>%
  select(`Input type`, report_model, correct_s1, correct_s2) %>%
  pivot_longer(cols = c(correct_s1, correct_s2),
               names_to = "source",
               values_to = "accuracy") %>%
  mutate(source = recode(source,
                         correct_s1 = "Report Model",
                         correct_s2 = "Baseline"))

# Calculate percentage retention
pct_retain <- plot_data %>%
  pivot_wider(
    names_from = source,
    values_from = accuracy
  ) %>%
  mutate(
    pct_retain = `Report Model` / Baseline 
  )

# Set factor levels for Input type
pct_retain$`Input type` <- factor(pct_retain$`Input type`,
                                  levels = c("Code + Data", "Data", "Code + Graph  + Data", "Code + Graph", "Graph"))

# Calculate average retention by input type for horizontal lines
pct_retain_by_input_type <- pct_retain %>%
  group_by(`Input type`) %>%
  summarise(
    avg_pct_retain = mean(pct_retain, na.rm = TRUE)
  )

pct_retain_by_input_type$`Input type` <- factor(pct_retain_by_input_type$`Input type`,
                                                levels = c("Code + Data", "Data", "Code + Graph  + Data", "Code + Graph", "Graph"))

# Update model names
pct_retain <- pct_retain %>%
  mutate(report_model = recode(report_model,
                               "claude-3-7-sonnet-20250219" = "Claude 3.7",
                               "gemini-2.0-flash" = "Gemini 2.0",
                               "gpt-4o" = "GPT-4o",
                               "o1" = "o1"
  ))

# Create the plot
fig.3b=ggplot(pct_retain, aes(x = report_model, y = pct_retain * 100, fill = report_model)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.6) +
  facet_grid(~ `Input type`) +
  geom_text(
    aes(label = round(pct_retain * 100, 1)),  # scale if needed
    position = position_dodge(width = 0.75),
    vjust = -0.5,
    size = 3.5
  ) +
  labs(
    # title = "Model Accuracy Comparison: Report vs Baseline",
    x = "Model",
    y = "Percentage retain (%)",
    fill = "Source"
  ) +
  scale_fill_manual(values = c(
    "Claude 3.7" = "#EF6F6A",   # Claude
    "Gemini 2.0" = "#6388B4",   # Gemini
    "GPT-4o" = "#55AD89",       # ChatGPT 4o
    "o1" = "#64CDCC"            # o1
  )) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.major = element_line(size = 0.2),  # thinner major grid lines
        panel.grid.minor = element_line(size = 0.1)   ) +
  geom_hline(
    data = pct_retain_by_input_type,
    aes(yintercept = avg_pct_retain * 100),
    linetype = "dashed",
    color = "purple",
    linewidth = 0.8
  )


 
ggsave("Figure3B.pdf",plot=fig.3b, width = 30, height = 12 , units = "cm")
