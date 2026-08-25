# Load required libraries
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(stringr)
})


# Read and process data
url <- "https://docs.google.com/spreadsheets/d/1yFDcz5VEJg4iws9exABzG9eMF46wLGS38pWxalxQC-k/export?format=csv&gid=339476217"
mcq_data <- read.csv(url, check.names = F)
mcq_data[, "qid"] <- paste0(mcq_data$CaseStudy_ID, "_", mcq_data$Question_ID)

url <- "https://docs.google.com/spreadsheets/d/1yFDcz5VEJg4iws9exABzG9eMF46wLGS38pWxalxQC-k/export?format=csv&gid=652035055"
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

case_data_clean <- case_data[, !names(case_data) %in% setdiff(intersect(names(strategy2), names(case_data)), "Sample_ID")]
strategy2_case <- merge(strategy2, case_data_clean, by = "Sample_ID", all.x = TRUE)
mcq_data_clean <- mcq_data[, !names(mcq_data) %in% setdiff(intersect(names(strategy2_case), names(mcq_data)), "qid")]
strategy2_full <- merge(strategy2_case, mcq_data_clean, by = "qid", all.x = TRUE)


# Compare baseline and report model strategies
baseline <- strategy2_full |>
  select(qid, `Input type`, Model_name, Answer, Model_return, Info_check) |>
  rename(
    Model_return_baseline = Model_return,
    Info_check_baseline = Info_check
  )

report <- strategy1_full |>
  select(qid, `Input type`, Model_name, report_model, Model_return, Info_check) |>
  rename(
    Model_return_report = Model_return,
    Info_check_report = Info_check
  )

classified_df <- baseline |>
  inner_join(report, by = c("qid", "Input type", "Model_name")) |>
  mutate(Info_pair = paste0(Info_check_baseline, "/", Info_check_report))

accuracy_all <- classified_df |>
  group_by(report_model, Info_pair) |>
  summarise(
    n = n(),
    baseline_accuracy = mean(Model_return_baseline == Answer, na.rm = TRUE),
    model_accuracy = mean(Model_return_report == Answer, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(model = recode(report_model,
                        "claude-3-7-sonnet-20250219" = "Claude 3.7",
                        "claude-opus-5" = "Claude Opus 5",
                        "gemini-2.0-flash" = "Gemini 2.0",
                        "gemini-3.6-flash" = "Gemini 3.6",
                        "gpt-4o" = "GPT-4o",
                        "o1-2024-12-17" = "o1",
                        "gpt-5.6-sol" = "GPT-5.6"))

accuracy_all$Info_pair <- factor(accuracy_all$Info_pair,
                                 levels = c("Yes/Yes", "Yes/No", "No/Yes", "No/No"))

accuracy_all$model <- factor(accuracy_all$model,
                             levels = c("Claude 3.7", "Claude Opus 5",
                                        "Gemini 2.0", "Gemini 3.6",
                                        "GPT-4o", "o1", "GPT-5.6"))

accuracy_long <- accuracy_all |>
  pivot_longer(
    cols = c(baseline_accuracy, model_accuracy),
    names_to = "type",
    values_to = "accuracy"
  )

accuracy_long$type <- recode(accuracy_long$type,
                             baseline_accuracy = "Baseline",
                             model_accuracy = "Report Model")


# Create the plot
# fig.3a=ggplot(accuracy_long, aes(x = model, y = accuracy, fill = type)) +
#   scale_fill_manual(values = c(
#     "Baseline" = "#D7301F",
#     "Report Model" = "#2166AC"
#   )) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
#   geom_text(aes(label = round(accuracy, 2)),
#             position = position_dodge(width = 0.7),
#             vjust = -0.7, size = 3) +
#   facet_wrap(~ Info_pair, nrow = 1) +
#   labs(
#     x = "LLM Model",
#     y = "Accuracy",
#     fill = "Source"
#   ) +
#   scale_y_continuous(limits = c(0, 1.05)) +
#   theme_minimal(base_size = 13) +
#   theme(
#     strip.text = element_text(size = 12, face = "bold"),
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     panel.grid.major = element_line(linewidth = 0.2),
#     panel.grid.minor = element_line(linewidth = 0.1)
#   )

fig.3a <- ggplot(
  accuracy_long,
  aes(x = model, y = accuracy, fill = type)
) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.6
  ) +
  geom_text(
    aes(label = round(accuracy, 2)),
    position = position_dodge(width = 0.75),
    hjust = -0.15,
    size = 3
  ) +
  coord_flip() +
  facet_wrap(~Info_pair, ncol = 2) +
  scale_fill_manual(
    values = c(
      "Baseline" = "#D7301F",
      "Report Model" = "#2166AC"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 1.1),
    breaks = seq(0, 1, 0.2)
  ) +
  labs(
    x = "LLM Model",
    y = "Accuracy",
    fill = "Source"
  ) +
  theme_minimal(base_size = 13) +
  scale_x_discrete(limits = rev(model_order)) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("Figure3A_Aug23.pdf", plot = fig.3a, width = 30, height = 18, units = "cm")
