# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(quanteda)
library(quanteda.textstats)

# Read and process data
url <- "https://docs.google.com/spreadsheets/d/1yFDcz5VEJg4iws9exABzG9eMF46wLGS38pWxalxQC-k/export?format=csv&gid=339476217"
mcq_data <- read.csv(url, check.names = F)
mcq_data[, "qid"] <- paste0(mcq_data$CaseStudy_ID, "_", mcq_data$Question_ID)

url <- 'https://docs.google.com/spreadsheets/d/1yFDcz5VEJg4iws9exABzG9eMF46wLGS38pWxalxQC-k/export?format=csv&gid=652035055'
case_data_raw <- read.csv(url, stringsAsFactors = FALSE, check.names = F)
case_data <- case_data_raw

# Read strategy data files
strategy1_old <- read.csv("data/APRIL15_MCQ_result_strategy1.csv", check.names = F)
strategy1_new <- read.csv("../../20260820-new_MCQ_analysis-LY/out/AUG11_MCQ_result_strategy1_merged_clean.csv", check.names = F)

colnames(strategy1_old) <- gsub("\\.", " ", colnames(strategy1_old))
colnames(strategy1_new) <- gsub("\\.", " ", colnames(strategy1_new))

strategy1 <- bind_rows(strategy1_old, strategy1_new)

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

# Read and process report texts
txt_list_old <- readRDS("data/LLM_300_reports.rds")
txt_list_new <- readRDS("../../20260820-new_MCQ_analysis-LY/out/LLM_new468_reports.rds")
txt_list <- c(txt_list_old, txt_list_new)

# Create reports dataframe
reports <- tibble(
  filename = names(txt_list),
  text = unlist(txt_list)
) |>
  mutate(model = str_extract(filename, "(claude-3-7-sonnet-20250219|claude-opus-5|gemini-2\\.0-flash|gemini-3\\.6-flash|gpt-4o|o1-2024-12-17|gpt-5\\.6-sol)"),
         case_id = str_remove(filename, "\\.(claude-3-7-sonnet-20250219|claude-opus-5|gemini-2\\.0-flash|gemini-3\\.6-flash|gpt-4o|o1-2024-12-17|gpt-5\\.6-sol)(-updated)?\\.txt$")
  )

# Get readability statistics
corp <- corpus(reports$text)
readability_metrics <- quanteda.textstats::textstat_readability(corp, 
                                                                measure = c("Dale.Chall.PSK", "Dale.Chall", "Flesch", 
                                                                            "Flesch.PSK", "Flesch.Kincaid", "FOG", "SMOG")) 
readability_results <- readability_metrics |>
  as_tibble() |>
  mutate(model = reports$model, case_id = reports$case_id)

# Fix base names for joining
readability_results_fixed <- readability_results |>
  mutate(
    base_name = gsub("-updated\\.txt$", ".txt", document)  # Replace -updated.txt with .txt
  )

# Calculate accuracy by report
strategy1_accuracy <- strategy1_full |>
  group_by(Report_name, report_model) |>
  summarise(avg_accuracy = mean(correct, na.rm = TRUE), .groups = "drop")

# Join readability with accuracy data
readability_joined <- readability_results_fixed |>
  left_join(strategy1_accuracy, by = c("base_name" = "Report_name"))

# Test the report-level correlation across all old and new report generators
pearson_test_readability_accuracy <- cor.test(
  readability_joined$Dale.Chall,
  readability_joined$avg_accuracy,
  method = "pearson",
  use = "complete.obs"
)

cat("\nNumber of reports included:",
    sum(complete.cases(readability_joined$Dale.Chall,
                       readability_joined$avg_accuracy)), "\n")
cat("Pearson correlation between New Dale-Chall and MCQ accuracy:",
    unname(pearson_test_readability_accuracy$estimate), "\n")
print(pearson_test_readability_accuracy)

# Create accuracy categories
readability_joined$accuracy_type <- ifelse(readability_joined$avg_accuracy > 0.3,
                                           ifelse(readability_joined$avg_accuracy >= 0.7, "High accuracy", "Medium accuracy"),
                                           "Low accuracy")

readability_joined$accuracy_type <- factor(readability_joined$accuracy_type, 
                                           levels = c("Low accuracy", "Medium accuracy", "High accuracy"))

# Update model names
readability_joined <- readability_joined |>
  mutate(report_model = recode(report_model,
                               "claude-3-7-sonnet-20250219" = "Claude 3.7",
                               "claude-opus-5" = "Claude Opus 5",
                               "gemini-2.0-flash" = "Gemini 2.0",
                               "gemini-3.6-flash" = "Gemini 3.6",
                               "gpt-4o" = "GPT-4o",
                               "o1-2024-12-17" = "o1",
                               "gpt-5.6-sol" = "GPT-5.6"
  ))

readability_joined$report_model <- factor(readability_joined$report_model,
                                          levels = c("Claude 3.7", "Claude Opus 5",
                                                     "Gemini 2.0", "Gemini 3.6",
                                                     "GPT-4o", "o1", "GPT-5.6"))

# Create the plot
fig.3d=ggplot(readability_joined, aes(x = accuracy_type, y = Dale.Chall, fill = report_model)) +
  geom_boxplot(outliers = FALSE) +  
  scale_fill_manual(values = c(
    "Claude 3.7" = "#EF6F6A",
    "Claude Opus 5" = "#F4A09C",
    "Gemini 2.0" = "#6388B4",
    "Gemini 3.6" = "#93ACCB",
    "GPT-4o" = "#55AD89",
    "o1" = "#64CDCC",
    "GPT-5.6" = "#E6B655"
  )) +  
  labs(
    x = NULL,
    y = "Dale-Chall",
    fill = "Report Model"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(size = 0.2),  # thinner major grid lines
        panel.grid.minor = element_line(size = 0.1))



ggsave("Figure3D_Aug23.pdf",plot=fig.3d, width = 18, height = 12 , units = "cm")

