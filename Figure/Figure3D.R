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
strategy1 <- read.csv("data/APRIL15_MCQ_result_strategy1.csv", check.names = F)

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

# Read and process report texts
txt_list <- readRDS("data/LLM_300_reports.rds")

# Create reports dataframe
reports <- tibble(
  filename = names(txt_list),
  text = unlist(txt_list)
) %>%
  mutate(model = str_extract(filename, "(gemini-2\\.0-flash|gpt-4o|claude-3-7-sonnet-20250219)"),
         case_id = str_remove(filename, "\\.(gemini-2\\.0-flash|gpt-4o|claude-3-7-sonnet-20250219)(-updated)?\\.txt$")
  )

# Get readability statistics
corp <- corpus(reports$text)
readability_metrics <- quanteda.textstats::textstat_readability(corp, measure = c("Dale.Chall.PSK", "Dale.Chall", "Flesch", "Flesch.PSK", "Flesch.Kincaid", "FOG", "SMOG")) 
readability_results <- readability_metrics %>%
  as_tibble() %>%
  mutate(model = reports$model, case_id = reports$case_id)

# Fix base names for joining
readability_results_fixed <- readability_results %>%
  mutate(
    base_name = gsub("-updated\\.txt$", ".txt", document)  # Replace -updated.txt with .txt
  )

# Calculate accuracy by report
strategy1_accuracy <- strategy1_full %>%
  group_by(Report_name, report_model) %>%
  summarise(avg_accuracy = mean(correct, na.rm = TRUE), .groups = "drop")

# Join readability with accuracy data
readability_joined <- readability_results_fixed %>%
  left_join(strategy1_accuracy, by = c("base_name" = "Report_name"))

# Create accuracy categories
readability_joined$accuracy_type <- ifelse(readability_joined$avg_accuracy > 0.3,
                                           ifelse(readability_joined$avg_accuracy >= 0.7, "High accuracy", "Medium accuracy"),
                                           "Low accuracy")

readability_joined$accuracy_type <- factor(readability_joined$accuracy_type, 
                                           levels = c("Low accuracy", "Medium accuracy", "High accuracy"))

# Update model names
readability_joined <- readability_joined %>%
  mutate(report_model = recode(report_model,
                               "claude-3-7-sonnet-20250219" = "Claude 3.7",
                               "gemini-2.0-flash" = "Gemini 2.0",
                               "gpt-4o" = "GPT-4o",
                               "o1" = "o1"
  ))

# Create the plot
ggplot(readability_joined, aes(x = accuracy_type, y = Dale.Chall, fill = report_model)) +
  geom_boxplot(outliers = FALSE) +  
  scale_fill_manual(values = c(
    "Claude 3.7" = "#EF6F6A",
    "Gemini 2.0" = "#6388B4",
    "GPT-4o" = "#55AD89",
    "o1" = "#64CDCC"
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



ggsave("Dale_Chall.pdf", width = 12, height = 12 , units = "cm")



