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

# Read strategy2 data file
strategy2 <- read.csv("data/APRIL16_MCQ_result_strategy2_cleaned.csv", check.names = F)

# Process strategy2 data
strategy2$Info_check <- gsub("\\.", "", strategy2$Info_check)
strategy2$correct <- as.character(strategy2$Answer) == as.character(strategy2$Model_return)
strategy2[, "qid"] <- paste0(strategy2$CaseStudy_ID, "_", strategy2$Question_ID)

# Merge strategy2 with case data
case_data_clean <- case_data[, !names(case_data) %in% setdiff(intersect(names(strategy2), names(case_data)), "Sample_ID")]
strategy2_case <- merge(strategy2, case_data_clean, by = "Sample_ID", all.x = TRUE)
mcq_data_clean <- mcq_data[, !names(mcq_data) %in% setdiff(intersect(names(strategy2_case), names(mcq_data)), "qid")]
strategy2_full <- merge(strategy2_case, mcq_data_clean, by = "qid", all.x = TRUE)

# Recode Information retrieval strategy labels
strategy2_full <- strategy2_full %>%
  mutate(`Information retrieval strategy` = recode(`Information retrieval strategy`,
                                                   "1. Fact-based (literal) retrieval → Remembering" = "1. Remembering",
                                                   "2. Logical or structural comprehension → Understanding" = "2. Understanding",
                                                   "3. Contextual use of information → Applying" = "3. Applying",
                                                   "4. Complex synthesis and meaning extraction → Analyzing" = "4. Analyzing",
                                                   "5. Evaluative and critical retrieval → Evaluative" = "5. Evaluating",
                                                   "6. Real-world synthesis and knowledge expansion → Creating" = "6. Creating"
  ))

# Define target input types
target_input_types <- c("Code + Data", "Code + Graph")

# Start from strategy2_full, calculate IsCorrect, then filter
filtered <- strategy2_full %>%
  mutate(IsCorrect = Answer == Model_return) %>%
  filter(`Input type` %in% target_input_types)

# Find CaseStudy_ID and Question_ID pairs with both input types
valid_combos <- filtered %>%
  group_by(CaseStudy_ID, Question_ID) %>%
  summarize(n_types = n_distinct(`Input type`), .groups = "drop") %>%
  filter(n_types == 2)

# Subset to only those rows and select relevant columns
final_subset <- filtered %>%
  semi_join(valid_combos, by = c("CaseStudy_ID", "Question_ID")) %>%
  dplyr::select(CaseStudy_ID, Question_ID, Model_name, `Input type`, IsCorrect, `Information retrieval strategy`)

# Pivot to wide format
df_wide <- final_subset %>%
  pivot_wider(
    names_from = `Input type`,
    values_from = IsCorrect,
    names_prefix = "Correct_"
  )

# Identify differential performance cases (only one input type correct)
df_diff <- df_wide %>%
  mutate(Result = case_when(
    `Correct_Code + Data` == TRUE & `Correct_Code + Graph` == FALSE ~ "Code + Data (Correct)",
    `Correct_Code + Data` == FALSE & `Correct_Code + Graph` == TRUE ~ "Code + Graph (Correct)",
    TRUE ~ "Other"
  )) %>%
  filter(Result %in% c("Code + Data (Correct)", "Code + Graph (Correct)"))

# Add Bloom category info back (from original table)
df_diff <- df_diff %>%
  left_join(
    filtered %>%
      dplyr::select(CaseStudy_ID, Question_ID, `Information retrieval strategy`) %>%
      distinct(),
    by = c("CaseStudy_ID", "Question_ID", "Information retrieval strategy")
  )

# Create the plot
sup.fig3=ggplot(df_diff, aes(x = `Information retrieval strategy`, fill = Result)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Differential Performance: One Input Type Correct (baseline)",
    x = "Information Retrieval Strategy",
    y = "Number of Differential Questions",
    fill = "Superior Input Type"
  ) +
  theme_minimal() +
  coord_flip()



ggsave("SuppFigure3.pdf",plot=sup.fig3, width = 17, height = 12, units = "cm")
