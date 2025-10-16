# Load required libraries
library(ggplot2)
library(dplyr)
library(stringr)
library(tibble)
library(quanteda)
library(quanteda.textstats)
library(readr)

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

# Update model names
readability_results <- readability_results %>%
  mutate(model = recode(model,
                        "claude-3-7-sonnet-20250219" = "Claude 3.7",
                        "gemini-2.0-flash" = "Gemini 2.0",
                        "gpt-4o" = "GPT-4o"
  ))

# Create the scatter plot
ggplot(readability_results, aes(x = Dale.Chall, y = Flesch.Kincaid, color = model)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Dale–Chall vs. Flesch.Kincaid per Model",
       x = "Dale–Chall",
       y = "Flesch.Kincaid",
       color = "Model")




ggsave("readability.pdf", width = 15, height = 12, units = "cm")
