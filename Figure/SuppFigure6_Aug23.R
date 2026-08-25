# Load required libraries
library(ggplot2)
library(dplyr)
library(stringr)
library(tibble)
library(quanteda)
library(quanteda.textstats)
library(readr)
library(patchwork)



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

# Print average readability scores for each non-reasoning model
average_readability_non_reasoning <- readability_results %>%
  group_by(model) %>%
  summarise(
    Dale.Chall = mean(Dale.Chall, na.rm = TRUE),
    Flesch.Kincaid = mean(Flesch.Kincaid, na.rm = TRUE),
    .groups = "drop"
  )

print(average_readability_non_reasoning)

# Run and summarise a Pearson correlation test within each model
pearson_test_by_model <- function(data) {
  data %>%
    filter(complete.cases(Dale.Chall, Flesch.Kincaid)) %>%
    group_by(model) %>%
    group_modify(~ {
      test <- cor.test(.x$Dale.Chall, .x$Flesch.Kincaid,
                       method = "pearson")
      tibble(
        n = nrow(.x),
        correlation = unname(test$estimate),
        t_statistic = unname(test$statistic),
        df = unname(test$parameter),
        p_value = test$p.value,
        conf_low = test$conf.int[1],
        conf_high = test$conf.int[2]
      )
    }) %>%
    ungroup()
}

# Pearson tests for each model represented in sup.fig6a
pearson_tests_non_reasoning <- pearson_test_by_model(readability_results)
print(pearson_tests_non_reasoning)

# Create the scatter plot
sup.fig6a=ggplot(readability_results, aes(x = Dale.Chall, y = Flesch.Kincaid, color = model)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Dale-Chall vs. Flesch.Kincaid per non-reasoning model",
       x = "Dale-Chall",
       y = "Flesch.Kincaid",
       color = "Model")


# Read and process report texts
txt_list <- readRDS("../../20260820-new_MCQ_analysis-LY/out/LLM_new468_reports.rds")

# Create reports dataframe
reports <- tibble(
  filename = names(txt_list),
  text = unlist(txt_list)
) |>
  mutate(model = str_extract(filename, "(claude-opus-5|gemini-3\\.6-flash|gpt-5\\.6-sol|o1-2024-12-17)"),
         case_id = str_remove(filename, "\\.(claude-opus-5|gemini-3\\.6-flash|gpt-5\\.6-sol|o1-2024-12-17)(-updated)?\\.txt$")
  )

# Get readability statistics
corp <- corpus(reports$text)
readability_metrics <- quanteda.textstats::textstat_readability(corp, measure = c("Dale.Chall.PSK", "Dale.Chall", "Flesch", "Flesch.PSK", "Flesch.Kincaid", "FOG", "SMOG")) 
readability_results <- readability_metrics |>
  as_tibble() |>
  mutate(model = reports$model, case_id = reports$case_id)

# Update model names
readability_results <- readability_results |>
  mutate(model = recode(model,
                        "claude-opus-5" = "Claude Opus 5",
                        "gemini-3.6-flash" = "Gemini 3.6",
                        "gpt-5.6-sol" = "GPT-5.6",
                        "o1-2024-12-17" = "o1"
  ))

readability_results$model <- factor(readability_results$model,
                                    levels = c("Claude Opus 5", "GPT-5.6",
                                               "Gemini 3.6", "o1"))

# Print average readability scores for each reasoning model
average_readability_reasoning <- readability_results |>
  group_by(model) |>
  summarise(
    Dale.Chall = mean(Dale.Chall, na.rm = TRUE),
    Flesch.Kincaid = mean(Flesch.Kincaid, na.rm = TRUE),
    .groups = "drop"
  )

print(average_readability_reasoning)

# Pearson tests for each model represented in sup.fig6b
pearson_tests_reasoning <- pearson_test_by_model(readability_results)
print(pearson_tests_reasoning)

# Create the scatter plot
sup.fig6b=ggplot(readability_results, aes(x = Dale.Chall, y = Flesch.Kincaid, color = model)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Dale-Chall vs. Flesch.Kincaid per reasoning model ",
       x = "Dale-Chall",
       y = "Flesch.Kincaid",
       color = "Model")

fig.all <- (sup.fig6a + sup.fig6b) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")


ggsave("SuppFigure6_Aug23.pdf", plot=fig.all,width = 27, height = 12, units = "cm")
