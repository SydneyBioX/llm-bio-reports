#!/usr/bin/env Rscript

# Repeatability of reasoning-model evaluation across five independent runs.
# Produces one single-panel figure plus machine-readable statistical summaries.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_dir <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else normalizePath(getwd())
project_dir <- normalizePath(file.path(script_dir, ".."))
input_file <- file.path(project_dir, "in", "AUG25_MCQ_result_strategy1_5repeats.csv")
out_dir <- file.path(project_dir, "out")

dat <- read.csv(input_file, check.names = FALSE, stringsAsFactors = FALSE) |>
  rename(model = Model_name, returned_answer = Model_return, run = Repeat_number) |>
  mutate(
    run = as.integer(run),
    item = interaction(Report_name, Question_ID, drop = TRUE),
    correct = as.character(Answer) == as.character(returned_answer)
  )
stopifnot(!anyNA(dat$correct))

accuracy_by_run <- dat |>
  group_by(model, run) |>
  summarise(n = n(), n_correct = sum(correct), accuracy = mean(correct), .groups = "drop")

# Cochran's Q: matched comparison of accuracy among runs, calculated per model.
cochran_q <- function(d) {
  wide <- d |>
    select(item, run, correct) |>
    pivot_wider(names_from = run, values_from = correct) |>
    arrange(item)
  Y <- as.matrix(wide[, -1]) * 1
  k <- ncol(Y); C <- colSums(Y); R <- rowSums(Y)
  denominator <- k * sum(R) - sum(R^2)
  Q <- if (denominator == 0) NA_real_ else
    (k - 1) * (k * sum(C^2) - sum(C)^2) / denominator
  tibble(cochran_q = Q, df = k - 1,
         p_value = ifelse(is.na(Q), NA_real_, pchisq(Q, k - 1, lower.tail = FALSE)))
}

model_summary <- dat |>
  group_by(model) |>
  group_modify(~ cochran_q(.x)) |>
  left_join(
    accuracy_by_run |>
      group_by(model) |>
      summarise(mean_accuracy = mean(accuracy), min_accuracy = min(accuracy),
                max_accuracy = max(accuracy), range_percentage_points = 100 * (max(accuracy) - min(accuracy)),
                .groups = "drop"),
    by = "model"
  )

# Rank stability: pairwise Spearman correlation of model accuracies across runs.
rank_wide <- accuracy_by_run |>
  select(model, run, accuracy) |>
  pivot_wider(names_from = run, values_from = accuracy) |>
  arrange(model)
rank_cor <- cor(as.matrix(rank_wide[, -1]), method = "spearman")
mean_rank_correlation <- mean(rank_cor[upper.tri(rank_cor)])
minimum_rank_correlation <- min(rank_cor[upper.tri(rank_cor)])

rank_summary <- tibble(
  statistic = c("mean_pairwise_spearman_rank_correlation", "minimum_pairwise_spearman_rank_correlation"),
  value = c(mean_rank_correlation, minimum_rank_correlation)
)

write.csv(accuracy_by_run, file.path(out_dir, "reasoning_repeatability_accuracy_by_run.csv"), row.names = FALSE)
write.csv(model_summary, file.path(out_dir, "reasoning_repeatability_model_statistics.csv"), row.names = FALSE)
write.csv(rank_summary, file.path(out_dir, "reasoning_repeatability_rank_statistics.csv"), row.names = FALSE)

# Count how many of the five runs returned the modal answer for each item.
agreement_by_model <- dat |>
  count(model, item, returned_answer, name = "answer_count") |>
  group_by(model, item) |>
  summarise(same_answer_runs = max(answer_count), .groups = "drop") |>
  count(model, same_answer_runs, name = "n_items") |>
  complete(model, same_answer_runs = 2:5, fill = list(n_items = 0)) |>
  group_by(model) |>
  mutate(percentage = 100 * n_items / sum(n_items),
         agreement = factor(paste0(same_answer_runs, "/5 runs"),
                            levels = paste0(5:2, "/5 runs"))) |>
  ungroup()
write.csv(agreement_by_model,
          file.path(out_dir, "reasoning_repeatability_answer_agreement.csv"), row.names = FALSE)

model_order <- c("claude-opus-5", "gpt-5.6-sol", "gemini-3.6-flash", "o1-2024-12-17")
agreement_by_model$model <- factor(agreement_by_model$model, levels = model_order)

p <- ggplot(agreement_by_model, aes(model, percentage, fill = agreement)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72) +
  geom_text(aes(label = ifelse(percentage == 0, "", paste0(round(percentage, 1), "%"))),
            position = position_dodge(width = 0.8), vjust = -0.35, size = 3.1) +
  scale_fill_manual(values = c("5/5 runs" = "#1B9E77", "4/5 runs" = "#66C2A5",
                               "3/5 runs" = "#FC8D62", "2/5 runs" = "#D73027"),
                    drop = FALSE) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 88),
                     expand = expansion(mult = c(0, 0))) +
  labs(
    title = "Agreement of model answers across five independent runs",
    subtitle = "Percentage of 24 matched items returning the same answer in 5/5, 4/5, 3/5, or 2/5 runs",
    x = "Reasoning model", y = "Percentage of matched items", fill = "Answer agreement"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(colour = "grey30"),
    axis.text.x = element_text(angle = 20, hjust = 1), legend.position = "right"
  )

ggsave(file.path(out_dir, "Figure_reasoning_model_repeatability_5runs.pdf"), p, width = 12, height = 5.5)
# ggsave(file.path(out_dir, "Figure_reasoning_model_repeatability_5runs.png"), p, width = 9, height = 5.5, dpi = 300)

print(accuracy_by_run)
print(model_summary)
print(rank_summary)
print(agreement_by_model)
