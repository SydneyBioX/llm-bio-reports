suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tidytext)
  library(stringr)
  library(tokenizers)
  library(ggplot2)
  library(patchwork)
})

# Make paths independent of the directory from which Rscript is called.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1])))
} else {
  normalizePath(getwd())
}
project_dir <- normalizePath(file.path(script_dir, "."))
input_dir <- file.path(project_dir, "data", "Report_output_20260828_repeats")
mcq_file <- file.path(project_dir, "data", "AUG25_MCQ_result_strategy1_5repeats.csv")
out_dir <- file.path(project_dir)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

model_pattern <- "claude-opus-5|gemini-3\\.6-flash|gpt-5\\.6-sol|o1-2024-12-17"
files <- list.files(input_dir, pattern = "\\.txt$", full.names = TRUE)
if (!length(files)) stop("No .txt files found in: ", input_dir)

file_info <- tibble(path = files, filename = basename(files)) |>
  extract(
    filename,
    into = c("case_id", "model", "run"),
    regex = paste0("^(.*)\\.((?:", model_pattern, "))\\.([0-9]+)\\.txt$"),
    remove = FALSE,
    convert = TRUE
  )

reports <- file_info |>
  mutate(text = vapply(path, function(x) paste(readLines(x, warn = FALSE), collapse = "\n"),
                       character(1)))

# Keep definitions identical to 02-token_analysis.R for direct comparability.
sentence_metrics <- reports |>
  dplyr::select(model, case_id, run, text) |>
  mutate(sentences = lapply(text, tokenizers::tokenize_sentences),
         sentences = lapply(sentences, unlist)) |>
  unnest(sentences) |>
  group_by(model, case_id, run) |>
  summarise(
    avg_sentence_length = mean(str_count(sentences, "\\S+")),
    total_sentences = n(),
    .groups = "drop"
  )

word_metrics <- reports |>
  dplyr::select(model, case_id, run, text) |>
  unnest_tokens(output = token, input = text) |>
  group_by(model, case_id, run) |>
  summarise(
    avg_word_length = mean(nchar(token)),
    total_words = n(),
    unique_tokens = n_distinct(token),
    .groups = "drop"
  )

case_run_metrics <- sentence_metrics |>
  inner_join(word_metrics, by = c("model", "case_id", "run")) |>
  arrange(model, case_id, run)

expected_n <- n_distinct(case_run_metrics$model) *
  n_distinct(case_run_metrics$case_id) * n_distinct(case_run_metrics$run)

metric_labels <- c(
  unique_tokens = "Unique words",
  total_words = "Total words",
  avg_word_length = "Average word length",
  total_sentences = "Total sentences",
  avg_sentence_length = "Average sentence length"
)
metric_order <- names(metric_labels)

metrics_long <- case_run_metrics |>
  pivot_longer(all_of(metric_order), names_to = "metric", values_to = "value") |>
  mutate(metric = factor(metric, levels = metric_order, labels = unname(metric_labels)))

# Model comparisons: average the same reports within each run, rank the four
# models, and quantify agreement of rankings using mean pairwise Spearman rho.
run_model_means <- metrics_long |>
  group_by(run, model, metric) |>
  summarise(mean_value = mean(value), .groups = "drop") |>
  group_by(run, metric) |>
  mutate(rank = rank(-mean_value, ties.method = "average")) |>
  ungroup()

rank_stability <- run_model_means |>
  dplyr::select(run, model, metric, rank) |>
  pivot_wider(names_from = run, values_from = rank, names_prefix = "run_") |>
  group_by(metric) |>
  group_modify(function(.x, .y) {
    rank_matrix <- as.matrix(select(.x, starts_with("run_")))
    cors <- cor(rank_matrix, method = "spearman", use = "pairwise.complete.obs")
    tibble(mean_pairwise_spearman_rho = mean(cors[upper.tri(cors)], na.rm = TRUE),
           minimum_pairwise_spearman_rho = min(cors[upper.tri(cors)], na.rm = TRUE))
  }) |>
  ungroup()

# Keep all 10 run-pair correlations so they can be printed directly.
pairwise_rank_correlations <- run_model_means |>
  dplyr::select(run, model, metric, rank) |>
  pivot_wider(names_from = run, values_from = rank, names_prefix = "run_") |>
  group_by(metric) |>
  group_modify(function(.x, .y) {
    rank_matrix <- as.matrix(select(.x, starts_with("run_")))
    run_pairs <- combn(colnames(rank_matrix), 2)
    tibble(
      run_pair = paste(run_pairs[1, ], run_pairs[2, ], sep = " vs "),
      spearman_rho = apply(run_pairs, 2, function(z) {
        cor(rank_matrix[, z[1]], rank_matrix[, z[2]], method = "spearman")
      })
    )
  }) |>
  ungroup()

model_levels <- c("claude-opus-5", "gemini-3.6-flash", "gpt-5.6-sol", "o1-2024-12-17")
model_labels <- c("Claude Opus 5", "Gemini 3.6 Flash", "GPT-5.6", "o1")
model_colours <- c("#EF6F6A", "#6388B4", "#55AD89", "#E6B655")
metrics_long <- metrics_long |>
  mutate(model = factor(model, levels = model_levels, labels = model_labels),
         run = factor(run))

# Every dot is shown. The two reports belong to the same case study and are
# pooled; horizontal jitter separates their five generations. The black diamond
# is the model mean across both reports and all five runs.
set.seed(20260828)
p <- ggplot(metrics_long, aes(model, value, colour = model)) +
  geom_boxplot(width = 0.55, outlier.shape = NA, alpha = 0.10, linewidth = 0.45) +
  geom_point(aes(group = run), position = position_jitter(width = 0.15, height = 0),
             size = 2.0, alpha = 0.78) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3.0,
               fill = "white", colour = "black", stroke = 0.7) +
  facet_wrap(~metric, ncol = 5, scales = "free_y") +
  scale_colour_manual(values = model_colours, drop = FALSE) +
  labs(
    title = "Kidney pathway 1: text characteristics across five independent generations",
    subtitle = paste0(
      "Both reports are combined; each coloured point is one report-generation and ",
      "black diamonds show model means"
    ),
    x = NULL, y = NULL
  ) +
  theme_classic(base_size = 10) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 40, hjust = 1),
    strip.background = element_rect(fill = "grey94", colour = NA),
    strip.text = element_text(face = "bold", size = 9),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, colour = "grey30"),
    panel.spacing.x = grid::unit(0.8, "lines")
  )

# MCQ performance for the same repeated reports

mcq <- read.csv(mcq_file, check.names = FALSE, stringsAsFactors = FALSE) |>
  mutate(
    run = as.integer(Repeat_number),
    item = interaction(Report_name, Question_ID, drop = TRUE)
  )


# For each reasoning model and matched item, count how many of the five runs
# returned the modal answer, including categories with no observed items.
mcq_agreement <- mcq |>
  count(Model_name, item, Model_return, name = "answer_count") |>
  group_by(Model_name, item) |>
  summarise(same_answer_runs = max(answer_count), .groups = "drop") |>
  count(Model_name, same_answer_runs, name = "n_items") |>
  complete(Model_name, same_answer_runs = 1:5, fill = list(n_items = 0)) |>
  group_by(Model_name) |>
  mutate(
    percentage = 100 * n_items / sum(n_items),
    model = factor(Model_name, levels = model_levels, labels = model_labels),
    agreement = factor(paste0(same_answer_runs, "/5"), levels = paste0(5:1, "/5"))
  ) |>
  ungroup()

agreement_colours <- c("5/5" = "#1B9E77", "4/5" = "#66C2A5",
                       "3/5" = "#FC8D62", "2/5" = "#D73027", "1/5" = "#7F7F7F")

p_mcq <- ggplot(mcq_agreement, aes(model, percentage, fill = agreement)) +
  geom_col(position = position_dodge(width = 0.82), width = 0.74) +
  geom_text(aes(label = ifelse(percentage == 0, "", sprintf("%.1f%%", percentage))),
            position = position_dodge(width = 0.82), vjust = -0.3, size = 3.0) +
  scale_fill_manual(values = agreement_colours, drop = FALSE) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 90), breaks = seq(0, 80, 20),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = "Agreement of MCQ answers across five runs",
    subtitle = "Percentage of 24 matched MCQ items with each level of modal-answer agreement",
    x = NULL, y = "MCQ items", fill = "Same answer"
  ) +
  theme_classic(base_size = 10) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 25, hjust = 1),
    strip.background = element_rect(fill = "grey94", colour = NA),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, colour = "grey30")
  )


# The five faceted text metrics form panel A; MCQ agreement forms panel B.
panel_a <- p +
  labs(title = NULL, subtitle = NULL, tag = "A") +
  theme(plot.tag = element_text(face = "bold", size = 14))
panel_b <- p_mcq +
  labs(title = NULL, subtitle = NULL, tag = "B") +
  theme(plot.tag = element_text(face = "bold", size = 14))

combined_plot <- panel_a / panel_b +
  patchwork::plot_layout(heights = c(1, 1.1))
ggsave(file.path(out_dir, "SuppFigure10.pdf"), combined_plot,
       width = 15, height = 8)

# Print numerical results only; no CSV files are written.
cat("\nSpearman correlations for all 10 pairs of runs:\n")
print(pairwise_rank_correlations |>
        mutate(spearman_rho = round(spearman_rho, 2)), n = Inf)

cat("\nSpearman rank-stability summary:\n")
print(rank_stability |>
        mutate(across(where(is.numeric), ~ round(.x, 2))))

cat("\nMCQ modal-answer agreement across five runs:\n")
print(mcq_agreement |>
        select(model, agreement, n_items, percentage) |>
        mutate(percentage = round(percentage, 1)), n = Inf)

# Values quoted in the response to the reviewer.
stable_rho <- rank_stability |>
  filter(metric != "Total sentences") |>
  pull(mean_pairwise_spearman_rho) |>
  range()
sentence_rho <- rank_stability |>
  filter(metric == "Total sentences") |>
  pull(mean_pairwise_spearman_rho)
identical_range <- mcq_agreement |>
  filter(agreement == "5/5") |>
  pull(percentage) |>
  range()

cat(sprintf(
  paste0("\nReviewer-response summary:\n",
         "Mean pairwise Spearman rho for four text metrics: %.2f-%.2f\n",
         "Mean pairwise Spearman rho for total sentences: %.2f\n",
         "MCQ items with identical answers in 5/5 runs: %.1f-%.1f%%\n"),
  stable_rho[1], stable_rho[2], sentence_rho,
  identical_range[1], identical_range[2]
))
