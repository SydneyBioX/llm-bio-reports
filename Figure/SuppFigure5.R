# Required libraries
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(quanteda)
library(ggplot2)

# Load and process text data
text_list <- readRDS("data/REPORTS FOR MANUAL EVALUATION/manual_reports.rds")
models <- strsplit(names(text_list), "_")
models <- unlist(lapply(models, function(x){x[[1]]}))
case_id <- strsplit(names(text_list), "_")
case_id <- unlist(lapply(case_id, function(x){x[[2]]}))

# Calculate basic text metrics
df <- data.frame(model=NA, case=NA, avg_sentence_length=NA, n_sentences=NA)

for(i in 1:length(text_list)){
  txt <- unlist(text_list[[i]])
  txt <- iconv(txt, from = "CP1252", to = "UTF-8", sub = "")
  sentences <- purrr::map(txt, tokenizers::tokenize_sentences)
  sentences <- purrr::map(sentences, unlist)
  sentences <- unlist(sentences)
  
  avg_sentence_length <- mean(str_count(sentences, "\\S+"))
  total_sentences <- length(sentences)
  
  tmp_df <- data.frame(model=models[i], case=case_id[i], 
                       avg_sentence_length=avg_sentence_length,
                       n_sentences=total_sentences)
  df <- rbind(df, tmp_df)
}

# Calculate word-level metrics
df2 <- data.frame(model=NA, avg_word_length = NA, total_words = NA, 
                  unique_tokens = NA, ttr = NA)
for(i in 1:length(text_list)){
  txt <- data.frame(text=unlist(text_list[[i]]))
  tokens <- unnest_tokens(txt, output = token, input = text)
  avg_word_length <- mean(nchar(tokens$token))
  total_words <- nrow(tokens)
  unique_tokens <- n_distinct(tokens$token)
  ttr <- unique_tokens / sqrt(2*total_words)
  
  tmp_df <- data.frame(model=models[i], avg_word_length = avg_word_length,
                       total_words = total_words, unique_tokens = unique_tokens, ttr = ttr)
  df2 <- rbind(df2, tmp_df)
}

final_df <- cbind(df, df2[,-1])
final_df2 <- final_df[-1,]

# Calculate readability metrics
reports <- data.frame(filename = names(text_list), text = unlist(text_list))
corp <- corpus(reports$text)
readability_metrics <- quanteda.textstats::textstat_readability(corp, measure = c("Dale.Chall.PSK","Dale.Chall","Flesch","Flesch.PSK","Flesch.Kincaid","FOG","SMOG")) 
readability_results <- readability_metrics %>%
  as_tibble() %>%
  mutate(model = reports$model, case_id = reports$case_id)

final_df2 <- cbind(final_df2, readability_results[,-1])

# Load and process manual evaluation results
m_results <- readRDS("data/manual_eval_results.rds")
colnames(m_results) <- tolower(colnames(m_results))
results <- strsplit(as.character(m_results$report), " ")
results <- lapply(results, function(x){tolower(x[[1]])})
m_results$report <- unlist(results)

# Clean model names
clean_vec <- gsub("chatGPT 4o",  "chatgpt4o",  m_results$model, ignore.case = TRUE)
clean_vec <- gsub("chatGPT o1",  "chatgpto1",  clean_vec, ignore.case = TRUE)
clean_vec <- gsub("Claude 3\\.7", "claude",     clean_vec, ignore.case = TRUE)
clean_vec <- gsub("Gemini 2\\.0", "gemini",     clean_vec, ignore.case = TRUE)
m_results$model <- clean_vec
m_results$unique_id <- paste0(m_results$model, "_", m_results$report)

# Process case names and merge
case <- final_df2$case
case <- strsplit(case, ".txt")
case <- lapply(case, function(x){tolower(x[[1]])})
final_df2$case <- unlist(case)
final_df2$unique_id <- paste0(final_df2$model, "_", final_df2$case)

final_df2 <- merge(final_df2, m_results, by="unique_id")
final_df2$score <- ordered(final_df2$score, levels = c(0, 1, 2, 3, 4, 5))

# Create the plot
# Flesch-Kincaid (Stratified by model)
final_df2 <- final_df2 %>%
  rename(Model = model.x)

final_df2$Model <- recode(final_df2$Model,
                          "gemini"    = "Gemini 2.0",
                          "claude"    = "Claude 3.7",
                          "chatgpt4o" = "GPT-4o",
                          "chatgpto1" = "o1"
)


sup.fig5=ggplot(final_df2, aes(x = score, y = Flesch.Kincaid, color = Model, group = Model)) + 
  stat_summary(fun = mean, geom = "line", size = 0.5) +
  stat_summary(fun = mean, geom = "point", size = 1) +
  facet_wrap(~category) + 
  labs(x = "\nScore", y = "Flesch-Kincaid Grade Level\n") +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black"))

ggsave("SuppFigure5.pdf" , plot=sup.fig5,width = 17, height  =10 , units = "cm")

