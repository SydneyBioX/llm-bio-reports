# Required libraries
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(quanteda)
library(scales)
library(fmsb)

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

# Create spider plot data
final_df <- final_df2[,(colnames(final_df2) %in% c("model.x", "score", "question"))]
colnames(final_df) <- c("model", "question", "score")
final_df$score <- as.numeric(final_df$score)

score_wide <- final_df %>%
  pivot_wider(names_from = question,
              values_from = score,
              values_fn   = median,    
              values_fill = 0)       

max_row <- score_wide %>% summarise(across(-model, ~ 6)) %>%
  mutate(model = "max", .before = 1)
min_row <- score_wide %>% summarise(across(-model, ~ 0)) %>%
  mutate(model = "min", .before = 1)

radar_df <- bind_rows(max_row, min_row, score_wide)

row_order <- c("max", "min", "chatgpt4o", "chatgpto1", "claude", "gemini")
radar_df  <- radar_df %>% slice(match(row_order, model))

rownames(radar_df) <- radar_df$model
radar_df$model <- NULL        

outline <- c("#55AD89", "#64CDCC", "#EF6F6A", "#6388B4")   
fill    <- alpha(outline, 0.25)


pdf("Figure5A.pdf", width = 7, height = 6)
# Create the spider plot
radarchart(
  radar_df[, paste0("Q", 1:7)],  
  pcol  = outline,   
  pfcol = fill,
  plwd  = 2,
  plty  = 1,
  caxislabels = 0:5,         
  vlcex = 0.8,
  title = "Scores by statement",
  vlabels = paste0("S", 1:7)
)
legend("right", inset = 0,         
       legend = c("GPT-4o","o1","Claude 3.7","Gemini 2.0"),
       col = outline, lwd = 2, bty = "n", cex = 0.9)
dev.off()



