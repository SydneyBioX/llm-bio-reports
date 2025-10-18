
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
 

# Reports (Automated evaluation strategy)


case_df <- read.csv("data/token_analysis/automatic_case_level_metrics.csv", row.names=1)

model_df <- read.csv("data/token_analysis/automatic_summary_metrics_by_model.csv", row.names=1)
 

df_long <- case_df %>%                    
  pivot_longer(
    cols = c(-model, -case_id),                    
    names_to  = "metric",             
    values_to = "value"               
  )
df_long <- df_long[!(df_long$metric=="ttr"),]
df_long$metric <- factor(df_long$metric, levels=c("avg_word_length", "total_words", "unique_tokens", "avg_sentence_length", "total_sentences"))
model_colors <- c("#EF6F6A", "#6388B4", "#55AD89")
 

## avg_word_length

 
tmp <- df_long[df_long$metric == "avg_word_length",]
 
p_avg_word_length <- ggplot(tmp, aes(x=model, y=value, fill=model)) + 
  geom_boxplot() +
  scale_fill_manual(values=model_colors) +
  theme(panel.background=element_blank(),
        axis.line=element_line(color="black"),
        axis.text.x=element_text(angle=45, hjust=1)) +
  labs(y="avg_word_length") + ggtitle("Average word length")

 

## total_words

 
tmp <- df_long[df_long$metric == "total_words",]
 
p_number_words <- ggplot(tmp, aes(x=model, y=value, fill=model)) + 
  geom_boxplot() +
  scale_fill_manual(values=model_colors) +
  theme(panel.background=element_blank(),
        axis.line=element_line(color="black"),
        axis.text.x=element_text(angle=45, hjust=1)) +
  labs(y="total_words") + ggtitle("# words")

 

## unique_tokens

 
tmp <- df_long[df_long$metric == "unique_tokens",]
 
p_unique_word <- ggplot(tmp, aes(x=model, y=value, fill=model)) + 
  geom_boxplot() +
  scale_fill_manual(values=model_colors) +
  theme(panel.background=element_blank(),
        axis.line=element_line(color="black"),
        axis.text.x=element_text(angle=45, hjust=1)) +
  labs(y="unique_tokens") + ggtitle("# unique words")

 

## avg_sentence_length

 
tmp <- df_long[df_long$metric == "avg_sentence_length",]
 
p_avg_sentence_length <- ggplot(tmp, aes(x=model, y=value, fill=model)) + 
  geom_boxplot() +
  scale_fill_manual(values=model_colors) +
  theme(panel.background=element_blank(),
        axis.line=element_line(color="black"),
        axis.text.x=element_text(angle=45, hjust=1)) +
  labs(y="avg_sentence_length") + ggtitle("Average sentence length")

 


## total_sentences

 
tmp <- df_long[df_long$metric == "total_sentences",]
 
p_number_sentence <- ggplot(tmp, aes(x=model, y=value, fill=model)) + 
  geom_boxplot() +
  scale_fill_manual(values=model_colors) +
  theme(panel.background=element_blank(),
        axis.line=element_line(color="black"),
        axis.text.x=element_text(angle=45, hjust=1)) +
  labs(y="total_sentences") + ggtitle("# sentences")

 

 
# Theme for removing x tick labels
remove_x_ticks <- theme(
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank()
)

plots <- lapply(
  list(p_unique_word, p_number_words, p_avg_word_length, 
       p_number_sentence, p_avg_sentence_length),
  function(p) p + remove_x_ticks
)

ggarrange( plotlist = plots,
           ncol = 5 , nrow = 1,
           common.legend = T)

 






# Reports (Human evaluation strategy)

 
human <- readRDS("data/token_analysis/human_summary_metrics.rds")

human <- human[,colnames(human) %in% c("model.x", 
                                       "avg_sentence_length",
                                       "n_sentences",
                                       "avg_word_length",
                                       "total_words",
                                       "unique_tokens")]

model_colors <- c("#EF6F6A", "#6388B4", "#55AD89", "#64CDCC")
df_long <- human %>%                    
  pivot_longer(
    cols = -model.x,                    
    names_to  = "metric",             
    values_to = "value"               
  )
df_long <- df_long[!duplicated(df_long),]
df_long$metric <- factor(df_long$metric, levels=c("avg_word_length", "total_words", "unique_tokens", "avg_sentence_length", "n_sentences"))
df_long$model.x <- factor(df_long$model.x, levels=c("claude", "gemini", "chatgpt4o", "chatgpto1"))
 


## avg_word_length
 
tmp <- df_long[df_long$metric=="avg_word_length",]
 
p_avg_word_length <- ggplot(tmp, aes(x = model.x, y = value, color = model.x)) +
  scale_color_manual(values=model_colors) +
  geom_point(size = 5) +          # individual points
  stat_summary(fun = mean, geom = "crossbar", width = 0.3, size=1.2, 
               color = "black", fatten = 1.5) +                # mean bar
  labs(y = "avg_word_length") +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black")) + 
  ggtitle("Average word length")

 
## total_words
 
tmp <- df_long[df_long$metric=="total_words",]
 
p_number_words <-  ggplot(tmp, aes(x = model.x, y = value, color = model.x)) +
  scale_color_manual(values=model_colors) +
  geom_point(size = 5) +          # individual points
  stat_summary(fun = mean, geom = "crossbar", width = 0.3, size=1.2, 
               color = "black", fatten = 1.5) +                # mean bar
  labs(y = "total_words") +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black")) + 
  ggtitle("# words")

 


## unique_tokens

 
tmp <- df_long[df_long$metric=="unique_tokens",]

p_unique_word <- ggplot(tmp, aes(x = model.x, y = value, color = model.x)) +
  scale_color_manual(values=model_colors) +
  geom_point(size = 5) +          # individual points
  stat_summary(fun = mean, geom = "crossbar", width = 0.3, size=1.2, 
               color = "black", fatten = 1.5) +                # mean bar
  labs(y = "unique_tokens") +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black")) + ggtitle("# unique words")
 



## avg_sentence_length

 
tmp <- df_long[df_long$metric=="avg_sentence_length",]
 
p_avg_sentence_length <- ggplot(tmp, aes(x = model.x, y = value, color = model.x)) +
  scale_color_manual(values=model_colors) +
  geom_point(size = 5) +          # individual points
  stat_summary(fun = mean, geom = "crossbar", width = 0.3, size=1.2, 
               color = "black", fatten = 1.5) +                # mean bar
  labs(y = "avg_sentence_length") +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black")) + 
  ggtitle("Average sentence length")
 



## n_sentences

 
tmp <- df_long[df_long$metric=="n_sentences",]

p_number_sentence <-  ggplot(tmp, aes(x = model.x, y = value, color = model.x)) +
  scale_color_manual(values=model_colors) +
  geom_point(size = 5) +          # individual points
  stat_summary(fun = mean, geom = "crossbar", width = 0.3, size=1.2, 
               color = "black", fatten = 1.5) +                # mean bar
  labs(y = "n_sentences") +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black")) + ggtitle("# sentences")

 
# Theme for removing x tick labels
remove_x_ticks <- theme(
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank()
)

plots <- lapply(
  list(p_unique_word, p_number_words, p_avg_word_length, 
       p_number_sentence, p_avg_sentence_length),
  function(p) p + remove_x_ticks
)

ggarrange( plotlist = plots,
           ncol = 5 , nrow = 1,
           common.legend = T)
 




