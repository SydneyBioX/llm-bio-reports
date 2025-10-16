
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)
library(pheatmap)
 

raw_data <- read.csv("data/preprocessed_manual_eval_results.csv", row.names=1)
raw_data$Report <- ifelse(raw_data$Report=="Annotation (Lijia)", "Classification (Lijia)", raw_data$Report)
raw_data$What.is.your.discipline. <- factor(raw_data$What.is.your.discipline.)
raw_data$Report <- factor(raw_data$Report)
raw_data$Model <- factor(raw_data$Model)
raw_data$First.name <- factor(raw_data$First.name)
data <- raw_data[,c("Model", "First.name", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7")]

 
 

# Panel c 

 
data <- raw_data[,c("Model", "Report", "First.name", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7")]

df_long <- data %>%
  pivot_longer(
    cols      = starts_with("Q"),     
    names_to  = "Question",
    values_to = "Score"
  )

group <- ifelse(df_long$Question %in% c("Q1", "Q2", "Q3"), "Factual consistency",
         ifelse(df_long$Question ==  "Q4",                   "Lack of harmfulness",
         ifelse(df_long$Question ==  "Q5",                   "Comprehensiveness",
         ifelse(df_long$Question %in% c("Q6", "Q7"),         "Coherence",
                                                          df_long$Question))))
df_long$Category <- group


freq_df <- data.frame(with(df_long, table(Model, Category, Score)))
colnames(freq_df) <- c("Model", "Category", "Score", "Frequency")
freq_df$Category <- as.factor(freq_df$Category)
 
tmp_freq <- freq_df[freq_df$Category=="Factual consistency",]
tmp_freq$Score <- factor(tmp_freq$Score, levels=unique(tmp_freq$Score))

factual <- ggplot(tmp_freq, aes(x = Score, y = Frequency, fill=Score)) +
  geom_bar(stat="identity") +
  facet_grid(~Model) +
  labs(title="Factual consistency", x = "\nScore", y = "Frequency\n") +
  scale_fill_manual(values = c("#d7d7d9", "#ccae72", "#cfa54e", "#cf9b2d", "#c78c0e", "#c97b36")) +
  theme(panel.background=element_blank(),
        axis.line=element_line(color="black"))
 
tmp_freq <- freq_df[freq_df$Category=="Lack of harmfulness",]
tmp_freq <- tmp_freq[!(tmp_freq$Score=="0"),]
tmp_freq$Score <- factor(tmp_freq$Score, levels=unique(tmp_freq$Score))

harmful <- ggplot(tmp_freq, aes(x = Score, y = Frequency, fill=Score)) +
  geom_bar(stat="identity") +
  facet_grid(~Model) +
  labs(title="Lack of harmfulness", x = "\nScore", y = "Frequency\n") +
  scale_fill_manual(values = c("#dae5eb", "#a1bccc", "#76adcc", "#4195c4", "#097bba")) +
  theme(panel.background=element_blank(),
        axis.line=element_line(color="black"))
 
tmp_freq <- freq_df[freq_df$Category=="Comprehensiveness",]
tmp_freq <- tmp_freq[!(tmp_freq$Score=="0"),]
tmp_freq$Score <- factor(tmp_freq$Score, levels=unique(tmp_freq$Score))

comprehensive <- ggplot(tmp_freq, aes(x = Score, y = Frequency, fill=Score)) +
  geom_bar(stat="identity") +
  facet_grid(~Model) +
  labs(title="Comprehensiveness", x = "\nScore", y = "Frequency\n") +
  scale_fill_manual(values = c("#e8cfc5", "#e3b29f", "#db8969", "#db6d42", "#d64106")) +
  theme(panel.background=element_blank(),
        axis.line=element_line(color="black"))
 

tmp_freq <- freq_df[freq_df$Category=="Coherence",]
tmp_freq <- tmp_freq[!(tmp_freq$Score=="0"),]
tmp_freq$Score <- factor(tmp_freq$Score, levels=unique(tmp_freq$Score))
coherence <- ggplot(tmp_freq, aes(x = Score, y = Frequency, fill=Score)) +
  geom_bar(stat="identity") +
  facet_grid(~Model) +
  labs(title="Coherence", x = "\nScore", y = "Frequency\n") +
  scale_fill_manual(values = c("#c5e3dd", "#97ded0", "#60d6bf", "#30c7a9", "#04b390")) +
  theme(panel.background=element_blank(),
        axis.line=element_line(color="black"))

ggarrange(plotlist = list( factual, comprehensive, harmful, coherence ), 
          ncol = 2 , nrow = 2)


# ggsave( "Figure5.pdf" , width = 25, height = 15, units = "cm" )
 
 
 