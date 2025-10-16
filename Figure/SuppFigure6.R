 
library(dplyr)
library(RColorBrewer)
library(tidyr)
library(ggplot2)
library(ggpubr)
 
raw_data <- read.csv("data/preprocessed_manual_eval_results.csv", row.names=1)
raw_data$Report <- ifelse(raw_data$Report=="Annotation (Lijia)", "Classification (Lijia)", raw_data$Report)
raw_data$What.is.your.discipline. <- factor(raw_data$What.is.your.discipline.)
raw_data$Report <- factor(raw_data$Report)
raw_data$Model <- factor(raw_data$Model)
raw_data$First.name <- factor(raw_data$First.name)
data <- raw_data[,c("Model", "Report", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7")]
reports <- strsplit(as.character(data$Report), " ")
reports <- unlist(lapply(reports, function(x){x[1]}))
data$Report <- reports
 
df_long <- data %>%
  pivot_longer(
    cols      = starts_with("Q"),     
    names_to  = "Question",
    values_to = "Score"
  )


freq_df <- data.frame(with(df_long, table(Model, Report, Score)))
colnames(freq_df) <- c("Model", "Report", "Score", "Frequency")
freq_df$Category <- as.factor(freq_df$Report)
 
tmp <- freq_df[freq_df$Report=="CCI",]
tmp$Score <- factor(tmp$Score, levels=unique(tmp$Score))


CCI <- ggplot(tmp, aes(x = Score, y = Frequency, fill=Score)) +
  geom_bar(stat="identity") +
  facet_grid(~Model) +
  labs(title="CCI", x = "\nScore", y = "Frequency\n") +
  scale_fill_manual(values = c("#d1d1d1","#e8cfc5", "#e3b29f", "#db8969", "#db6d42", "#d64106")) +
  theme(panel.background=element_blank(),
        legend.position="none",
        axis.line=element_line(color="black")) + 
  ggtitle("Cell-Cell Interaction(CCI) Analysis")



tmp <- freq_df[freq_df$Report=="Pathway",]
tmp$Score <- factor(tmp$Score, levels=unique(tmp$Score))

pathway <- ggplot(tmp, aes(x = Score, y = Frequency, fill=Score)) +
  geom_bar(stat="identity") +
  facet_grid(~Model) +
  labs(title="Pathway", x = "\nScore", y = "Frequency\n") +
  scale_fill_manual(values = c("#d1d1d1", "#dae5eb", "#a1bccc", "#76adcc", "#4195c4", "#097bba")) +
  theme(panel.background=element_blank(),
        legend.position="none",
        axis.line=element_line(color="black"))+ 
  ggtitle("Classification Analysis")


tmp <- freq_df[freq_df$Report=="Classification",]
tmp$Score <- factor(tmp$Score, levels=unique(tmp$Score))

classification <- ggplot(tmp, aes(x = Score, y = Frequency, fill=Score)) +
  geom_bar(stat="identity") +
  facet_grid(~Model) +
  labs(title="Classification", x = "\nScore", y = "Frequency\n") +
  scale_fill_manual(values = c("#d1d1d1", "#c5e3dd", "#97ded0", "#60d6bf", "#30c7a9", "#04b390")) +
  theme(panel.background=element_blank(),
        legend.position="none",
        axis.line=element_line(color="black"))+ 
  ggtitle("Pathway Analysis")



ggarrange(plotlist = list(CCI, classification, pathway) , 
          nrow = 3, ncol = 1)



ggsave("manual_report_barplot.pdf" , height = 16, width = 15, units = "cm")

 
