# Load required libraries
library(ggplot2)
library(tidyr)
library(patchwork)
library(dplyr)

# Read data
url <- "https://docs.google.com/spreadsheets/d/1yFDcz5VEJg4iws9exABzG9eMF46wLGS38pWxalxQC-k/export?format=csv&gid=339476217"
mcq_data <- read.csv(url, check.names = F)

url <- 'https://docs.google.com/spreadsheets/d/1yFDcz5VEJg4iws9exABzG9eMF46wLGS38pWxalxQC-k/export?format=csv&gid=652035055'
case_data_raw <- read.csv(url, stringsAsFactors = FALSE, check.names = F)
case_data <- case_data_raw

# Create frequency data for input types
frequency_input_type <- case_data %>%
  group_by(`Input type`, `Google Folder`) %>%
  summarise(frequency = n(), .groups = "drop") %>%
  complete(`Input type`, `Google Folder`, fill = list(frequency = NA))

# Create frequency data for questions
frequency_question <- mcq_data %>%
  group_by(`Information retrieval strategy`, `Google Folder`) %>%
  summarise(frequency = n(), .groups = "drop") %>%
  complete(`Google Folder`, `Information retrieval strategy`)

# Recode the Information retrieval strategy labels
frequency_question <- frequency_question %>%
  mutate(`Information retrieval strategy` = recode(`Information retrieval strategy`,
                                                   "1. Fact-based (literal) retrieval → Remembering" = "1. Remembering",
                                                   "2. Logical or structural comprehension → Understanding" = "2. Understanding",
                                                   "3. Contextual use of information → Applying" = "3. Applying",
                                                   "4. Complex synthesis and meaning extraction → Analyzing" = "4. Analyzing",
                                                   "5. Evaluative and critical retrieval → Evaluative" = "5. Evaluating",
                                                   "6. Real-world synthesis and knowledge expansion → Creating" = "6. Creating"
  ))

# Set factor levels for consistent ordering
frequency_input_type$`Google Folder` <- factor(frequency_input_type$`Google Folder`, 
                                               levels = c("DE", "Pathway", "CCI", "SpatialSim", "ClassifyR", "Spatial"))

frequency_question$`Google Folder` <- factor(frequency_question$`Google Folder`, 
                                             levels = c("DE", "Pathway", "CCI", "SpatialSim", "ClassifyR", "Spatial"))

# Create the plots
p1 <- ggplot(frequency_input_type, aes(x = `Input type`, 
                                       y = `Google Folder`, 
                                       fill = frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "lightpink", na.value = "grey90") +
  geom_text(aes(label = ifelse(is.na(frequency), "", frequency))) +
  labs(title = "Cases", fill = "Frequency") +
  theme_minimal() + 
  xlab(NULL) + ylab(NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
        axis.text.y = element_text(color = "black"))

p2 <- ggplot(frequency_question, aes(x = `Information retrieval strategy`, 
                                     y = `Google Folder`, 
                                     fill = frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "lightblue", na.value = "grey90") +
  geom_text(aes(label = ifelse(is.na(frequency), "", frequency))) +
  labs(title = "Questions", fill = "Frequency") +
  theme_minimal() + 
  xlab(NULL) + ylab(NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
        axis.text.y = element_text(color = "black"))


sup.f1=ggpubr::ggarrange( plotlist= list(p1, p2), nrow = 1, ncol = 2)

ggsave("SuppFigure1.pdf", plot=sup.f1,width = 18, height = 8, units = "cm")
