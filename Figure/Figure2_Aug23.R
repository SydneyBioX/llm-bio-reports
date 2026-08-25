

library(dplyr)
library(tidyr)
library(stringr)
library(funkyheatmap)
url <- "https://docs.google.com/spreadsheets/d/1yFDcz5VEJg4iws9exABzG9eMF46wLGS38pWxalxQC-k/export?format=csv&gid=339476217"
mcq_data <- read.csv(url,check.names = F)
mcq_data[,"qid"]=paste0(mcq_data$CaseStudy_ID,"_",mcq_data$Question_ID)
url <- 'https://docs.google.com/spreadsheets/d/1yFDcz5VEJg4iws9exABzG9eMF46wLGS38pWxalxQC-k/export?format=csv&gid=652035055'
case_data_raw <- read.csv(url, stringsAsFactors = FALSE,check.names = F)

# Keep the first row as header and remove rows 2, 3, 4 (which become rows 1, 2, 3 after header)
case_data <- case_data_raw#[-c(1:3),]


strategy1_old=read.csv("data/APRIL15_MCQ_result_strategy1.csv",check.names = F)
strategy2_old=read.csv("data/APRIL16_MCQ_result_strategy2_cleaned.csv",check.names = F)

strategy1_new=read.csv("data/AUG11_MCQ_result_strategy1_merged_clean.csv",check.names = F)
strategy2_new=read.csv("data/AUG18_MCQ_result_strategy2_merged_clean.csv",check.names = F)

colnames(strategy1_old) <- gsub("\\."," ",colnames(strategy1_old))
colnames(strategy2_old) <- gsub("\\."," ",colnames(strategy2_old))
colnames(strategy1_new) <- gsub("\\."," ",colnames(strategy1_new))
colnames(strategy2_new) <- gsub("\\."," ",colnames(strategy2_new))

strategy1=bind_rows(strategy1_old,strategy1_new)
strategy2=bind_rows(strategy2_old,strategy2_new)

model_order=c("claude-3-7-sonnet-20250219","claude-opus-5",
              "gemini-2.0-flash","gemini-3.6-flash",
              "gpt-4o","o1-2024-12-17","gpt-5.6-sol")

evaluator_order=c("claude-3-7-sonnet-20250219","gemini-2.0-flash","gpt-4o",
                  "claude-opus-5","gpt-5.6-sol",
                  "gemini-3.6-flash","o1-2024-12-17")



strategy2$Info_check <- gsub("\\.", "", strategy2$Info_check)
strategy2$Info_check <- ifelse(strategy2$Info_check %in% c("Yes","No"),strategy2$Info_check,"Invalid")
strategy2$Model_name <- factor(strategy2$Model_name,levels=model_order)

strategy2$correct <- as.character(strategy2$Answer)==as.character(strategy2$Model_return)
strategy2[,"qid"]=paste0(strategy2$CaseStudy_ID,"_",strategy2$Question_ID)
case_data_clean <- case_data[ , !names(case_data) %in% setdiff(intersect(names(strategy2), names(case_data)), "Sample_ID")]
strategy2_case <- merge(strategy2, case_data_clean, by = "Sample_ID", all.x = TRUE)
mcq_data_clean <- mcq_data[ , !names(mcq_data) %in% setdiff(intersect(names(strategy2_case), names(mcq_data)), "qid")]
strategy2_full <- merge(strategy2_case, mcq_data_clean, by = "qid", all.x = TRUE)




strategy2_full <- strategy2_full |>
  mutate(`Information retrieval strategy` = recode(`Information retrieval strategy`,
                                                   "1. Fact-based (literal) retrieval → Remembering" = "1. Remembering",
                                                   "2. Logical or structural comprehension → Understanding" = "2. Understanding",
                                                   "3. Contextual use of information → Applying" = "3. Applying",
                                                   "4. Complex synthesis and meaning extraction → Analyzing" = "4. Analyzing",
                                                   "5. Evaluative and critical retrieval → Evaluative" = "5. Evaluating",
                                                   "6. Real-world synthesis and knowledge expansion → Creating" = "6. Creating"
  ))




strategy1$Info_check <- gsub("\\.", "", strategy1$Info_check)
strategy1$Info_check <- ifelse(strategy1$Info_check %in% c("Yes","No"),strategy1$Info_check,"Invalid")
strategy1$Model_name <- factor(strategy1$Model_name,levels=model_order)

strategy1$correct <- as.character(strategy1$Answer)==as.character(strategy1$Model_return)
strategy1[,"qid"]=paste0(strategy1$CaseStudy_ID,"_",strategy1$Question_ID)
case_data_clean <- case_data[ , !names(case_data) %in% setdiff(intersect(names(strategy1), names(case_data)), "Sample_ID")]
strategy1_case <- merge(strategy1, case_data_clean, by = "Sample_ID", all.x = TRUE)
mcq_data_clean <- mcq_data[ , !names(mcq_data) %in% setdiff(intersect(names(strategy1_case), names(mcq_data)), "qid")]
strategy1_full <- merge(strategy1_case, mcq_data_clean, by = "qid", all.x = TRUE)



strategy1_full <- strategy1_full |>
  mutate(`Information retrieval strategy` = recode(`Information retrieval strategy`,
                                                   "1. Fact-based (literal) retrieval → Remembering" = "1. Remembering",
                                                   "2. Logical or structural comprehension → Understanding" = "2. Understanding",
                                                   "3. Contextual use of information → Applying" = "3. Applying",
                                                   "4. Complex synthesis and meaning extraction → Analyzing" = "4. Analyzing",
                                                   "5. Evaluative and critical retrieval → Evaluative" = "5. Evaluating",
                                                   "6. Real-world synthesis and knowledge expansion → Creating" = "6. Creating"
  ))

strategy1_full[,"report_model"]=str_extract(strategy1_full$Report_name, "(?<=\\.)[^.]+(?:\\.[^.]+)*(?=\\.txt)")
strategy1_full$report_model <- factor(strategy1_full$report_model,levels=model_order)




wide_df.case_category <- strategy2_full |>
  group_by(Model_name, `Google Folder`) |>
  summarise(accuracy = mean(correct), .groups = "drop") |>
  pivot_wider(names_from = `Google Folder`, values_from = accuracy)


wide_df.retrieval_strategy <- strategy2_full |>
  group_by(Model_name, `Information retrieval strategy`) |>
  summarise(accuracy = mean(correct), .groups = "drop") |>
  pivot_wider(names_from = `Information retrieval strategy`, values_from = accuracy)


wide_df.Input_type <- strategy2_full |>
  group_by(Model_name, `Input type`) |>
  summarise(accuracy = mean(correct), .groups = "drop") |>
  pivot_wider(names_from = `Input type`, values_from = accuracy)


wide_df.overall <-  strategy2_full |>
  group_by(Model_name) |>
  summarise(overall_accuracy = mean(correct), .groups = "drop") 



wide_df.Info_check_percent_long <- strategy2_full |>
  group_by(Model_name, Info_check) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(Model_name) |>
  mutate(percent = n / sum(n)) |>
  ungroup() |>
  dplyr::rename(category = Info_check,
                value = percent)




info_check_list <- wide_df.Info_check_percent_long |>
  group_by(Model_name) |>
  summarise(info_check = list(setNames(value, category)))



list_of_dfs <- list(
  wide_df.overall,
  wide_df.case_category,
  wide_df.retrieval_strategy,
  wide_df.Input_type,
  info_check_list
)

final_df <- purrr::reduce(list_of_dfs, left_join, by = "Model_name")


colnames(final_df)[1]="id"
final_df <- final_df |>
  arrange(id)

column_info <- tribble(
  ~id,     ~group,         ~name,                      ~geom,        ~options,
  "id",    "",             "Model name",                         "text",       list(hjust = 0, width = 7),
  "overall_accuracy",   "overall",      "overall_accuracy",           "bar",        list(width=6),
  "DE",   "group1",      "Differential Expression",      "funkyrect",        lst(),
  "Pathway",  "group1",       "Pathway analysis",    "funkyrect",  lst(),
  "CCI",    "group1",       "CCI",         "funkyrect",  lst(),
  "SpatialSim",  "group1",       "SpatialSim",          "funkyrect",  lst(),
  "ClassifyR",    "group1",      "ClassifyR",        "funkyrect",  lst(),
  "Spatial",  "group1",       "Spatial",            "funkyrect",  lst(),
  "1. Remembering" ,    "group2",       "1. Remembering",                   "funkyrect",  lst(),
  "2. Understanding" ,    "group2",       "2. Understanding",             "funkyrect",  lst(),
  "3. Applying",  "group2",       "3. Applying",          "funkyrect",  lst(),
  "4. Analyzing",  "group2",       "4. Analyzing",            "funkyrect",  lst(),
  "5. Evaluating", "group2",       "5. Evaluating",            "funkyrect",  lst(),
  "6. Creating" , "group2",       "6. Creating",            "funkyrect",  lst(),
  "Code + Graph  + Data", "group3",       "Code + Graph  + Data",            "funkyrect",  lst(),
  "Code + Data", "group3",       "Code + Data",            "funkyrect",  lst(),
  "Code + Graph", "group3",      "Code + Graph",            "funkyrect",  lst(),
  "Data", "group3",       "Data",            "funkyrect",  lst(),
  "Graph", "group3",       "Graph",            "funkyrect",  lst(),
  "info_check" , "group4",       "Info check yes or no",            "pie",  list(width = 1.85)
)

column_info$palette <- c(NA, "average_accuracy", rep("Case", 6), rep("Input", 6), rep("retrieval", 5),rep("Info", 1))


column_groups <- tibble(
  Category = c("Average Accuracy","Case group", "retrieval strategy", "Input type", "Info"),
  group = c("overall","group1", "group2", "group3", "group4"),
  palette = c("average_accuracy", "Case", "Input", "retrieval","Info")
)


funkyp1=funky_heatmap(
  final_df,
  column_info = column_info,
  position_args = position_arguments(
    row_space = 0.4,
    expand_xmax = 4
  ),
  scale_column=F,
  column_groups = column_groups
)


funkyp1

plot_scale <- 1.6

wide_df.case_category <- strategy1_full |>
  group_by(Model_name, `Google Folder`,report_model) |>
  summarise(accuracy = mean(correct), .groups = "drop") |>
  pivot_wider(names_from = `Google Folder`, values_from = accuracy)


wide_df.retrieval_strategy <- strategy1_full |>
  group_by(Model_name, `Information retrieval strategy`,report_model) |>
  summarise(accuracy = mean(correct), .groups = "drop") |>
  pivot_wider(names_from = `Information retrieval strategy`, values_from = accuracy)


wide_df.Input_type <- strategy1_full |>
  group_by(Model_name, `Input type`,report_model) |>
  summarise(accuracy = mean(correct), .groups = "drop") |>
  pivot_wider(names_from = `Input type`, values_from = accuracy)


wide_df.overall <-  strategy1_full |>
  group_by(Model_name,report_model) |>
  summarise(overall_accuracy = mean(correct), .groups = "drop")

wide_df.Info_check_percent_long <- strategy1_full |>
  group_by(Model_name, Info_check,report_model) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(Model_name,report_model) |>
  mutate(percent = n / sum(n)) |>
  ungroup() |>
  dplyr::rename(Model_name = Model_name,
                report_model=report_model,
                category = Info_check,
                value = percent)

info_check_list <- wide_df.Info_check_percent_long |>
  group_by(Model_name,report_model) |>
  summarise(info_check = list(setNames(value, category)))


list_of_dfs <- list(
  wide_df.overall,
  wide_df.case_category,
  wide_df.retrieval_strategy,
  wide_df.Input_type,
  info_check_list
)

final_df <- purrr::reduce(list_of_dfs, left_join, by = c("Model_name","report_model"))

colnames(final_df)[1]="id"

final_df <- final_df |>
  arrange(report_model,factor(as.character(id),levels=evaluator_order))
final_df$id=paste0(final_df$id,"|",final_df$report_model)

column_info <- tribble(
  ~id,     ~group,         ~name,                      ~geom,        ~options,
  "id",    "",             "Model name",                         "text",       list(hjust = 0, width = 7),
  "overall_accuracy",   "overall",      "overall_accuracy",           "bar",        list(width=6),
  "DE",   "group1",      "Differential Expression",      "funkyrect",        lst(),
  "Pathway",  "group1",       "Pathway analysis",    "funkyrect",  lst(),
  "CCI",    "group1",       "CCI",         "funkyrect",  lst(),
  "SpatialSim",  "group1",       "SpatialSim",          "funkyrect",  lst(),
  "ClassifyR",    "group1",      "ClassifyR",        "funkyrect",  lst(),
  "Spatial",  "group1",       "Spatial",            "funkyrect",  lst(),
  "1. Remembering" ,    "group2",       "1. Remembering",                   "funkyrect",  lst(),
  "2. Understanding" ,    "group2",       "2. Understanding",             "funkyrect",  lst(),
  "3. Applying",  "group2",       "3. Applying",          "funkyrect",  lst(),
  "4. Analyzing",  "group2",       "4. Analyzing",            "funkyrect",  lst(),
  "5. Evaluating", "group2",       "5. Evaluating",            "funkyrect",  lst(),
  "6. Creating" , "group2",       "6. Creating",            "funkyrect",  lst(),
  "Code + Graph  + Data", "group3",       "Code + Graph  + Data",            "funkyrect",  lst(),
  "Code + Data", "group3",       "Code + Data",            "funkyrect",  lst(),
  "Code + Graph", "group3",      "Code + Graph",            "funkyrect",  lst(),
  "Data", "group3",       "Data",            "funkyrect",  lst(),
  "Graph", "group3",       "Graph",            "funkyrect",  lst(),
  "info_check" , "group4",       "Info check yes or no",            "pie",  list(width = 1.85)
)

column_info$palette <- c(NA, "average_accuracy", rep("Case", 6), rep("Input", 6), rep("retrieval", 5),rep("Info", 1))


column_groups <- tibble(
  Category = c("Average Accuracy","Case group", "retrieval strategy", "Input type", "Info"),
  group = c("overall","group1", "group2", "group3", "group4"),
  palette = c("average_accuracy", "Case", "Input", "retrieval","Info")
)

row_info <- final_df[,c("id","report_model")]
colnames(row_info)[2]="group"

row_groups <- row_info |>
  distinct(group) |>
  dplyr::rename(Group=group) |>
  mutate(Group=as.character(Group),
         group=Group)

row_order <- row_info |>
  pull(id)

final_df  <- final_df  |>
  mutate(id = factor(id, levels = row_order)) |>
  arrange(id)

row_info  <- row_info  |>
  mutate(id = factor(id, levels = row_order)) |>
  mutate(group=as.character(group)) |>
  arrange(id)

funkyp=funky_heatmap(
  final_df,
  column_info   = column_info,
  position_args = position_arguments(expand_xmax = 4),
  scale_column  = FALSE,
  row_groups    = row_groups,
  row_info      = row_info,
  column_groups = column_groups
)

funkyp



plot_width <- funkyp$width * plot_scale

ggplot2::ggsave("Figure2_Aug23_strategy2.pdf", funkyp1,
                width = plot_width+3,
                height = funkyp1$height * plot_scale)

ggplot2::ggsave("Figure2_Aug23_strategy1.pdf", funkyp,
                width = plot_width,
                height = funkyp$height * plot_scale)
