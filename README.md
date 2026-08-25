# Large Language Models for Accessible Reporting of Bioinformatics Analyses in Interdisciplinary Contexts 

<!--
**Authors:** Lijia Yu, Daniel Kim, Yue Cao, Matthew Wei Shun Shu, Maya Shen, Xiaoqi Liang, Jasmine Gu, Rojashree Jayakumar, Wenze Ding, Fei Yang, Xumou Zhang, Jinman Kim, Pengyi Yang, and Jean Yee Hwa Yang
-->

## Overview <a href="https://github.com/SydneyBioX/llm-bio-reports"><img src="https://i.imgur.com/c2EsglI.png" title="BioLLM report hex sticker" align="right" height="138" /></a>

This repository accompanies the manuscript *Large Language Models for Accessible Reporting of Bioinformatics Analyses in Interdisciplinary Contexts*. It contains the case studies, evaluation materials, model-generated reports, analysis code, and figure-generation resources used in the study.

The study evaluates the ability of large language models (LLMs) to communicate results from bioinformatics analyses to interdisciplinary audiences. The evaluation combines automated report generation and multiple-choice question (MCQ) tasks with human assessment of selected reports. The included case studies cover several common bioinformatics analysis settings, including differential expression, pathway analysis, cell–cell interaction analysis, classification, and spatial transcriptomics.

## Repository contents

| Path | Description |
| --- | --- |
| `Automated_evaluation_code/` | Jupyter notebooks used for report generation, MCQ answering, and baseline sensitivity analysis. |
| `Automated_evaluation_report/` | LLM-generated reports from the non-reasoning models evaluated in the study. |
| `Automated_evaluation_report_reasoning_model/` | LLM-generated reports from the reasoning models evaluated in the study. |
| `CaseStudies_and_MCQs.xlsx` | Case-study metadata and the MCQs used for automated evaluation. |
| `Casestudies/` | Bioinformatics case-study inputs and analytical reports used to evaluate the LLMs. |
| `data/` | Source and derived datasets associated with the case studies. |
| `Human_evaluation/` | Materials used for human evaluation, including questionnaires, selected reports, text inputs, and processed evaluation results. |
| `Figure/` | R, Quarto, and Jupyter source files, processed data, and outputs used to produce the manuscript figures and supplementary figures. |

## Evaluation workflow

The repository supports the principal stages of the study:

1. Bioinformatics results and supporting materials are assembled as case-study inputs in `Casestudies/`.
2. The notebooks in `Automated_evaluation_code/` are used to generate accessible reports and obtain model responses to the MCQs.
3. Generated reports are retained separately for non-reasoning and reasoning models.
4. Selected reports are assessed using the materials in `Human_evaluation/`.
5. Evaluation results are summarised and visualised using the scripts and processed data in `Figure/`.

## Reproducing the analyses

The main computational entry points are:

- `Automated_evaluation_code/Report_generator.ipynb` for LLM-based report generation;
- `Automated_evaluation_code/MCQ_responder.ipynb` for the MCQ evaluation;
- `Automated_evaluation_code/Sensitivity_analysis_baseline.ipynb` for baseline sensitivity analysis; and
- the scripts and notebooks in `Figure/` for manuscript figure generation.

These notebooks interact with externally hosted LLMs. To rerun them, users must configure access credentials for the relevant model providers and may need to update local input and output paths. Model availability, API behaviour, and generated outputs can change over time; therefore, newly generated results may not exactly reproduce the archived outputs in this repository. Consult each notebook or figure script for its specific inputs before execution.

## Data and output organisation

Case-study directories contain combinations of source data, rendered analytical reports, text representations, and figures supplied to the models. The `Automated_evaluation_report/` and `Automated_evaluation_report_reasoning_model/` directories preserve generated text outputs. Data used for plotting and summary analyses are located under `Figure/data/`, while human-assessment source files and processed responses are under `Human_evaluation/`.

Some files are retained in their original formats to preserve the exact study materials. Consequently, the repository includes a mixture of plain-text files, spreadsheets, R Markdown or Quarto documents, notebooks, images, PDFs, and serialised R objects.

## Citation

If you use the code, data, or evaluation materials from this repository, please cite:

> Yu L, Kim D, Cao Y, Shu MWS, Shen M, Liang X, Gu J, Jayakumar R, Ding W, Yang F, Zhang X, Kim J, Yang P, Yang JYH. *Large Language Models for Accessible Reporting of Bioinformatics Analyses in Interdisciplinary Contexts*. Manuscript.

Publication details and a persistent identifier will be added when available.

## Licence

No licence is currently specified for this repository. Unless a licence is added, reuse of the code, data, and other materials requires permission from the copyright holders.

## Contact

For questions about the study or repository, please contact the corresponding authors through the contact information provided in the manuscript.
