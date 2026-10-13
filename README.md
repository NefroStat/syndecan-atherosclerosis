# Serum Syndecan-1 and Low-Density Lipoprotein as Independent Predictors of Atherosclerosis

[![R Version](https://img.shields.io/badge/R-4.5.3-blue.svg)](https://cran.r-project.org/)
[![Status](https://img.shields.io/badge/Status-Under_Review-orange.svg)]()
<!-- DOI badge will be added here via Zenodo upon publication -->

## Overview
This repository contains the complete anonymized dataset, statistical analysis scripts, and reproducible visualization code for the exploratory cross-sectional study investigating the diagnostic capacity of serum Syndecan-1 (an endothelial glycocalyx degradation marker) and low-density lipoprotein (LDL) in statin-naive patients.

**Study Title:** Serum Syndecan-1 and low-density lipoprotein as independent predictors of atherosclerosis in statin-naive patients: an exploratory cross-sectional study.

## Repository Structure
The project is organized to ensure full computational reproducibility:

```text
├── data/
│   └── syndecan1.xlsx              # Fully anonymized clinical and biochemical dataset (n = 63)
├── scripts/
│   ├── 01_data_preparation.R       # Data import, cleaning, and variable standardization
│   ├── 02_descriptive_stats.R      # Table 1: Baseline characteristics (Mann-Whitney & Fisher exact tests)
│   ├── 03_correlation_matrix.R     # Figure 1: Spearman correlation matrix visualization
│   ├── 04_firth_regression.R       # Table 3 & Figure 2: Firth's penalized logistic regression & Forest Plot
│   ├── 05_roc_analysis.R           # Figure 3: ROC curves and DeLong's tests with FDR adjustment
│   └── 06_supplementary_data.R     # Comprehensive exact p-value tables and extended statistical output
├── results/                        # Automatically generated output folder for tables and high-res plots
└── README.md                       # Project documentation

Prerequisites and Environment
The analysis was performed using R version 4.5.3. To reproduce the results, the following R packages are required:

Data manipulation & formatting: dplyr, tidyr, readxl, openxlsx

Statistical modeling: rstatix, logistf, Hmisc, car

Diagnostic performance: pROC

Visualization: ggplot2, patchwork

You can install all required dependencies by running the following command in R:

R
install.packages(c("dplyr", "tidyr", "readxl", "openxlsx", "rstatix", "logistf", "Hmisc", "car", "pROC", "ggplot2", "patchwork"))
Usage Instructions
To reproduce the analysis and generate all figures/tables presented in the manuscript:

Clone this repository to your local machine.

Ensure the working directory is set to the project root.

Run the scripts in the scripts/ directory sequentially from 01 to 06.

All outputs (high-resolution PNG figures and Excel tables) will be automatically saved to the locally generated results/ folder.

Data Privacy and Ethics
The dataset (syndecan1.xlsx) has been strictly anonymized. All direct personal identifiers, including names, initials, exact dates of birth, and detailed residential information, have been permanently removed to comply with the Declaration of Helsinki and local ethical guidelines.

License
Code: The R scripts provided in this repository are licensed under the MIT License.

Data: The clinical dataset is licensed under a Creative Commons Attribution 4.0 International License (CC BY 4.0).

Citation
If you use the data or code in this repository, please cite our corresponding publication:

[Citation placeholder - Authors, Title, Journal, Year, DOI will be updated upon publication]

Contact
For any methodological inquiries or data access requests, please contact the corresponding author at: [Your Professional Email Address].