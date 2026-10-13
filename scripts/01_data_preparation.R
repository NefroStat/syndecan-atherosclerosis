# ==============================================================================
# Script 01: Data Import and Preparation
# ==============================================================================
# Install required packages if not present
required_packages <- c("readxl", "dplyr", "rstatix", "openxlsx", "logistf", 
                       "pROC", "car", "ggplot2", "patchwork", "tidyr", "Hmisc")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(readxl)
library(dplyr)

# 1. Create directory for results
if(!dir.exists("../results")) dir.create("../results")

# 2. Load dataset
data_path <- "../data/синдекан1.xlsx" 
df <- read_excel(data_path)

# 3. Clean and translate variables for international reproducibility
df_clean <- df %>% 
  filter(!is.na(ЛПНЩ), !is.na(`синдекан 1(ng/ml)`), !is.na(група)) %>%
  rename(
    Atherosclerosis = група,
    Age = вік,
    Smoking = `статус куріння`,
    Antihypertensive = `статус антигіпертензивної терапії`,
    Syndecan_1 = `синдекан 1(ng/ml)`,
    CIMT = КІМ,
    Stenosis = `% Стенозу (число)`,
    SBP = САТ,
    DBP = ДАТ,
    BMI = ІМТ,
    TC = ЗХ,
    LDL = ЛПНЩ,
    TG = ТАГ,
    HDL = ЛПВЩ,
    Glucose = Глюкоза,
    Creatinine = креатинін
  )

# 4. Define numerical variables
num_vars <- c("Age", "Syndecan_1", "CIMT", "Stenosis", "SBP", "DBP", 
              "BMI", "TC", "LDL", "TG", "HDL", "Glucose", "Creatinine")

# 5. Force numeric conversion (replaces commas with dots to prevent errors)
df_clean <- df_clean %>%
  mutate(
    # Примусове перетворення всіх числових колонок у числовий формат
    across(all_of(num_vars), ~ as.numeric(gsub(",", ".", as.character(.)))),
    
    # Factors for group comparisons
    Group_Factor = as.factor(Atherosclerosis),
    Group_Numeric = as.numeric(as.character(Atherosclerosis)),
    
    # Z-score standardization for regression analysis
    Z_LDL = scale(LDL)[, 1],
    Z_Syndecan_1 = scale(Syndecan_1)[, 1],
    Z_Age = scale(Age)[, 1]
  )

cat("Data preparation completed. Cleaned dataset contains", nrow(df_clean), "patients.\n")