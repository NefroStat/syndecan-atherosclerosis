# ==============================================================================
# MASTER SCRIPT: Generation of Supplementary Data Workbook
# ==============================================================================
# Завантаження пакетів
library(readxl)
library(dplyr)
library(openxlsx)
library(logistf)
library(pROC)
library(Hmisc)
library(car)

# ------------------------------------------------------------------------------
# 1. ЗАВАНТАЖЕННЯ ТА ПІДГОТОВКА ДАНИХ
# ------------------------------------------------------------------------------
data_path <- "../data/синдекан1.xlsx" 
if(!file.exists(data_path)) data_path <- "синдекан1.xlsx"

df <- read_excel(data_path)
num_vars <- c("вік", "синдекан 1(ng/ml)", "КІМ", "% Стенозу (число)", "САТ", 
              "ДАТ", "ІМТ", "ЗХ", "ЛПНЩ", "ТАГ", "ЛПВЩ", "Глюкоза", "креатинін")

df_clean <- df %>% 
  filter(!is.na(ЛПНЩ), !is.na(`синдекан 1(ng/ml)`), !is.na(група)) %>%
  mutate(across(all_of(num_vars[num_vars %in% colnames(df)]), 
                ~ as.numeric(gsub(",", ".", as.character(.))))) %>%
  rename(
    Atherosclerosis = група, Age = вік, Smoking = `статус куріння`,
    Antihypertensive = `статус антигіпертензивної терапії`, Syndecan_1 = `синдекан 1(ng/ml)`,
    CIMT = КІМ, Stenosis = `% Стенозу (число)`, SBP = САТ, DBP = ДАТ,
    BMI = ІМТ, TC = ЗХ, LDL = ЛПНЩ, TG = ТАГ, HDL = ЛПВЩ,
    Glucose = Глюкоза, Creatinine = креатинін
  ) %>%
  mutate(
    Group_Factor = as.factor(Atherosclerosis),
    Group_Numeric = as.numeric(as.character(Atherosclerosis)),
    Z_LDL = scale(LDL)[, 1], Z_Syndecan_1 = scale(Syndecan_1)[, 1], Z_Age = scale(Age)[, 1]
  )

# ------------------------------------------------------------------------------
# 2. БАЗОВІ ХАРАКТЕРИСТИКИ (Манн-Уїтні та Фішер)
# ------------------------------------------------------------------------------
cat_vars <- c("Smoking", "Antihypertensive")
num_vars_eng <- c("Age", "Syndecan_1", "CIMT", "Stenosis", "SBP", "DBP", 
                  "BMI", "TC", "LDL", "TG", "HDL", "Glucose", "Creatinine")

res_list <- list()
for (var in num_vars_eng) {
  val_0 <- df_clean[[var]][df_clean$Group_Factor == "0"]
  val_1 <- df_clean[[var]][df_clean$Group_Factor == "1"]
  test <- wilcox.test(val_0, val_1, exact = FALSE)
  
  res_list[[var]] <- data.frame(
    Parameter = var,
    Median_Q1_Q3_Control = sprintf("%.2f [%.2f; %.2f]", median(val_0, na.rm=T), quantile(val_0, 0.25, na.rm=T), quantile(val_0, 0.75, na.rm=T)),
    Median_Q1_Q3_Athero = sprintf("%.2f [%.2f; %.2f]", median(val_1, na.rm=T), quantile(val_1, 0.25, na.rm=T), quantile(val_1, 0.75, na.rm=T)),
    Exact_p_value = signif(test$p.value, 5)
  )
}
for (var in cat_vars) {
  tbl <- table(df_clean$Group_Factor, df_clean[[var]])
  f_test <- fisher.test(tbl)
  res_list[[var]] <- data.frame(
    Parameter = var,
    Median_Q1_Q3_Control = sprintf("%d (%.1f%%)", tbl[1,2], (tbl[1,2]/sum(tbl[1,]))*100),
    Median_Q1_Q3_Athero = sprintf("%d (%.1f%%)", tbl[2,2], (tbl[2,2]/sum(tbl[2,]))*100),
    Exact_p_value = signif(f_test$p.value, 5)
  )
}
tab_baseline <- bind_rows(res_list) %>% 
  mutate(Exact_FDR_p_value = signif(p.adjust(Exact_p_value, method = "fdr"), 5))

# ------------------------------------------------------------------------------
# 3. КОРЕЛЯЦІЙНА МАТРИЦЯ (Табличний формат з точними p)
# ------------------------------------------------------------------------------
data_cor <- df_clean %>% select(all_of(num_vars_eng)) %>% na.omit()
cor_res <- rcorr(as.matrix(data_cor), type = "spearman")
tab_cor <- as.data.frame(as.table(cor_res$r)) %>% rename(Var1 = Var1, Var2 = Var2, Spearman_rho = Freq) %>%
  left_join(as.data.frame(as.table(cor_res$P)) %>% rename(Var1 = Var1, Var2 = Var2, Exact_p_value = Freq), by = c("Var1", "Var2")) %>%
  filter(as.numeric(Var1) < as.numeric(Var2)) %>%
  mutate(Spearman_rho = round(Spearman_rho, 4), Exact_p_value = signif(Exact_p_value, 5))

# ------------------------------------------------------------------------------
# 4. РЕГРЕСІЯ ФІРТА (Абсолютні та Z-оцінки) + VIF
# ------------------------------------------------------------------------------
get_firth_stats <- function(model, var_name, display_name, model_type) {
  idx <- which(names(coef(model)) == var_name)
  data.frame(
    Predictor = display_name, Model_Type = model_type,
    Odds_Ratio = round(exp(coef(model)[idx]), 4),
    CI_95_Lower = round(exp(confint(model)[idx, 1]), 4),
    CI_95_Upper = round(exp(confint(model)[idx, 2]), 4),
    Exact_p_value = signif(model$prob[idx], 5)
  )
}
m_abs_ldl <- logistf(Group_Numeric ~ LDL, data = df_clean)
m_abs_synd <- logistf(Group_Numeric ~ Syndecan_1, data = df_clean)
m_abs_comb <- logistf(Group_Numeric ~ LDL + Syndecan_1, data = df_clean)

tab_firth_abs <- bind_rows(
  get_firth_stats(m_abs_ldl, "LDL", "LDL (mmol/L)", "Unadjusted"),
  get_firth_stats(m_abs_comb, "LDL", "LDL (mmol/L)", "Adjusted (Combined)"),
  get_firth_stats(m_abs_synd, "Syndecan_1", "Syndecan-1 (ng/mL)", "Unadjusted"),
  get_firth_stats(m_abs_comb, "Syndecan_1", "Syndecan-1 (ng/mL)", "Adjusted (Combined)")
)

m_z_ldl <- logistf(Group_Numeric ~ Z_LDL, data = df_clean)
m_z_synd <- logistf(Group_Numeric ~ Z_Syndecan_1, data = df_clean)
m_z_comb <- logistf(Group_Numeric ~ Z_LDL + Z_Syndecan_1, data = df_clean)
m_z_sens <- logistf(Group_Numeric ~ Z_LDL + Z_Syndecan_1 + Z_Age, data = df_clean)

tab_firth_z <- bind_rows(
  get_firth_stats(m_z_ldl, "Z_LDL", "Z-LDL", "Unadjusted"),
  get_firth_stats(m_z_comb, "Z_LDL", "Z-LDL", "Adjusted (Combined)"),
  get_firth_stats(m_z_synd, "Z_Syndecan_1", "Z-Syndecan-1", "Unadjusted"),
  get_firth_stats(m_z_comb, "Z_Syndecan_1", "Z-Syndecan-1", "Adjusted (Combined)"),
  get_firth_stats(m_z_sens, "Z_Syndecan_1", "Z-Syndecan-1", "Sensitivity (Age-adjusted)")
)

model_glm <- glm(Group_Numeric ~ Z_LDL + Z_Syndecan_1, data = df_clean, family = binomial)
tab_vif <- data.frame(Predictor = names(vif(model_glm)), VIF = round(vif(model_glm), 4))

# ------------------------------------------------------------------------------
# 5. ROC-АНАЛІЗ ТА ТЕСТИ ДЕЛОНГА
# ------------------------------------------------------------------------------
set.seed(123)
roc_ldl <- roc(df_clean$Group_Factor, df_clean$LDL, quiet = TRUE, ci = TRUE, boot.n = 1000)
roc_synd <- roc(df_clean$Group_Factor, df_clean$Syndecan_1, quiet = TRUE, ci = TRUE, boot.n = 1000)
roc_comb <- roc(df_clean$Group_Factor, m_abs_comb$predict, quiet = TRUE, ci = TRUE, boot.n = 1000)

get_roc_table <- function(roc_obj, name) {
  coords_b <- coords(roc_obj, "best", ret = c("threshold", "specificity", "sensitivity"), transpose = FALSE)[1,]
  data.frame(
    Model = name, AUC = round(roc_obj$auc, 4), AUC_CI_Lower = round(roc_obj$ci[1], 4), AUC_CI_Upper = round(roc_obj$ci[3], 4),
    Threshold = round(coords_b$threshold, 4), Sensitivity = round(coords_b$sensitivity, 4), Specificity = round(coords_b$specificity, 4)
  )
}
tab_roc <- bind_rows(get_roc_table(roc_ldl, "LDL"), get_roc_table(roc_synd, "Syndecan-1"), get_roc_table(roc_comb, "Combined"))

p_l_s <- roc.test(roc_ldl, roc_synd, method = "delong")$p.value
p_l_c <- roc.test(roc_ldl, roc_comb, method = "delong")$p.value
p_s_c <- roc.test(roc_synd, roc_comb, method = "delong")$p.value

tab_delong <- data.frame(
  Comparison = c("LDL vs. Syndecan-1", "LDL vs. Combined", "Syndecan-1 vs. Combined"),
  Exact_p_value = signif(c(p_l_s, p_l_c, p_s_c), 5),
  Exact_FDR_p_value = signif(p.adjust(c(p_l_s, p_l_c, p_s_c), method = "fdr"), 5)
)

# ------------------------------------------------------------------------------
# 6. СТВОРЕННЯ EXCEL-КНИГИ ТА ЗБЕРЕЖЕННЯ
# ------------------------------------------------------------------------------
if(!dir.exists("../results")) dir.create("../results")
out_file <- "../results/Supplementary_Data_Full_Stats.xlsx"

wb <- createWorkbook()
addWorksheet(wb, "Table_S1_Baseline")
addWorksheet(wb, "Table_S2_Correlations")
addWorksheet(wb, "Table_S3_Regression_Absolute")
addWorksheet(wb, "Table_S4_Regression_Z_Scores")
addWorksheet(wb, "Table_S5_ROC_and_DeLong")

writeData(wb, "Table_S1_Baseline", tab_baseline)
writeData(wb, "Table_S2_Correlations", tab_cor)
writeData(wb, "Table_S3_Regression_Absolute", tab_firth_abs)
writeData(wb, "Table_S4_Regression_Z_Scores", tab_firth_z)
writeData(wb, "Table_S4_Regression_Z_Scores", tab_vif, startRow = nrow(tab_firth_z) + 3)
writeData(wb, "Table_S5_ROC_and_DeLong", tab_roc)
writeData(wb, "Table_S5_ROC_and_DeLong", tab_delong, startRow = nrow(tab_roc) + 3)

hs <- createStyle(textDecoration = "bold")
for(i in 1:5) {
  addStyle(wb, sheet = i, hs, rows = 1, cols = 1:10, gridExpand = TRUE)
  setColWidths(wb, sheet = i, cols = 1:10, widths = "auto")
}
saveWorkbook(wb, out_file, overwrite = TRUE)

cat("Supplementary Data workbook saved to:", out_file, "\n")