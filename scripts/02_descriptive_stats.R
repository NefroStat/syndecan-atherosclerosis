# ==============================================================================
# Script 02: Baseline Characteristics (Table 1)
# ==============================================================================
library(dplyr)
library(rstatix)
library(openxlsx)

# Source variables from Script 01
cat_vars <- c("Smoking", "Antihypertensive")
num_vars <- c("Age", "Syndecan_1", "CIMT", "Stenosis", "SBP", "DBP", 
              "BMI", "TC", "LDL", "TG", "HDL", "Glucose", "Creatinine")

results_list <- list()

# 1. Mann-Whitney U test for continuous variables
for (var in num_vars) {
  formula <- as.formula(paste0("`", var, "` ~ Group_Factor"))
  
  stats <- df_clean %>% group_by(Group_Factor) %>%
    summarise(med = median(!!sym(var), na.rm=TRUE), 
              q1 = quantile(!!sym(var), 0.25, na.rm=TRUE), 
              q3 = quantile(!!sym(var), 0.75, na.rm=TRUE), .groups='drop')
  
  val_0 <- stats %>% filter(Group_Factor == "0")
  val_1 <- stats %>% filter(Group_Factor == "1")
  
  str_0 <- sprintf("%.2f [%.2f; %.2f]", val_0$med, val_0$q1, val_0$q3)
  str_1 <- sprintf("%.2f [%.2f; %.2f]", val_1$med, val_1$q1, val_1$q3)
  
  test <- tryCatch(df_clean %>% wilcox_test(formula), error = function(e) NULL)
  
  results_list[[var]] <- data.frame(
    Parameter = var, 
    Non_Atherosclerosis = str_0, 
    Atherosclerosis = str_1, 
    p_value_raw = if(!is.null(test)) test$p else NA, 
    check.names = FALSE
  )
}

# 2. Fisher's Exact test for categorical variables
for (var in cat_vars) {
  tbl <- table(df_clean$Group_Factor, df_clean[[var]])
  f_test <- fisher.test(tbl)
  
  calc_binom <- function(grp_val) {
    n_tot <- sum(df_clean$Group_Factor == grp_val, na.rm = TRUE); n_1 <- sum(df_clean$Group_Factor == grp_val & df_clean[[var]] == 1, na.rm = TRUE);
    
    if (n_tot > 0) {
      b_test <- binom.test(n_1, n_tot);
      prop <- (n_1 / n_tot) * 100;
      ci_low <- b_test$conf.int[1] * 100;
      ci_high <- b_test$conf.int[2] * 100;
      return(sprintf("%.1f%% [%.1f%% - %.1f%%] (n=%d)", prop, ci_low, ci_high, n_1));
    } else {
      return("NA");
    }
  }
  
  results_list[[var]] <- data.frame(
    Parameter = var, 
    Non_Atherosclerosis = calc_binom(0), 
    Atherosclerosis = calc_binom(1), 
    p_value_raw = f_test$p.value, 
    check.names = FALSE
  )
}

# 3. Apply FDR adjustment and export
table_1 <- bind_rows(results_list) %>%
  mutate(
    FDR_adjusted_p = round(p.adjust(p_value_raw, method = "fdr"), 4),
    p_value_raw = round(p_value_raw, 4)
  )

write.xlsx(table_1, "../results/Table_1_Baseline.xlsx", overwrite = TRUE)
cat("Table 1 successfully generated and exported.\n")