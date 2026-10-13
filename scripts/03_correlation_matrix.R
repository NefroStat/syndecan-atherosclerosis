# ==============================================================================
# Script 03: Spearman Correlation Matrix (Figure 1)
# ==============================================================================
library(ggplot2)
library(tidyr)
library(dplyr)
library(Hmisc)

# 1. Prepare numerical data for correlation
data_cor <- df_clean %>% 
  select(Age, Syndecan_1, CIMT, Stenosis, SBP, DBP, BMI, TC, LDL, TG, HDL, Glucose, Creatinine) %>% 
  na.omit()

# 2. Calculate Spearman correlation matrix and p-values
cor_res <- rcorr(as.matrix(data_cor), type = "spearman")
r_mat <- cor_res$r
p_mat <- cor_res$P

# 3. Hierarchical clustering for variable ordering
hc <- hclust(as.dist(1 - abs(r_mat)))
ordered_vars <- colnames(data_cor)[hc$order]

# 4. Melt matrices and merge
r_df <- as.data.frame(as.table(r_mat)) %>% rename(Var1 = Var1, Var2 = Var2, r = Freq)
p_df <- as.data.frame(as.table(p_mat)) %>% rename(Var1 = Var1, Var2 = Var2, p_val = Freq)
cor_data <- left_join(r_df, p_df, by = c("Var1", "Var2"))

# 5. Filter upper triangle and format
cor_data <- cor_data %>% 
  mutate(
    num1 = as.numeric(factor(Var1, levels = ordered_vars)),
    num2 = as.numeric(factor(Var2, levels = ordered_vars))
  ) %>% 
  filter(num1 < num2) %>%
  mutate(
    Significance = if_else(p_val < 0.05, "Significant (p < 0.05)", "Non-significant (p ≥ 0.05)"),
    r_label = sprintf("%.2f", r),
    Var1 = factor(Var1, levels = ordered_vars),
    Var2 = factor(Var2, levels = rev(ordered_vars))
  )

# 6. Plot generation
cor_plot <- ggplot(cor_data, aes(x = Var1, y = Var2, fill = Significance)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = r_label), color = "black", size = 4) +
  scale_fill_manual(values = c("Significant (p < 0.05)" = "#e06666", 
                               "Non-significant (p ≥ 0.05)" = "#9fc5e8")) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  coord_fixed()

ggsave("../results/Figure_1_Correlation_Matrix.png", plot = cor_plot, width = 8, height = 8, dpi = 300, bg = "white")
cat("Figure 1 (Correlation Matrix) successfully generated.\n")