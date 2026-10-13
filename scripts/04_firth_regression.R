# ==============================================================================
# Script 04: Firth's Penalized Logistic Regression (Table 3 & Figure 2)
# ==============================================================================
library(logistf)
library(car)
library(ggplot2)
library(patchwork)
library(dplyr)

# 1. Firth's Regression Models (Absolute Values)
m_abs_ldl <- logistf(Group_Numeric ~ LDL, data = df_clean)
m_abs_synd <- logistf(Group_Numeric ~ Syndecan_1, data = df_clean)
m_abs_comb <- logistf(Group_Numeric ~ LDL + Syndecan_1, data = df_clean)

# 2. Firth's Regression Models (Z-scores)
m_z_ldl <- logistf(Group_Numeric ~ Z_LDL, data = df_clean)
m_z_synd <- logistf(Group_Numeric ~ Z_Syndecan_1, data = df_clean)
m_z_comb <- logistf(Group_Numeric ~ Z_LDL + Z_Syndecan_1, data = df_clean)
m_z_sens <- logistf(Group_Numeric ~ Z_LDL + Z_Syndecan_1 + Z_Age, data = df_clean) # Age-adjusted

# 3. Multicollinearity Assessment (VIF)
model_glm <- glm(Group_Numeric ~ Z_LDL + Z_Syndecan_1, data = df_clean, family = binomial)
vif_val <- vif(model_glm)
cat("VIF for combined model:", round(vif_val[1], 3), "\n")

# 4. Forest Plot Visualization Data Preparation
df_a <- data.frame(
  Label = factor(c("LDL (Adjusted)", "LDL (Unadjusted)", "Syndecan-1 (Adjusted)", "Syndecan-1 (Unadjusted)"), 
                 levels = c("LDL (Adjusted)", "LDL (Unadjusted)", "Syndecan-1 (Adjusted)", "Syndecan-1 (Unadjusted)")),
  OR = c(exp(coef(m_abs_comb)["LDL"]), exp(coef(m_abs_ldl)["LDL"]), exp(coef(m_abs_comb)["Syndecan_1"]), exp(coef(m_abs_synd)["Syndecan_1"])),
  CI_L = c(exp(confint(m_abs_comb)["LDL", 1]), exp(confint(m_abs_ldl)["LDL", 1]), exp(confint(m_abs_comb)["Syndecan_1", 1]), exp(confint(m_abs_synd)["Syndecan_1", 1])),
  CI_U = c(exp(confint(m_abs_comb)["LDL", 2]), exp(confint(m_abs_ldl)["LDL", 2]), exp(confint(m_abs_comb)["Syndecan_1", 2]), exp(confint(m_abs_synd)["Syndecan_1", 2]))
)

df_b <- data.frame(
  Label = factor(c("Z-Syndecan-1 (Age-adj.)", "Z-Syndecan-1 (Adjusted)", "Z-Syndecan-1 (Unadjusted)", "Z-LDL (Adjusted)", "Z-LDL (Unadjusted)"), 
                 levels = c("Z-Syndecan-1 (Age-adj.)", "Z-Syndecan-1 (Adjusted)", "Z-Syndecan-1 (Unadjusted)", "Z-LDL (Adjusted)", "Z-LDL (Unadjusted)")),
  OR = c(exp(coef(m_z_sens)["Z_Syndecan_1"]), exp(coef(m_z_comb)["Z_Syndecan_1"]), exp(coef(m_z_synd)["Z_Syndecan_1"]), exp(coef(m_z_comb)["Z_LDL"]), exp(coef(m_z_ldl)["Z_LDL"])),
  CI_L = c(exp(confint(m_z_sens)["Z_Syndecan_1", 1]), exp(confint(m_z_comb)["Z_Syndecan_1", 1]), exp(confint(m_z_synd)["Z_Syndecan_1", 1]), exp(confint(m_z_comb)["Z_LDL", 1]), exp(confint(m_z_ldl)["Z_LDL", 1])),
  CI_U = c(exp(confint(m_z_sens)["Z_Syndecan_1", 2]), exp(confint(m_z_comb)["Z_Syndecan_1", 2]), exp(confint(m_z_synd)["Z_Syndecan_1", 2]), exp(confint(m_z_comb)["Z_LDL", 2]), exp(confint(m_z_ldl)["Z_LDL", 2]))
)

# 5. Plot Generation Function
create_forest_panel <- function(data, title, color_code, hide_x) {
  data <- data %>% mutate(text_label = ifelse(CI_U > 100, sprintf("%.2f [%.2f-%.0f]", OR, CI_L, CI_U), sprintf("%.2f [%.2f-%.2f]", OR, CI_L, CI_U)))
  p <- ggplot(data, aes(x = OR, y = Label)) +
    geom_errorbarh(aes(xmin = CI_L, xmax = CI_U), height = 0.2, color = color_code, size = 0.8) +
    geom_point(color = color_code, size = 3, shape = 15) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
    geom_text(aes(x = CI_U * 1.3, label = text_label), hjust = 0, size = 3.5, color = "black") +
    scale_x_log10(limits = c(0.8, 20000), breaks = c(1, 10, 100, 1000)) +
    labs(title = title, x = "Odds Ratio", y = NULL) +
    theme_classic() + theme(axis.text.y = element_text(size = 11, color = "black"), plot.title = element_text(face = "bold", size = 12))
  if (hide_x) p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
  return(p)
}

plot_a <- create_forest_panel(df_a, "A", "#d73027", TRUE)
plot_b <- create_forest_panel(df_b, "B", "#4575b4", FALSE)
final_plot <- plot_a / plot_b + plot_layout(heights = c(4, 5))

ggsave("../results/Figure_2_Forest_Plot.png", plot = final_plot, width = 10, height = 8, dpi = 300, bg = "white")
cat("Figure 2 (Forest Plot) successfully generated.\n")