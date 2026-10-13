# ==============================================================================
# Script 05: ROC Curve Analysis and DeLong's Tests (Figure 3)
# ==============================================================================
library(pROC)
library(ggplot2)
library(patchwork)

set.seed(123) # Ensure reproducibility for bootstrapping

# 1. Generate ROC objects
roc_ldl <- roc(df_clean$Group_Factor, df_clean$LDL, quiet = TRUE, ci = TRUE, boot.n = 1000)
roc_synd <- roc(df_clean$Group_Factor, df_clean$Syndecan_1, quiet = TRUE, ci = TRUE, boot.n = 1000)
roc_comb <- roc(df_clean$Group_Factor, m_abs_comb$predict, quiet = TRUE, ci = TRUE, boot.n = 1000)

# 2. DeLong's pairwise tests and FDR adjustment
p_ldl_vs_synd  <- roc.test(roc_ldl, roc_synd, method = "delong")$p.value
p_ldl_vs_comb  <- roc.test(roc_ldl, roc_comb, method = "delong")$p.value
p_synd_vs_comb <- roc.test(roc_synd, roc_comb, method = "delong")$p.value

p_adjusted <- p.adjust(c(p_ldl_vs_synd, p_ldl_vs_comb, p_synd_vs_comb), method = "fdr")

format_pval <- function(p) { if (p < 0.001) return("< 0.001") else return(sprintf("= %.3f", p)) }
delong_text <- sprintf(
  "DeLong's test (FDR-adjusted):\n\nCombined vs. LDL, p %s\nCombined vs. Syndecan-1, p %s\nSyndecan-1 vs. LDL, p %s",
  format_pval(p_adjusted[2]), format_pval(p_adjusted[3]), format_pval(p_adjusted[1])
)

# 3. Extract coordinates for plotting
extract_roc_data <- function(roc_obj) { data.frame(FPR = 1 - roc_obj$specificities, TPR = roc_obj$sensitivities) }
df_ldl  <- extract_roc_data(roc_ldl)
df_synd <- extract_roc_data(roc_synd)
df_comb <- extract_roc_data(roc_comb)

# 4. Panel generation
create_roc_panel <- function(data, color_code, inner_label) {
  ggplot(data, aes(x = FPR, y = TPR)) +
    geom_line(size = 1.2, color = color_code, linetype = "solid") +
    geom_abline(intercept = 0, slope = 1, linetype = "dotted", color = "gray60", size = 0.8) +
    annotate("text", x = 0.5, y = 0.20, label = inner_label, size = 4.5, fontface = "bold", lineheight = 1.2) +
    labs(x = "1 - Specificity", y = "Sensitivity") + theme_classic() +
    theme(axis.text = element_text(size = 11, color = "black"), axis.title = element_text(size = 12, color = "black")) +
    coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1))
}

plot_ldl  <- create_roc_panel(df_ldl,  "#4575b4", sprintf("LDL\nAUC = %.3f", roc_ldl$auc))
plot_synd <- create_roc_panel(df_synd, "#1a9850", sprintf("Syndecan-1\nAUC = %.3f", roc_synd$auc))
plot_comb <- create_roc_panel(df_comb, "#d73027", sprintf("Combined Model\nAUC = %.3f", roc_comb$auc))

text_panel <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = delong_text, size = 4.5, fontface = "italic", color = "black", hjust = 0.5, lineheight = 1.5) +
  theme_void() + coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))

# 5. Compile matrix and save
final_roc_matrix <- (plot_ldl | plot_synd) / (plot_comb | text_panel) +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 14, face = "bold"))

ggsave("../results/Figure_3_ROC_Matrix.png", plot = final_roc_matrix, width = 9, height = 9, dpi = 300, bg = "white")
cat("Figure 3 (ROC Matrix) successfully generated.\n")