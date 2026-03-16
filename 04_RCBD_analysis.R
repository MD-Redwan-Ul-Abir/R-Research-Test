# ============================================================
# RCBD (Randomized Complete Block Design) Analysis
# Use this if your experiment has BLOCKS (replications as blocks)
# This is very common in field/agricultural experiments
# ============================================================

library(agricolae)
library(dplyr)
library(ggplot2)

# ----------------------------------------------------------------
# Enter YOUR data here
# Example: 4 treatments x 4 blocks (replications)
# ----------------------------------------------------------------

# Treatment labels
trt <- c("T1", "T2", "T3", "T4")

# Block labels
block <- c("B1", "B2", "B3", "B4")

# Your data values — row = treatment, col = block
# Arrange your data: each treatment in each block
raw_values <- c(
  # B1    B2    B3    B4
  25.3, 26.1, 24.8, 25.9,   # T1
  30.2, 31.5, 29.8, 30.9,   # T2
  22.1, 21.8, 23.0, 22.5,   # T3
  35.0, 34.5, 36.1, 35.4    # T4
)

# Build data frame
n_trt   <- length(trt)
n_block <- length(block)

rcbd_data <- data.frame(
  Treatment   = factor(rep(trt, each = n_block)),
  Block       = factor(rep(block, times = n_trt)),
  Value       = raw_values
)

cat("Your RCBD Data:\n")
print(rcbd_data)
cat("\n")

dir.create("Results", showWarnings = FALSE)
sink("Results/RCBD_Results.txt")

cat("=================================================================\n")
cat("    RCBD (Randomized Complete Block Design) ANALYSIS\n")
cat("=================================================================\n\n")

# ----------------------------------------------------------------
# ANOVA for RCBD (Treatment + Block effects)
# ----------------------------------------------------------------
cat("---------------------------------------------------------------\n")
cat("ANOVA TABLE (RCBD)\n")
cat("---------------------------------------------------------------\n\n")

rcbd_model  <- aov(Value ~ Treatment + Block, data = rcbd_data)
rcbd_anova  <- summary(rcbd_model)
print(rcbd_anova)
cat("\n")

f_trt  <- rcbd_anova[[1]]$`F value`[1]
p_trt  <- rcbd_anova[[1]]$`Pr(>F)`[1]
ms_err <- rcbd_anova[[1]]$`Mean Sq`[3]
df_err <- rcbd_anova[[1]]$Df[3]

cat(sprintf("F-value (Treatment) = %.4f\n", f_trt))
cat(sprintf("p-value (Treatment) = %.4f\n", p_trt))
cat(sprintf("MS Error            = %.4f\n", ms_err))
cat(sprintf("df Error            = %d\n\n", df_err))

cat(sprintf("ANOVA Result = %s\n\n",
    ifelse(p_trt < 0.05,
           "SIGNIFICANT — treatments have significant effect",
           "NOT significant — no significant treatment effect")))

# ----------------------------------------------------------------
# CV Calculation
# ----------------------------------------------------------------
grand_mean <- mean(rcbd_data$Value)
cv_percent <- (sqrt(ms_err) / grand_mean) * 100

cat(sprintf("Grand Mean = %.4f\n", grand_mean))
cat(sprintf("CV (%%)    = %.2f\n\n", cv_percent))

# ----------------------------------------------------------------
# LSD Test
# ----------------------------------------------------------------
cat("---------------------------------------------------------------\n")
cat("LSD TEST\n")
cat("---------------------------------------------------------------\n\n")

lsd_result <- LSD.test(rcbd_model, "Treatment", p.adj = "none")
cat(sprintf("LSD (5%%) = %.4f\n\n", lsd_result$statistics$LSD))
cat("Treatment Means with LSD Grouping:\n")
print(lsd_result$groups)
cat("\n")

# ----------------------------------------------------------------
# DMRT
# ----------------------------------------------------------------
cat("---------------------------------------------------------------\n")
cat("DUNCAN'S MULTIPLE RANGE TEST (DMRT)\n")
cat("---------------------------------------------------------------\n\n")

dmrt_result <- duncan.test(rcbd_model, "Treatment", console = FALSE)
print(dmrt_result$groups)
cat("\n")

# ----------------------------------------------------------------
# Final Summary Table
# ----------------------------------------------------------------
cat("---------------------------------------------------------------\n")
cat("FINAL SUMMARY TABLE\n")
cat("---------------------------------------------------------------\n\n")

trt_means <- rcbd_data %>%
  group_by(Treatment) %>%
  summarise(Mean = mean(Value), SE = sd(Value) / sqrt(n()))

lsd_grp <- lsd_result$groups
lsd_grp$Treatment <- rownames(lsd_grp)

final <- merge(trt_means, lsd_grp[, c("Treatment", "groups")], by = "Treatment")

cat("Treatment | Mean     | SE       | Group\n")
cat(rep("-", 45), "\n", sep = "")
for (i in 1:nrow(final)) {
  cat(sprintf("%-10s| %-9.4f| %-9.4f| %s\n",
              final$Treatment[i], final$Mean[i],
              final$SE[i], final$groups[i]))
}
cat(rep("-", 45), "\n", sep = "")
cat(sprintf("Grand Mean: %.4f\n", grand_mean))
cat(sprintf("LSD (5%%): %.4f\n",  lsd_result$statistics$LSD))
cat(sprintf("CV (%%): %.2f\n",    cv_percent))

sink()
cat("RCBD results saved to Results/RCBD_Results.txt\n")

# ----------------------------------------------------------------
# Graph
# ----------------------------------------------------------------
lsd_grp2 <- lsd_result$groups
lsd_grp2$Treatment <- rownames(lsd_grp2)
plot_data <- merge(trt_means, lsd_grp2[, c("Treatment", "groups")], by = "Treatment")

p <- ggplot(plot_data, aes(x = Treatment, y = Mean, fill = Treatment)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2) +
  geom_text(aes(label = groups, y = Mean + SE + 0.5), size = 5, fontface = "bold") +
  labs(title = "Treatment Means with LSD Grouping Letters",
       x = "Treatment", y = "Mean Value",
       caption = paste0("LSD (5%) = ", round(lsd_result$statistics$LSD, 4),
                        " | CV = ", round(cv_percent, 2), "%")) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("Results/RCBD_bar_with_LSD_groups.png", p, width = 8, height = 6, dpi = 300)
cat("Graph saved to Results/RCBD_bar_with_LSD_groups.png\n")
