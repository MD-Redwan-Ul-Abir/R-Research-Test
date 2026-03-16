# ============================================================
# STEP 3: Full Statistical Analysis for Research Paper
# Run this AFTER running 02_enter_your_data.R
# ============================================================

# Load required libraries
library(agricolae)
library(dplyr)
library(ggplot2)
library(car)

# Make sure your data is loaded (run 02_enter_your_data.R first)
# Or source it directly:
source("02_enter_your_data.R")

# Create output directory for results
dir.create("Results", showWarnings = FALSE)

# Open a file to save all results
sink("Results/All_Results.txt")

cat("=================================================================\n")
cat("         COMPLETE STATISTICAL ANALYSIS FOR RESEARCH PAPER\n")
cat("=================================================================\n\n")


# ================================================================
# 1. DESCRIPTIVE STATISTICS
# ================================================================
cat("---------------------------------------------------------------\n")
cat("1. DESCRIPTIVE STATISTICS (Mean, SD, SE, CV per Treatment)\n")
cat("---------------------------------------------------------------\n\n")

desc_stats <- data %>%
  group_by(Treatment) %>%
  summarise(
    N          = n(),
    Mean       = mean(Value),
    SD         = sd(Value),
    SE         = sd(Value) / sqrt(n()),
    Min        = min(Value),
    Max        = max(Value),
    CV_percent = (sd(Value) / mean(Value)) * 100
  )

print(desc_stats)
cat("\n")

grand_mean <- mean(data$Value)
grand_cv   <- (sd(data$Value) / grand_mean) * 100
cat(sprintf("Grand Mean : %.4f\n", grand_mean))
cat(sprintf("Overall CV : %.2f%%\n\n", grand_cv))


# ================================================================
# 2. COEFFICIENT OF VARIATION (CV)
# ================================================================
cat("---------------------------------------------------------------\n")
cat("2. COEFFICIENT OF VARIATION (CV)\n")
cat("---------------------------------------------------------------\n\n")

cat("CV = (Standard Deviation / Mean) x 100\n\n")
for (trt in unique(data$Treatment)) {
  trt_data <- data$Value[data$Treatment == trt]
  cv_val   <- (sd(trt_data) / mean(trt_data)) * 100
  cat(sprintf("  %s : CV = %.2f%%\n", trt, cv_val))
}

cat("\n  Interpretation:\n")
cat("  CV < 10% : Low variability (very good)\n")
cat("  CV 10-20%: Moderate variability (acceptable)\n")
cat("  CV > 20% : High variability (review your data)\n\n")


# ================================================================
# 3. ONE-SAMPLE t-TEST
# ================================================================
cat("---------------------------------------------------------------\n")
cat("3. ONE-SAMPLE t-TEST (Test if mean equals a specific value)\n")
cat("---------------------------------------------------------------\n\n")

# Test each treatment against the grand mean
cat(sprintf("Testing each treatment mean against Grand Mean = %.4f\n\n", grand_mean))

for (trt in unique(data$Treatment)) {
  trt_data <- data$Value[data$Treatment == trt]
  t_result <- t.test(trt_data, mu = grand_mean)

  cat(sprintf("Treatment: %s\n", trt))
  cat(sprintf("  Mean     = %.4f\n", mean(trt_data)))
  cat(sprintf("  t-value  = %.4f\n", t_result$statistic))
  cat(sprintf("  df       = %d\n", as.integer(t_result$parameter)))
  cat(sprintf("  p-value  = %.4f\n", t_result$p.value))
  cat(sprintf("  95%% CI  = [%.4f, %.4f]\n", t_result$conf.int[1], t_result$conf.int[2]))
  cat(sprintf("  Result   = %s\n\n",
      ifelse(t_result$p.value < 0.05,
             "SIGNIFICANT (p < 0.05) — mean differs from grand mean",
             "NOT significant (p >= 0.05) — mean does not differ")))
}


# ================================================================
# 4. TWO-SAMPLE t-TEST (Compare two specific treatments)
# ================================================================
cat("---------------------------------------------------------------\n")
cat("4. TWO-SAMPLE t-TEST (Compare pairs of treatments)\n")
cat("---------------------------------------------------------------\n\n")

trt_names <- unique(data$Treatment)

for (i in 1:(length(trt_names) - 1)) {
  for (j in (i + 1):length(trt_names)) {
    grp1 <- data$Value[data$Treatment == trt_names[i]]
    grp2 <- data$Value[data$Treatment == trt_names[j]]

    t_result <- t.test(grp1, grp2, var.equal = TRUE)

    cat(sprintf("Comparing %s vs %s:\n", trt_names[i], trt_names[j]))
    cat(sprintf("  Mean(%s) = %.4f | Mean(%s) = %.4f\n",
                trt_names[i], mean(grp1), trt_names[j], mean(grp2)))
    cat(sprintf("  t-value = %.4f | df = %d | p-value = %.4f\n",
                t_result$statistic,
                as.integer(t_result$parameter),
                t_result$p.value))
    cat(sprintf("  Result  = %s\n\n",
        ifelse(t_result$p.value < 0.05,
               "SIGNIFICANT difference (p < 0.05)",
               "NO significant difference (p >= 0.05)")))
  }
}


# ================================================================
# 5. F-TEST (Variance ratio test)
# ================================================================
cat("---------------------------------------------------------------\n")
cat("5. F-TEST (Test equality of variances between treatments)\n")
cat("---------------------------------------------------------------\n\n")

trt_names <- unique(data$Treatment)

for (i in 1:(length(trt_names) - 1)) {
  for (j in (i + 1):length(trt_names)) {
    grp1 <- data$Value[data$Treatment == trt_names[i]]
    grp2 <- data$Value[data$Treatment == trt_names[j]]

    f_result <- var.test(grp1, grp2)

    cat(sprintf("F-Test: %s vs %s\n", trt_names[i], trt_names[j]))
    cat(sprintf("  Var(%s) = %.4f | Var(%s) = %.4f\n",
                trt_names[i], var(grp1), trt_names[j], var(grp2)))
    cat(sprintf("  F-value = %.4f | df = (%d, %d) | p-value = %.4f\n",
                f_result$statistic,
                f_result$parameter[1],
                f_result$parameter[2],
                f_result$p.value))
    cat(sprintf("  Result  = %s\n\n",
        ifelse(f_result$p.value < 0.05,
               "Variances are SIGNIFICANTLY different (p < 0.05)",
               "Variances are NOT significantly different (p >= 0.05)")))
  }
}


# ================================================================
# 6. ONE-WAY ANOVA
# ================================================================
cat("---------------------------------------------------------------\n")
cat("6. ONE-WAY ANOVA (Analysis of Variance)\n")
cat("---------------------------------------------------------------\n\n")

# Fit ANOVA model
anova_model  <- aov(Value ~ Treatment, data = data)
anova_result <- summary(anova_model)

print(anova_result)
cat("\n")

# Extract values
f_value <- anova_result[[1]]$`F value`[1]
p_value <- anova_result[[1]]$`Pr(>F)`[1]
df_trt  <- anova_result[[1]]$Df[1]
df_err  <- anova_result[[1]]$Df[2]
ms_err  <- anova_result[[1]]$`Mean Sq`[2]

cat(sprintf("F-value        = %.4f\n", f_value))
cat(sprintf("p-value        = %.4f\n", p_value))
cat(sprintf("df (Treatment) = %d\n",   df_trt))
cat(sprintf("df (Error)     = %d\n",   df_err))
cat(sprintf("MS Error       = %.4f\n", ms_err))
cat(sprintf("\nANOVA Result   = %s\n\n",
    ifelse(p_value < 0.05,
           "SIGNIFICANT (p < 0.05) — treatments differ significantly",
           "NOT significant (p >= 0.05) — no significant difference")))


# ================================================================
# 7. LSD (Fisher's Least Significant Difference) TEST
# ================================================================
cat("---------------------------------------------------------------\n")
cat("7. LSD TEST (Fisher's Least Significant Difference)\n")
cat("   Post-hoc comparison after significant ANOVA\n")
cat("---------------------------------------------------------------\n\n")

lsd_result <- LSD.test(anova_model, "Treatment", p.adj = "none")

cat("--- LSD Value ---\n")
cat(sprintf("LSD (at 5%%) = %.4f\n\n", lsd_result$statistics$LSD))

cat("--- Treatment Means with Grouping Letters ---\n")
cat("(Treatments sharing the same letter are NOT significantly different)\n\n")
print(lsd_result$groups)

cat("\n--- Pairwise Differences vs LSD ---\n")
print(lsd_result$comparison)
cat("\n")


# ================================================================
# 8. TUKEY'S HSD TEST (Alternative to LSD — more conservative)
# ================================================================
cat("---------------------------------------------------------------\n")
cat("8. TUKEY'S HSD TEST (Honest Significant Difference)\n")
cat("---------------------------------------------------------------\n\n")

tukey_result <- HSD.test(anova_model, "Treatment", console = FALSE)

cat("--- Treatment Means with Tukey Grouping ---\n")
print(tukey_result$groups)
cat("\n")


# ================================================================
# 9. DUNCAN'S MULTIPLE RANGE TEST (DMRT)
# ================================================================
cat("---------------------------------------------------------------\n")
cat("9. DUNCAN'S MULTIPLE RANGE TEST (DMRT)\n")
cat("---------------------------------------------------------------\n\n")

dmrt_result <- duncan.test(anova_model, "Treatment", console = FALSE)

cat("--- Treatment Means with Duncan Grouping ---\n")
print(dmrt_result$groups)
cat("\n")


# ================================================================
# 10. SUMMARY TABLE (Publication-ready)
# ================================================================
cat("---------------------------------------------------------------\n")
cat("10. FINAL SUMMARY TABLE (For Research Paper)\n")
cat("---------------------------------------------------------------\n\n")

# Get LSD groups
lsd_groups <- lsd_result$groups
lsd_groups$Treatment <- rownames(lsd_groups)

# Merge with descriptive stats
summary_table <- desc_stats %>%
  left_join(lsd_groups[, c("Treatment", "groups")], by = "Treatment") %>%
  rename(LSD_Group = groups) %>%
  mutate(
    `Mean ± SE` = paste0(round(Mean, 2), " ± ", round(SE, 2))
  ) %>%
  select(Treatment, `Mean ± SE`, CV_percent, LSD_Group)

cat("Treatment | Mean ± SE | CV (%) | LSD Group\n")
cat(rep("-", 50), "\n", sep = "")
for (i in 1:nrow(summary_table)) {
  cat(sprintf("%-10s| %-15s| %-7.2f | %s\n",
              summary_table$Treatment[i],
              summary_table$`Mean ± SE`[i],
              summary_table$CV_percent[i],
              summary_table$LSD_Group[i]))
}

cat("\n")
cat(sprintf("Grand Mean    : %.4f\n", grand_mean))
cat(sprintf("LSD (5%%)     : %.4f\n", lsd_result$statistics$LSD))
cat(sprintf("CV (%%)       : %.2f\n", grand_cv))
cat(sprintf("F-value       : %.4f\n", f_value))
cat(sprintf("p-value       : %.4f\n", p_value))

cat("\n=================================================================\n")
cat("                    ANALYSIS COMPLETE\n")
cat("  Results saved in: Results/All_Results.txt\n")
cat("  Graphs saved in:  Results/ folder\n")
cat("=================================================================\n")

# Stop writing to file
sink()

cat("All results saved to Results/All_Results.txt\n")


# ================================================================
# GRAPHS (saved as PNG files)
# ================================================================

# Graph 1: Bar chart with error bars
desc_stats_plot <- data %>%
  group_by(Treatment) %>%
  summarise(Mean = mean(Value), SE = sd(Value) / sqrt(n()))

p1 <- ggplot(desc_stats_plot, aes(x = Treatment, y = Mean, fill = Treatment)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2, linewidth = 0.8) +
  labs(title = "Treatment Means with Standard Error",
       x = "Treatment", y = "Mean Value") +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("Results/bar_chart_with_SE.png", p1, width = 8, height = 6, dpi = 300)

# Graph 2: Box plot
p2 <- ggplot(data, aes(x = Treatment, y = Value, fill = Treatment)) +
  geom_boxplot(color = "black") +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(title = "Distribution of Values by Treatment",
       x = "Treatment", y = "Value") +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("Results/boxplot.png", p2, width = 8, height = 6, dpi = 300)

cat("Graphs saved:\n")
cat("  - Results/bar_chart_with_SE.png\n")
cat("  - Results/boxplot.png\n")
cat("\nDone! Open Results/All_Results.txt to see all your statistics.\n")
