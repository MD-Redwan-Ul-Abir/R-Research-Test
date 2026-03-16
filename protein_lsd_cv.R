# ============================================================
# LSD & CV Analysis — Protein Content (%)
# Design: CRD with 3 replications
# ============================================================

library(agricolae)

# ── Read data ────────────────────────────────────────────────
raw <- read.csv("data/protein content-Protein Content (%).csv",
                check.names = FALSE, stringsAsFactors = FALSE)

# Keep only rows with actual sample names (drop blank rows)
raw <- raw[!is.na(raw[,1]) & trimws(raw[,1]) != "", ]

# Short labels for ANOVA output
sample_names <- trimws(raw[["Sample"]])

# Build long-format data frame (Treatment + Rep + Value)
protein_data <- data.frame(
  Treatment = factor(rep(sample_names, each = 3)),
  Rep       = factor(rep(c("R1","R2","R3"), times = nrow(raw))),
  Value     = c(rbind(as.numeric(raw[["Reading 1"]]),
                      as.numeric(raw[["Reading 2"]]),
                      as.numeric(raw[["Reading 3"]])))
)

cat("Data loaded:\n")
print(protein_data)
cat("\n")

dir.create("Results", showWarnings = FALSE)
sink("Results/Protein_LSD_CV.txt")

cat("=================================================================\n")
cat("    PROTEIN CONTENT (%) — LSD & CV ANALYSIS\n")
cat("=================================================================\n\n")

# ── ANOVA ────────────────────────────────────────────────────
cat("---------------------------------------------------------------\n")
cat("ANOVA TABLE\n")
cat("---------------------------------------------------------------\n\n")

model      <- aov(Value ~ Treatment + Rep, data = protein_data)
anova_summ <- summary(model)
print(anova_summ)
cat("\n")

ms_err     <- anova_summ[[1]]$`Mean Sq`[3]   # Error MS
df_err     <- anova_summ[[1]]$Df[3]          # Error df
f_trt      <- anova_summ[[1]]$`F value`[1]
p_trt      <- anova_summ[[1]]$`Pr(>F)`[1]

cat(sprintf("F-value (Treatment) = %.4f\n", f_trt))
cat(sprintf("p-value (Treatment) = %.4f\n", p_trt))
cat(sprintf("MS Error            = %.6f\n", ms_err))
cat(sprintf("df Error            = %d\n\n", df_err))

cat(sprintf("Result: %s\n\n",
    ifelse(p_trt < 0.05,
           "SIGNIFICANT (p < 0.05)",
           "NOT SIGNIFICANT (p >= 0.05)")))

# ── CV ───────────────────────────────────────────────────────
grand_mean <- mean(protein_data$Value)
cv_percent <- (sqrt(ms_err) / grand_mean) * 100

cat(sprintf("Grand Mean = %.4f\n", grand_mean))
cat(sprintf("CV (%%)    = %.2f\n\n", cv_percent))

# ── LSD Test ─────────────────────────────────────────────────
cat("---------------------------------------------------------------\n")
cat("LSD TEST (5%)\n")
cat("---------------------------------------------------------------\n\n")

lsd_result <- LSD.test(model, "Treatment", p.adj = "none", console = FALSE)

cat(sprintf("LSD Value (5%%) = %.4f\n\n", lsd_result$statistics$LSD))

cat("Treatment Means with LSD Grouping:\n\n")
lsd_groups <- lsd_result$groups
lsd_groups$Treatment <- rownames(lsd_groups)

cat(sprintf("%-52s %8s  %s\n", "Sample", "Mean", "Group"))
cat(rep("-", 68), "\n", sep = "")
for (i in 1:nrow(lsd_groups)) {
  cat(sprintf("%-52s %8.4f  %s\n",
              lsd_groups$Treatment[i],
              lsd_groups$Value[i],
              lsd_groups$groups[i]))
}
cat(rep("-", 68), "\n", sep = "")

# ── Summary ──────────────────────────────────────────────────
cat("\n---------------------------------------------------------------\n")
cat("SUMMARY\n")
cat("---------------------------------------------------------------\n\n")
cat(sprintf("Grand Mean : %.4f\n", grand_mean))
cat(sprintf("LSD (5%%)   : %.4f\n", lsd_result$statistics$LSD))
cat(sprintf("CV (%%)     : %.2f\n", cv_percent))

sink()

# Also print to console
cat("\n=== RESULTS ===\n")
cat(sprintf("Grand Mean : %.4f\n", grand_mean))
cat(sprintf("LSD (5%%)   : %.4f\n", lsd_result$statistics$LSD))
cat(sprintf("CV (%%)     : %.2f\n\n", cv_percent))

cat("Treatment Means with LSD Groups:\n")
print(lsd_groups[, c("Treatment","Value","groups")])

cat("\nFull results saved to Results/Protein_LSD_CV.txt\n")
