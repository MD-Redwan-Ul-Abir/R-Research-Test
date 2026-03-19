# ============================================================
# LSD & CV Analysis — All CSV Files
# Design: CRD with 3 replications
# Reads columns by position (col 2,3,4 = three readings)
# so it works even when header names differ across files.
# ============================================================

library(agricolae)

dir.create("Results", showWarnings = FALSE)

# ── File list ────────────────────────────────────────────────
files <- list(
  list(csv  = "data/protein content-Protein Content (%).csv",
       title = "PROTEIN CONTENT (%)",
       out   = "Results/Protein_Content_LSD_CV.txt"),

  list(csv  = "data/ash content-Ash Content (%).csv",
       title = "ASH CONTENT (%)",
       out   = "Results/Ash_Content_LSD_CV.txt"),

  list(csv  = "data/moisture content-Moisture Content (%).csv",
       title = "MOISTURE CONTENT (%)",
       out   = "Results/Moisture_Content_LSD_CV.txt"),

  list(csv  = "data/Fat content-Fat Content (%).csv",
       title = "FAT CONTENT (%)",
       out   = "Results/Fat_Content_LSD_CV.txt"),

  list(csv  = "data/Crude Fibre-Crude Fibre (%).csv",
       title = "CRUDE FIBRE (%)",
       out   = "Results/Crude_Fibre_LSD_CV.txt"),

  list(csv  = "data/Dietary Fibre-D. Fibre (%) dry basis.csv",
       title = "DIETARY FIBRE (% dry basis)",
       out   = "Results/Dietary_Fibre_LSD_CV.txt"),

  list(csv  = "data/carbohydrate-1-Carbohydrate Content.csv",
       title = "CARBOHYDRATE CONTENT",
       out   = "Results/Carbohydrate_Content_LSD_CV.txt"),

  list(csv  = "data/Antioxidant-Ic 50 ( micro gram ml ).csv",
       title = "ANTIOXIDANT IC50 (microgram/ml)",
       out   = "Results/Antioxidant_IC50_LSD_CV.txt"),

  list(csv  = "data/flavonoid-Micro gram gram.csv",
       title = "FLAVONOID CONTENT (microgram/g)",
       out   = "Results/Flavonoid_Content_LSD_CV.txt"),

  list(csv  = "data/phenolic -Phenolic content microgram gram.csv",
       title = "PHENOLIC CONTENT (microgram/g)",
       out   = "Results/Phenolic_Content_LSD_CV.txt")
)

# ── Analysis function ────────────────────────────────────────
run_analysis <- function(csv_path, title, out_path) {

  raw <- read.csv(csv_path, check.names = FALSE, stringsAsFactors = FALSE)

  # Drop blank rows
  raw <- raw[!is.na(raw[, 1]) & trimws(raw[, 1]) != "", ]

  sample_names <- trimws(raw[, 1])

  # Use columns 2,3,4 for the three readings (robust to header name differences)
  df <- data.frame(
    Treatment = factor(rep(sample_names, each = 3)),
    Rep       = factor(rep(c("R1", "R2", "R3"), times = nrow(raw))),
    Value     = c(rbind(as.numeric(raw[, 2]),
                        as.numeric(raw[, 3]),
                        as.numeric(raw[, 4])))
  )

  # ── ANOVA ────────────────────────────────────────────────
  model      <- aov(Value ~ Treatment + Rep, data = df)
  anova_summ <- summary(model)

  ms_err <- anova_summ[[1]]$`Mean Sq`[3]
  df_err <- anova_summ[[1]]$Df[3]
  f_trt  <- anova_summ[[1]]$`F value`[1]
  p_trt  <- anova_summ[[1]]$`Pr(>F)`[1]

  grand_mean <- mean(df$Value)
  cv_percent <- (sqrt(ms_err) / grand_mean) * 100

  # ── LSD ─────────────────────────────────────────────────
  lsd_result <- LSD.test(model, "Treatment", p.adj = "none", console = FALSE)
  lsd_value  <- lsd_result$statistics$LSD
  lsd_groups <- lsd_result$groups
  lsd_groups$Treatment <- rownames(lsd_groups)

  # ── Write output file ────────────────────────────────────
  sink(out_path)

  cat("=================================================================\n")
  cat(sprintf("    %s — LSD & CV ANALYSIS\n", title))
  cat("=================================================================\n\n")

  cat("---------------------------------------------------------------\n")
  cat("ANOVA TABLE\n")
  cat("---------------------------------------------------------------\n\n")
  print(anova_summ)
  cat("\n")

  cat(sprintf("F-value (Treatment) = %.4f\n", f_trt))
  cat(sprintf("p-value (Treatment) = %.4f\n", p_trt))
  cat(sprintf("MS Error            = %.6f\n", ms_err))
  cat(sprintf("df Error            = %d\n\n", df_err))

  cat(sprintf("Result: %s\n\n",
      ifelse(p_trt < 0.05,
             "SIGNIFICANT (p < 0.05)",
             "NOT SIGNIFICANT (p >= 0.05)")))

  cat("---------------------------------------------------------------\n")
  cat("CV & GRAND MEAN\n")
  cat("---------------------------------------------------------------\n\n")
  cat(sprintf("Grand Mean = %.4f\n", grand_mean))
  cat(sprintf("CV (%%)    = %.2f\n\n", cv_percent))

  cat("---------------------------------------------------------------\n")
  cat("LSD TEST (5%)\n")
  cat("---------------------------------------------------------------\n\n")
  cat(sprintf("LSD Value (5%%) = %.4f\n\n", lsd_value))

  cat("Treatment Means with LSD Grouping:\n\n")
  cat(sprintf("%-52s %10s  %s\n", "Sample", "Mean", "Group"))
  cat(rep("-", 70), "\n", sep = "")
  for (i in seq_len(nrow(lsd_groups))) {
    cat(sprintf("%-52s %10.4f  %s\n",
                lsd_groups$Treatment[i],
                lsd_groups$Value[i],
                lsd_groups$groups[i]))
  }
  cat(rep("-", 70), "\n", sep = "")

  cat("\n---------------------------------------------------------------\n")
  cat("SUMMARY\n")
  cat("---------------------------------------------------------------\n\n")
  cat(sprintf("Grand Mean : %.4f\n", grand_mean))
  cat(sprintf("LSD (5%%)   : %.4f\n", lsd_value))
  cat(sprintf("CV (%%)     : %.2f\n", cv_percent))

  sink()

  # Console progress report
  cat(sprintf("[DONE] %-40s  Grand Mean=%.4f  LSD=%.4f  CV=%.2f%%\n",
              title, grand_mean, lsd_value, cv_percent))
}

# ── Run all ──────────────────────────────────────────────────
cat("\nRunning LSD & CV analysis for all datasets...\n\n")

for (f in files) {
  run_analysis(f$csv, f$title, f$out)
}

cat("\nAll results saved to the Results/ folder.\n")
