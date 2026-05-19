# ============================================================
# LSD & CV Analysis — All CSV Files
# Design: CRD with 3 replications
# Reads columns by position (col 2,3,4 = three readings)
# so it works even when header names differ across files.
# ============================================================

library(agricolae)

dir.create("Results", showWarnings = FALSE)

# ── Analysis function ────────────────────────────────────────
run_analysis <- function(csv_path, title, out_path) {

  # Read the whole file to find "Treatment"
  raw_all <- read.csv(csv_path, header = FALSE, check.names = FALSE, stringsAsFactors = FALSE)
  
  # Find "Treatment" or "Sample" or "T1"
  coords <- which(apply(raw_all, 1:2, function(x) grepl("^(Treatment|Sample)$", trimws(x), ignore.case = TRUE)), arr.ind = TRUE)
  
  if (nrow(coords) == 0) {
    # Try finding something that looks like T1
    coords <- which(apply(raw_all, 1:2, function(x) grepl("^T1$", trimws(x), ignore.case = TRUE)), arr.ind = TRUE)
    if (nrow(coords) == 0) {
       # Look for DAT columns
       dat_coords <- which(apply(raw_all, 1:2, function(x) grepl("DAT", trimws(x), ignore.case = TRUE)), arr.ind = TRUE)
       if (nrow(dat_coords) > 0) {
           row_start <- dat_coords[1, 1]
           col_start <- which(raw_all[row_start + 1, ] != "" & !is.na(raw_all[row_start + 1, ]))[1]
       } else {
           cat(sprintf("[ERROR] Could not find 'Treatment' or 'DAT' in %s. Skipping.\n", title))
           return(NULL)
       }
    } else {
        row_start <- coords[1, 1] - 1
        col_start <- coords[1, 2]
    }
  } else {
    row_start <- coords[1, 1]
    col_start <- coords[1, 2]
  }

  headers <- as.character(raw_all[row_start, ])
  col_mean <- which(grepl("^Mean$", trimws(headers), ignore.case = TRUE))
  
  if (length(col_mean) > 0) {
    reading_cols <- (col_start + 1):(col_mean[1] - 1)
  } else {
    # Find numeric columns following Treatment
    idx <- col_start + 1
    while(idx <= ncol(raw_all) && any(!is.na(suppressWarnings(as.numeric(as.character(raw_all[(row_start+1):nrow(raw_all), idx])))))) {
        idx <- idx + 1
    }
    reading_cols <- (col_start + 1):(idx - 1)
  }

  data_rows <- raw_all[(row_start + 1):nrow(raw_all), ]
  data_rows <- data_rows[trimws(as.character(data_rows[, col_start])) != "" & !is.na(data_rows[, col_start]), ]

  res_list <- list()
  for (i in 1:nrow(data_rows)) {
    trt <- trimws(as.character(data_rows[i, col_start]))
    for (j in seq_along(reading_cols)) {
      val <- as.numeric(as.character(data_rows[i, reading_cols[j]]))
      if (!is.na(val)) {
        res_list[[length(res_list) + 1]] <- data.frame(
          Treatment = trt,
          Rep = paste0("R", j),
          Value = val,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  df <- do.call(rbind, res_list)
  df$Treatment <- factor(df$Treatment)
  df$Rep <- factor(df$Rep)

  # Check if we have enough treatments for ANOVA
  if (length(unique(df$Treatment)) < 2) {
    cat(sprintf("[SKIP] %s: Not enough treatments (found %d).\n", 
                title, length(unique(df$Treatment))))
    return(NULL)
  }

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

# ── Automatic File Detection ──────────────────────────────────
cat("\nScanning 'data/' folder for all datasets...\n")

csv_files <- list.files("data", pattern = "\\.csv$", full.names = TRUE)

if (length(csv_files) == 0) {
  stop("No CSV files found in the 'data/' folder!")
}

cat(sprintf("Found %d files. Starting analysis...\n\n", length(csv_files)))

# ── Run all ──────────────────────────────────────────────────
for (csv_path in csv_files) {
  
  # Generate Title and Output Filename automatically
  file_name <- basename(csv_path)
  base_name <- sub("\\.csv$", "", file_name)
  
  title_name <- toupper(sub("-Table 1$", "", base_name))
  output_path <- file.path("Results", paste0(base_name, "_LSD_CV.txt"))
  
  # Run the analysis
  run_analysis(csv_path, title_name, output_path)
}

cat("\nAll analyses complete! Check the 'Results/' folder.\n")
