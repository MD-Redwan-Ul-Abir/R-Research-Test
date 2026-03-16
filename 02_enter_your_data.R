# ============================================================
# STEP 2: Load Data from CSV Files
# ============================================================

# Helper function: reads a CSV with 3 reading columns and reshapes
# to the long format (Treatment, Replication, Value) required by the analysis
load_data <- function(file_path, label) {
  raw <- read.csv(file_path, check.names = FALSE, stringsAsFactors = FALSE)
  # Remove rows with empty sample names
  raw <- raw[!is.na(raw[[1]]) & trimws(raw[[1]]) != "", ]
  # Remove rows where Reading 1 is missing
  raw <- raw[trimws(as.character(raw[[2]])) != "" & !is.na(raw[[2]]), ]

  n <- nrow(raw)
  df <- data.frame(
    Treatment   = rep(trimws(raw[[1]]), each = 3),
    Replication = rep(1:3, times = n),
    Value       = suppressWarnings(
                    as.numeric(c(rbind(raw[[2]], raw[[3]], raw[[4]])))
                  )
  )
  # Drop rows where a reading was missing (NA)
  df <- df[!is.na(df$Value), ]

  cat(sprintf("  %-35s -> %d treatments, %d observations\n",
              label, n, nrow(df)))
  return(df)
}

cat("Loading datasets from data/ folder...\n\n")

moisture_data  <- load_data("data/moisture content-Moisture Content (%).csv",  "Moisture Content (%)")
protein_data   <- load_data("data/protein content-Protein Content (%).csv",    "Protein Content (%)")
fat_data       <- load_data("data/Fat content-Fat Content (%).csv",            "Fat Content (%)")
ash_data       <- load_data("data/ash content-Ash Content (%).csv",            "Ash Content (%)")
fibre_data     <- load_data("data/Crude Fibre-Crude Fibre (%).csv",            "Crude Fibre (%)")
flavonoid_data <- load_data("data/flavonoid-Table 1.csv",                      "Flavonoid")

cat("\n")

# ----------------------------------------------------------------
# SELECT WHICH DATASET TO ANALYZE
# Uncomment ONE line below to choose which parameter to analyze.
# Run this file again after changing the selection.
# ----------------------------------------------------------------
data <- moisture_data    # Moisture Content (%)
# data <- protein_data   # Protein Content (%)
# data <- fat_data       # Fat Content (%)
# data <- ash_data       # Ash Content (%)
# data <- fibre_data     # Crude Fibre (%)
# data <- flavonoid_data # Flavonoid

# ----------------------------------------------------------------
# View selected data to confirm it looks correct
# ----------------------------------------------------------------
cat("Selected data:\n")
print(data)

cat("\nData summary:\n")
print(summary(data))

cat("\nData loaded successfully! Now run 03_full_analysis.R\n")
