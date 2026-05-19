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

cat("Automatically scanning data/ folder for datasets...\n\n")

# List all CSV files in the data folder
csv_files <- list.files("data", pattern = "\\.csv$", full.names = TRUE)

# Create a list to store all datasets
datasets <- list()

for (f in csv_files) {
    # Generate a clean label from the filename (e.g., "AVERAGE NO OF...")
    file_label <- sub("-Table 1\\.csv$", "", basename(f))
    file_label <- sub("\\.csv$", "", file_label)
    
    # Load the data using the smart detector
    cat(sprintf("Loading: %s\n", basename(f)))
    datasets[[file_label]] <- load_data(f, file_label)
}

cat("\nAll datasets loaded successfully!\n")

# ----------------------------------------------------------------
# SELECT WHICH DATASET TO ANALYZE
# To change the dataset, simply change the name inside the brackets ["..."]
# ----------------------------------------------------------------
cat("Available datasets:\n")
print(names(datasets))

# By default, we select the first one found
selected_name <- names(datasets)[1] 
data <- datasets[[selected_name]]

# ----------------------------------------------------------------
# View selected data
# ----------------------------------------------------------------
cat(sprintf("\n--- Selected for Analysis: %s ---\n", selected_name))

cat("\nData summary:\n")
print(summary(data))

cat("\nNow you can run 03_full_analysis.R\n")

# ----------------------------------------------------------------
# View selected data to confirm it looks correct
# ----------------------------------------------------------------
cat("Selected data:\n")
print(data)

cat("\nData summary:\n")
print(summary(data))

cat("\nData loaded successfully! Now run 03_full_analysis.R\n")
