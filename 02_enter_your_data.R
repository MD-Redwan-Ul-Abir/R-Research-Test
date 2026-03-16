# ============================================================
# STEP 2: Enter Your Research Data Here
# Edit this file with YOUR actual data before running analysis
# ============================================================

# ----------------------------------------------------------------
# OPTION A: Enter data manually (edit the numbers below)
# ----------------------------------------------------------------

# Example: Experiment with 3 Treatments and 3 Replications
# Replace these numbers with YOUR data

# Treatment names (change as needed)
treatments <- c("T1", "T2", "T3", "T4")

# Your observed values for each treatment (each row = one replication)
# Format: each treatment gets its own vector of values
T1 <- c(25.3, 26.1, 24.8, 25.9)   # Replace with your T1 values
T2 <- c(30.2, 31.5, 29.8, 30.9)   # Replace with your T2 values
T3 <- c(22.1, 21.8, 23.0, 22.5)   # Replace with your T3 values
T4 <- c(35.0, 34.5, 36.1, 35.4)   # Replace with your T4 values

# Number of replications
replications <- 4

# Build the data frame (do not change this part)
data <- data.frame(
  Treatment = rep(treatments, each = replications),
  Replication = rep(1:replications, times = length(treatments)),
  Value = c(T1, T2, T3, T4)
)

# ----------------------------------------------------------------
# OPTION B: Load from CSV file (uncomment if using a file)
# ----------------------------------------------------------------
# Put your CSV file in the Research_Analysis folder, then:
# data <- read.csv("your_data.csv")

# ----------------------------------------------------------------
# OPTION C: Load from Excel file (uncomment if using Excel)
# ----------------------------------------------------------------
# library(readxl)
# data <- read_excel("your_data.xlsx", sheet = 1)

# ----------------------------------------------------------------
# View your data to confirm it looks correct
# ----------------------------------------------------------------
cat("Your data:\n")
print(data)

cat("\nData summary:\n")
print(summary(data))

cat("\nData loaded successfully! Now run 03_full_analysis.R\n")
