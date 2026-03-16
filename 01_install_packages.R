# ============================================================
# STEP 1: Install Required Packages
# Run this file ONCE before running any other scripts
# ============================================================

# Install all necessary packages
install.packages("agricolae")   # For LSD, ANOVA, Duncan's test
install.packages("dplyr")       # For data manipulation
install.packages("ggplot2")     # For graphs and charts
install.packages("readxl")      # For reading Excel files
install.packages("writexl")     # For writing Excel files
install.packages("car")         # For Levene's test, ANOVA
install.packages("emmeans")     # For post-hoc comparisons
install.packages("multcomp")    # For multiple comparisons
install.packages("tidyr")       # For data reshaping

cat("All packages installed successfully!\n")
cat("Now you can run the main analysis script.\n")
