# How to Use These R Scripts for Your Research Paper

## Files in This Folder

| File | Purpose |
|------|---------|
| `01_install_packages.R` | Install required R packages (run once) |
| `02_enter_your_data.R` | Enter your research data here |
| `03_full_analysis.R` | Complete statistical analysis |
| `04_RCBD_analysis.R` | Use this if your design has Blocks (RCBD) |

---

## Step-by-Step Instructions

### Step 1: Install Packages (Do this ONCE only)
1. Open RStudio
2. Open `01_install_packages.R`
3. Click **Run** (top right of the editor) or press **Ctrl+Shift+Enter**
4. Wait for all packages to install

### Step 2: Enter Your Data
1. Open `02_enter_your_data.R`
2. Find the section that says `T1 <- c(...)`
3. Replace the example numbers with YOUR actual data values
4. Change treatment names if needed (e.g., "Control", "Treatment A", etc.)
5. Save the file

### Step 3: Run the Analysis
1. Open `03_full_analysis.R`
2. Press **Ctrl+Shift+Enter** to run the whole script
3. Results will be saved in a new `Results/` folder

---

## What Tests Are Performed

| Test | What It Does |
|------|-------------|
| **Descriptive Statistics** | Mean, SD, SE, Min, Max per treatment |
| **CV (Coefficient of Variation)** | Measures data variability (%) |
| **One-Sample t-test** | Tests if treatment mean differs from grand mean |
| **Two-Sample t-test** | Compares pairs of treatments |
| **F-test** | Tests if variances between treatments are equal |
| **One-Way ANOVA** | Tests if any treatment difference exists |
| **LSD Test** | Identifies which treatments differ (post-ANOVA) |
| **Tukey's HSD** | More conservative version of LSD |
| **DMRT (Duncan)** | Duncan's Multiple Range Test |

---

## Output Files (in Results/ folder)

- `All_Results.txt` — All statistics in one text file
- `bar_chart_with_SE.png` — Bar chart with error bars
- `boxplot.png` — Box plot showing data distribution
- `RCBD_Results.txt` — If you used RCBD design

---

## Which Design to Use?

### Use `03_full_analysis.R` if:
- Your experiment has no blocks
- It is a simple CRD (Completely Randomized Design)

### Use `04_RCBD_analysis.R` if:
- Your experiment has replications treated as blocks
- Example: 3 treatments x 4 replications in a field layout

---

## Common Questions

**Q: My data has more/fewer treatments — what do I change?**
A: In `02_enter_your_data.R`, add or remove treatment vectors (T1, T2, etc.) and update the `treatments` list and the `data.frame()` call accordingly.

**Q: Where do I find my results?**
A: After running the script, look in the `Results/` folder inside `Research_Analysis/`.

**Q: What does the LSD group letter mean?**
A: Treatments with the **same letter** are NOT significantly different from each other. Treatments with **different letters** ARE significantly different (p < 0.05).

**Q: What is a good CV value?**
A: CV < 10% is excellent. CV 10–20% is acceptable. CV > 20% means high variability.
