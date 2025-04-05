# 🧬 Metabolomics Data Analysis for Biomarker Discovery

## 📌 Overview
A data-driven project focused on discovering potential biomarkers through metabolomics analysis of drug-induced cardiotoxicity data. This project uses statistical and visualization techniques in **R** to analyze and extract biologically meaningful insights from metabolite profiling of cardiomyocyte response data.

---

## 🎯 Project Objective

To identify significant metabolic biomarkers associated with cardiotoxicity based on drug exposure data obtained from human-induced pluripotent stem cell-derived cardiomyocytes (hiPSC-CM). The goal is to:

- Analyze raw metabolomics data to uncover meaningful patterns.
- Discover potential biomarkers using statistical and visualization techniques.
- Build an interactive frontend (R Shiny app) for visualization and interpretation of the results.

---

## 📁 Dataset

**Source**: Dryad (referenced in the publication [Toxicological Sciences, Oxford](https://academic.oup.com/toxsci/article/174/2/218/5732694))

**File**: `A_Targeted_Metabolomics-Based_Assay_Using_Human_Induced_Pluripotent_Stem_Cell-Derived_Cardiomyocytes_rawdata.csv`  
**Size**: ~1.79 MB

**Columns:**

- `Compound`: Drug tested
- `Effect`: Cardiotoxic classification (Toxic/Non)
- `Metabolite`: Measured metabolite
- `Plate`: Experimental batch
- `Dose`: Drug concentration
- `SampleType`: `cells` or `media`
- `Well`: Position in plate
- `ApexIntensity`: Peak intensity from UPLC-HRMS (proxy for metabolite abundance)

---

## 🏠 Project Structure
```
Metabolomics-Biomarker-Discovery/
├── analysis/                # All core data analysis scripts (R)
│   └── analysis.R
├── dashboard/               # Shiny App for interactive UI
│   ├── ui.R
│   ├── server.R
│   └── www/                 # Static files (CSS, assets)
├── data/
│   └── raw_dataset.csv
├── output/                  # Plots, summary CSVs, reports
└── README.md
```

---

## 🧪 Project Workflow

### 🔬 Phase 1: Data Analysis (in R)

- Data preprocessing
- Exploratory Data Analysis (EDA)
- Statistical testing (e.g., t-test, ANOVA)
- Biomarker discovery using intensity thresholds and significance
- Dose-response visualization

### 🧰 Phase 2: Interactive Dashboard

- Built using **Shiny** (R framework)
- Features:
  - Filter by compound, dose, and sample type
  - Display top biomarkers by intensity or significance
  - Visualize dose-response plots
  - Exportable summary reports

---

## ⚙️ Installation & Setup
### 1️⃣ Prerequisites  
- **R (4.0+ recommended)**  
- **Shiny (for frontend visualization)**  
- **tidyverse, caret, pheatmap, ggplot2, and other R libraries**

### 2️⃣ Clone the Repository  
```sh
git clone https://github.com/yourusername/Metabolomics-Analysis.git
cd Metabolomics-Analysis
```

### 3️⃣ Install Dependencies  
```r
install.packages(c("tidyverse", "pheatmap", "ggplot2", "caret", "shiny"))
```

---

## 🚀 How to Run
### 🏗️ Backend (R-based Analysis)
1. Ensure your dataset is placed inside the **data** folder.
2. Run the main analysis script:
```r
source("run_analysis.R")
```
3. View the generated **biomarker summary**, **statistical outputs**, and **plots** in the output directory.

### 💻 Frontend (Shiny Dashboard)
1. Navigate to the frontend directory:
```sh
cd frontend
```
2. Run the Shiny app:
```r
shiny::runApp()
```
3. Access the interactive dashboard to explore results visually.

---

## 📊 Key Analysis Steps
1️⃣ **Data Preprocessing** – Normalization, transformation, handling missing values.  
2️⃣ **Exploratory Data Analysis (EDA)** – PCA, correlation analysis, statistical summaries.  
3️⃣ **Feature Selection** – Apply ML-based methods (Random Forest, SVM) for biomarker discovery.  
4️⃣ **Statistical Tests** – Perform t-tests, ANOVA, and Wilcoxon rank-sum tests.  
5️⃣ **Visualization** – Generate volcano plots, heatmaps, and boxplots.  

---

## 📈 Sample Output Visualizations
- **PCA Plot** – Visualize metabolite distributions.
- **Volcano Plot** – Identify significantly altered metabolites.
- **Heatmap** – Cluster potential biomarkers.

---

## 📢 Future Enhancements
- 📌 **Integrate Deep Learning for feature extraction**  
- 📌 **Expand dataset support (multi-omics integration)**  
- 📌 **Deploy the frontend as a web-based tool**  

---

## 🧪 Acknowledgments
This project is inspired by recent advancements in **metabolomics research** and **biomarker discovery** methodologies.

---

## 🎉 Enjoy Analyzing! 🚀   