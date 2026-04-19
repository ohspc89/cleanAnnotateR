# 🧹 cleanAnnotateR: ELAN Behavior Coding QC Pipeline

This repository contains a modular, R-based ETL and quality control (QC) pipeline developed to validate behavioral annotation data exported from [ELAN](https://archive.mpi.nl/tla/elan), used in infant motor control research at CHLA.

## 🧰 What It Does

This project:
- Extracts assignment data from a multi-month Excel tracker
- Transforms and maps coder annotations to file paths
- Validates annotation `.txt` files through **three structured quality checks**
- Outputs detailed logs and `.tsv` summaries for further review
- Coder-specific report generated inside `qc_by_coder/` folder
- Identifies *newly* assigned videos since the last run
  (full review at the first week of a month; incremental review the rest)

---

## ✅ Quality Checks Performed

1. **Last Offset Consistency**  
   All tiers within a `.txt` file should share the same final offset time.

2. **Onset-Offset Continuity**  
   Each row’s offset should match the next row’s onset (per tier) for continuous annotation.

3. **Label Format Validation**  
   Labels must be capitalized and match a predefined grammar (e.g., `MO`, `MDT`, `Q`).

---

## 🧩 File Structure

```
cleanAnnotateR/
├── .gitignore
├── LICENSE
├── code/
│   ├── fetch_ids.R         # Extract IDs & paths from Excel
│   ├── perform_qc.R        # Run QC procedures on annotation files
│   ├── qc_project.Rproj    # R Project (or .here) file to work with `here`
│   └── qc_functions.R      # Core QC logic
├── docs/                   # Related documentation
│   └── schema/             # Metadata of output .tsv
├── processed/              # Output .tsv and log files (gitignored)
│   ├── qc_state/           # Internal state for tracking new/removed assignments
│   ├── qc_by_coder/        # Personalized feedback reports for lab volunteers
│   └── reference.tsv       # The master mapping of subjects to file paths
└── README.md
```

---

## 🚀 How to Run

1. Ensure the following folder (the exact path may be different) is synced to your local machine via OneDrive:  
   `~/Library/CloudStorage/OneDrive-ChildrensHospitalLosAngeles/EEG reaching R01/Analysis/Behavior Coding/Reach & Grasp/`

2. Run the following in R:

```r
source("code/fetch_ids.R")   # Extract subject-month-activity mappings
source("code/perform_qc.R")  # Perform all 3 quality checks
```

---

## 📁 Example Output Files

| File Name                     | Description                           |
|-------------------------------|---------------------------------------|
| `qc_offset.tsv`               | Files failing the offset match check  |
| `qc_continuous.tsv`           | Rows with onset-offset mismatch       |
| `qc_labels.tsv`               | Improperly formatted labels           |
| `quality_check_summary.log`   | Full log of the QC session            |
| `failed_files.tsv`            | Files that could not be processed     |
| `unexpected_filenames.log`    | File names that are not marked in the |
|                               | master spreadsheet or have typos      |

---

## 🔍 Sample Output (QC)

**`qc_labels.tsv`**
```
filename              row#   label
TD14-M3_A2R2_CC.txt    25    mdx
TD17-M3_A3R2_AG.txt    11    Mo
```

**`qc_continuous.tsv`**
```
filename             tier  rows   prev_value  next_value
TD03-M4_A5R1_CC.txt   LA   19-20  59340       60000
```

---

## 📌 Real-World Context

This pipeline was developed as part of an NIH-funded research project at **Children’s Hospital Los Angeles**. It supports robust preprocessing of behavioral data used to analyze infant reach-and-grasp behavior via sensorimotor and EEG measurements.

---

## ✨ Highlights

- Parses structured assginment metrices from Excel
- Navigates **multi-level file structures** to locate and validate annotation files
- Built-in error handling and custom logging
- Modularized QC functions for reuse and testing
- Prepares structured reference tables for pipeline-ready downstream use

---

## Tip

Please ensure that OneDrive status icon shows "Synced" before running `fetch_ids.R`. There's a 'Sync Delay' often encountered with OneDrive.

---

## 📈 Skills Demonstrated

- R programming (modular scripting, `tryCatch()`, string parsing)
- ETL logic (extract-transform-load)
- Behavioral data validation & reproducibility
- Working with cloud-synced file systems (OneDrive)

---

## 🧑‍💻 Author

**Jinseok Oh**  
Postdoctoral Research Fellow @ CHLA  
📫 [joh@chla.usc.edu](mailto:joh@chla.usc.edu)
