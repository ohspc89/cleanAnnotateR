# 🧹 cleanAnnotateR: ELAN Behavior Coding QC Pipeline

An R pipeline for extracting Excel assignments, checking behavioral annotation exports from ELAN, and producing coder reports for infant motor control research at Children's Hospital Los Angeles (CHLA).

## Workflow

1. `code/fetch_ids.R` reads the current Excel assignments and saves a reference snapshot.
2. `code/perform_qc.R` finds matching exports, checks selected files in parallel, writes reports, and then saves per-file QC state.
3. Review the timestamped report folder, including discovery issues and coder reports. Fix annotation files and rerun.

The scripts choose a QC mode based on the run date; they do not schedule runs.

## Setup

Use R 4.1 or later. Install dependencies once:

```r
install.packages(c("readxl", "stringr", "tidyr", "dplyr", "fs", "purrr",
                   "future", "future.apply", "here"))
```

The usual OneDrive deployment layout is:

```text
Behavior Coding/
├── Reach_Assignments_2.xlsx
└── Reach & Grasp/
    ├── Quality Check/
    │   ├── qc_project.Rproj
    │   ├── fetch_ids.R
    │   ├── perform_qc.R
    │   └── qc_functions.R
    ├── Data/TD17/TD17_M3/TD17-M3A2R2_CC.txt
    └── processed/
        ├── qc_state/
        └── qc_performed_<timestamp>_<unique suffix>/
```

Copy the three R scripts into `Quality Check/` and create an RStudio project named `qc_project.Rproj` there. Open that project or run from its directory:

```sh
Rscript fetch_ids.R
Rscript perform_qc.R
```

In RStudio, `source("fetch_ids.R")` followed by `source("perform_qc.R")` does the same. Neither script changes the working directory or terminates the R session when there is nothing to check.

For another layout, set paths explicitly. For example, from this repository's root in R:

```r
Sys.setenv(
  QC_PROJECT_DIR = normalizePath("code"),
  QC_EXCEL_PATH = normalizePath("docs/Reach_Assignments_2 - Copy.xlsx"),
  QC_DATA_ROOT = "/absolute/path/to/Reach & Grasp"
)
source("code/fetch_ids.R")
source("code/perform_qc.R")
```

`QC_PROJECT_DIR` identifies the scripts directory and bypasses the project-file requirement. `QC_DATA_ROOT` contains `Data/` and receives `processed/`; it defaults to the scripts directory's parent. `QC_EXCEL_PATH` defaults to `Reach_Assignments_2.xlsx` two directories above the scripts directory. `QC_SHEET` defaults to `Coding_Assignments`. The scripts do not automatically discover OneDrive.

Ensure the workbook and exports are available locally. File discovery uses metadata; reading selected files may trigger OneDrive downloads.

## Assignment workbook

The first two rows contain month and activity headers. The first column contains unique subject IDs such as `TD17`. Activity headers are `A2`, `A3`, etc., under month labels such as `Month 3 (M3)`.

- Exactly `1` selects an assignment; `0`, blank cells, and `-` do not.
- Other values are excluded and recorded in `qc_state/assignment_issues.tsv` for review. Arbitrary text containing a digit is not treated as a selection.
- Invalid or duplicate subject IDs, duplicate month/activity columns, and activity headers without a valid month stop extraction before replacing the current snapshot.
- Footer rows without subject IDs are ignored.

Extraction writes `reference_current.tsv`, `reference_new.tsv`, and `reference_removed.tsv`. The current snapshot is authoritative: assignments removed from the workbook are no longer scanned. Diff files compare with the preceding snapshot, or the legacy reference log on the first upgraded run.

## Annotation checks

Exports have no header and contain either five columns (`tier`, `onset`, `offset`, `duration`, `label`) or six (`tier`, `activity`, `onset`, `offset`, `duration`, `label`).

Before QC, the reader rejects empty or malformed files, missing tiers, nonnumeric/missing/infinite/negative times, offsets before onsets, and durations inconsistent with `offset - onset` (absolute tolerance `1e-7`). Validation failures appear in `failed_files.tsv` and are retried on the next run.

The three QC checks are:

1. **Final offsets:** the last offset must agree across tiers.
2. **Continuity:** each row's offset must equal the next row's onset within its tier. Original row order is used; a single-row tier has no adjacent pair to check.
3. **Labels:** allowed values are exactly `N`, `Z`, `Q`, `MO`, `MDT`, `MDG`, and `MDX`. Label reports include the original row number.

Filenames must begin with an assignment prefix such as `TD17-M3A2`, followed by a repetition (`R1`, etc.), an underscore, or `.txt`. Files must be directly inside their assigned `Data/<ID>/<ID>_<month>/` folder. Unmatched filenames and files in the wrong assignment folder are reported. Coder IDs normally appear as two or three uppercase letters after the last underscore and before `.txt`.

The pipeline cannot infer which tiers or repetitions should exist from the current workbook. It reports assignments with no matching files and previously recorded files that disappear, but does not prove that every expected repetition or tier was exported.

## Full and incremental modes

Full mode runs from the month's first Monday through the following Friday; other dates default to incremental mode. Both modes use all current assignments, including newly selected ones, and work on the first run.

```sh
Rscript perform_qc.R --full
Rscript perform_qc.R --incremental
```

In RStudio, set `Sys.setenv(QC_FULL = "1")` for full or `"0"` for incremental before sourcing; use `Sys.unsetenv("QC_FULL")` to restore date-based selection. Explicit command-line flags take precedence.

Incremental mode checks new files, files whose size or modification time changed, files that failed processing, and files with unresolved QC issues. It skips only previously passing files with unchanged metadata. A new file is checked even when its modification time predates the previous run.

`qc_state/file_log.tsv` records each relative file path, assignment prefix, observed size and modification time, processing status, QC pass status, check time, and report directory. Assignment-level `reference_log.tsv` remains a summary, not the basis for file selection. Its `last_qc` is the summary refresh time. When upgrading from older assignment-only state, files are checked again to establish per-file history. Rerun extraction to establish the authoritative current assignment snapshot; until then, QC falls back to the legacy new/reference logs.

Metadata comparison cannot detect edits that preserve both size and modification time; use a full run in that case. A previously recorded file that disappears stays flagged until restored or its state is deliberately reconciled.

## Parallel execution and saving

Set `QC_WORKERS` to a positive integer (default `2`); use `1` for sequential execution. Parallel runs use separate R sessions through `future::multisession`. The previous future plan is restored on both success and failure.

Workers return results; only the main process writes reports. Per-file outputs mirror the source directory structure to prevent filename collisions. Files that change while being checked are treated as processing failures.

Reports finish before state is saved. Each state file is staged beside its destination and replaced using a rename; if replacement fails, the previous file is retained and the run stops. The per-file log is committed last and is the authoritative checkpoint. Multiple state files are not a single transaction, so a failed commit can cause safe rechecking.

Extraction and QC share a lock directory (`qc_state/.qc.lock`) to prevent concurrent state updates. It is removed on normal completion or errors; after a hard process crash, remove a stale lock only after confirming no run remains active. A failed run can leave a partial report folder, but it does not advance the per-file checkpoint before reports finish.

## Reports

Each run creates `processed/qc_performed_<timestamp>_<unique suffix>/`. Issue files are written only when corresponding issues occur:

| File | Contents |
|------|----------|
| `discovery_issues.tsv` | Missing directories, listing errors, assignments without exports, unmatched filenames, and previously recorded files now missing/inaccessible |
| `per_file/Data/.../<filename>.txt_offset_error.txt` | File whose final offsets disagree |
| `per_file/Data/.../<filename>.txt_cont_issues.csv` | Continuity messages with tier-relative row numbers |
| `per_file/Data/.../<filename>.txt_label_issues.csv` | `row`, `label`, `UPPER`, and `PROPER` columns |
| `failed_files.tsv` | File paths and parsing, validation, or processing errors |
| `qc_by_coder/qc_issues_<CODER>.txt` | Processing failures, offset issues, label issues, and continuity issues grouped by coder |

Reports cover files checked in that run. Discovery issues are separate from coder reports because a missing or unmatched file may not identify a coder. QC violations and processing failures are recorded separately; both cause incremental retries.

Generated `processed/` folders, `.tsv` files, `.log` files, and Excel workbooks are gitignored. Existing documents in `docs/schema/` may describe older output formats.

## Regression checks

From the repository root, with the dependencies installed:

```sh
Rscript tests/regression.R
```

Tests use temporary files and state, including real parallel workers. If the workbook copy exists in `docs/`, they also check extraction of its 1,529 selected assignments. The workbook is not modified.

## Research context and author

Developed as part of an NIH-funded project at Children's Hospital Los Angeles for infant reach-and-grasp research using sensorimotor and EEG measurements.

**Jinseok Oh**, Postdoctoral Research Fellow @ CHLA

[joh@chla.usc.edu](mailto:joh@chla.usc.edu)
