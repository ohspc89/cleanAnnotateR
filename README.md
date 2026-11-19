# 🧹 cleanAnnotateR: ELAN Behavior Coding QC Pipeline

An R-based quality-control pipeline for ELAN behavioral annotation exports, with Python/Prefect orchestration for scheduled execution and run monitoring. It reconciles Excel coding assignments with files on disk, performs annotation checks, and produces coder reports for infant motor control research at Children's Hospital Los Angeles (CHLA).

## Workflow orchestration with Prefect

The R-based QC pipeline can be orchestrated using Python and Prefect. The orchestration layer coordinates execution, scheduling, and run monitoring without replacing the existing R validation and state-management logic.

#### Execution sequence

1. `fetch_ids.R` - Refresh the authoritative assignment snapshot from Excel.
2. `perform_qc.R` - Discover annotation exports, reconcile assignments, execute QC, and publish reports.
3. Prefect - Record task and flow execution states, durations, and failures.

The second task runs only after the first completes successfully. A nonzero R process exit code causes the corresponding Prefect task to fail.

### Local deployment

Install the Python dependencies and start the Prefect server from the repository root:
```sh
uv sync
uv run prefect server start
```

In a separate terminal, configure the local Prefect API and the directory containing the operational RStudio project:

```sh
export PREFECT_API_URL="http://127.0.0.1:4200/api"
export QC_R_PROJECT_DIR="/path/to/OneDrive/Reach & Grasp/Quality Check"

uv run python orchestration/flow.py
```

The deployment `cleanAnnotateR QC/local-qc` uses the cron expression `0 14 * * 1-5`, corresponding to 2:00 PM Monday–Friday in the `America/Los_Angeles` time zone.

To trigger a run manually:

```sh
uv run prefect deployment run 'cleanAnnotateR QC/local-qc'
```

Open the Prefect UI at http://127.0.0.1:4200 to inspect deployment schedules, flow runs, task states, execution durations, and failures.

### QC mode and operational requirements

Prefect controls **when** the pipeline runs, while R determines **which QC mode** to use. When no mode override is supplied, R performs full QC during the first Monday-Friday period of each month and incremental QC on other days.

Scheduled execution requires the Mac to remain awake, the Prefect server and serving process to remain running, and the OneDrive project files to be accessible. This is a local deployment, not an always-on cloud service.

A completed Prefect flow indicates successful script execution; it does not mean every annotation passed QC. Annotation issues and processing failures are documented in the R-generated reports.


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

Annotation filenames must follow `TD<ID>-M<month>A<activity>[R<repetition>]_<CODER>.txt`, where the repetition suffix is optional, the month is M1–M7, and the coder ID consists of two or three uppercase letters. For example, `TD17-M3A2R2_CC.txt` and `TD17-M3A2_CC.txt` are valid names. Files must reside directly in the corresponding `Data/<ID>/<ID>_M<month>/` directory. Discovery reports invalid filenames, files placed in the wrong subject/month directory, and files with valid names whose assignment prefixes are absent from the current Excel snapshot. Only correctly located files with registered assignments are eligible for annotation QC.

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
| `qc_summary.tsv` | Eligible annotation exports (including passing/skipped files) and file, assignment, and directory discovery issues |
| `discovery_issues.tsv` | Unregistered or unavailable directories, listing errors, invalid filenames, wrong-directory files, unregistered assignments, assignments without matching exports, and previously recorded files now missing/inaccessible |
| `per_file/Data/.../<filename>.txt_offset_error.txt` | File whose final offsets disagree |
| `per_file/Data/.../<filename>.txt_cont_issues.csv` | Continuity messages with tier-relative row numbers |
| `per_file/Data/.../<filename>.txt_label_issues.csv` | `row`, `label`, `UPPER`, and `PROPER` columns |
| `failed_files.tsv` | File paths and parsing, validation, or processing errors |
| `qc_by_coder/qc_issues_<CODER>.txt` | Processing failures, offset issues, label issues, and continuity issues grouped by coder |

Open **`qc_summary.tsv` first** in Excel or another spreadsheet viewer. It includes `filename`, `path`, `prefix`, `coder`, `status`, `checked_this_run`, `last_qc`, `report_dir`, and `detail`. Unknown coder IDs are left blank/NA rather than guessed. `report_dir` refers to a folder under `processed/`; for skipped files it points to their previous check's reports.

| Summary status | Meaning |
|----------------|---------|
| `passed` | Checked and passed this run |
| `qc_issues` | Checked; offset, continuity, or label problems found |
| `processing_failed` | Reading, validation, or processing failed; see `detail` |
| `skipped_previous_pass` | Previously passed with unchanged metadata; not checked this run |
| `invalid_filename` | Discovered `.txt` file does not follow the required annotation filename convention |
| `wrong_directory` | Filename identifies a subject/month different from the directory containing the file |
| `unregistered_assignment` | Validly named file in the expected directory, but its assignment prefix is absent from the Excel snapshot |
| `directory_unregistered` | Subject/month directory exists but is not listed among current assignment directories |
| `missing_or_inaccessible` | A previously recorded file was not found during discovery |
| `no_matching_file` | No matching export was found for the assignment in its assigned folder |
| `directory_unavailable` | Assigned directory is not visible on this computer |
| `listing_failed` | Reading the directory listing failed |

`row_type` distinguishes `file`, `assignment`, and `directory` rows. An assignment gap and its unavailable directory can both appear, so the total row count is not a file count. File absence here does **not** prove that no upload occurred: discovery examines recognized subject/month directories under the local `Data/` tree, but does not inspect arbitrary locations or OneDrive upload history. A header-only summary is still written when there are no assignments or files.

Detailed issue reports cover files checked in that run. Discovery issues are separate from coder reports because a missing, invalid, or unregistered file may not identify a coder. QC violations and processing failures are recorded separately; both cause incremental retries.

Generated `processed/` folders, `.tsv` files, `.log` files, and Excel workbooks are gitignored. Existing documents in `docs/schema/` may describe older output formats.

## Regression checks

From the repository root, with the dependencies installed:

```sh
Rscript tests/regression.R
```

Tests use temporary files and state, including real parallel workers. If the workbook copy exists in `docs/`, they also check extraction of its 1,529 selected assignments. The workbook is not modified.

## Research context and author

Developed as part of an NIH-funded project at Children's Hospital Los Angeles for infant reach-and-grasp research using behavioral coding and EEG measurements.

**Jinseok Oh**, Postdoctoral Research Fellow @ CHLA

[joh@chla.usc.edu](mailto:joh@chla.usc.edu)
