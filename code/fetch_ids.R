# Extract the current Excel assignments and publish snapshots for QC.
# Source with options(cleanAnnotateR.autorun = FALSE) to call fetch_ids() directly.
fetch_ids <- function(filepath, project_dir, data_root = dirname(project_dir),
                      sheet = "Coding_Assignments") {
  required <- c("readxl", "stringr", "tidyr", "dplyr")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "))
  source(file.path(project_dir, "qc_functions.R"), local = TRUE)
  if (!file.exists(filepath)) stop("Excel file not found: ", filepath)
  if (!sheet %in% readxl::excel_sheets(filepath)) stop("Sheet not found: ", sheet)
  hdr <- readxl::read_excel(filepath, sheet = sheet,
      range = readxl::cell_limits(c(1, 1), c(2, NA)),
      col_names = FALSE, col_types = "text", .name_repair = "minimal")
  row1 <- trimws(as.character(hdr[1, ]))
  row2 <- trimws(as.character(hdr[2, ]))
  row1[row1 == ""] <- NA_character_
  month_header <- tidyr::fill(data.frame(val = row1), val)$val
  month_tag <- stringr::str_extract(month_header, "M\\d+")
  fallback <- stringr::str_extract(month_header, "(?<=Month )\\d+")
  use_fallback <- is.na(month_tag) & !is.na(fallback)
  month_tag[use_fallback] <- paste0("M", fallback[use_fallback])
  activity <- !is.na(row2) & grepl("^A[0-9]+$", row2)
  if (any(activity & is.na(month_tag))) stop("Activity header has no valid month.")
  keep <- which(activity)
  if (!length(keep)) stop("No activity columns found.")
  headers <- paste0(month_tag[keep], row2[keep])
  if (anyDuplicated(headers)) stop("Duplicate month/activity headers.")

  # Read from A3 explicitly, so leading blank cells cannot shift the columns.
  dat <- readxl::read_excel(filepath, sheet = sheet,
      range = readxl::cell_limits(c(3, 1), c(NA, ncol(hdr))),
      col_names = FALSE, col_types = "text", .name_repair = "minimal")
  out <- as.data.frame(dat[, c(1, keep)], stringsAsFactors = FALSE)
  names(out) <- c("ID", headers)
  out$ID <- trimws(out$ID)
  out <- out[!is.na(out$ID) & nzchar(out$ID), , drop = FALSE]
  if (any(!grepl("^TD[0-9]+$", out$ID))) stop("Invalid subject IDs in workbook.")
  if (anyDuplicated(out$ID)) stop("Duplicate subject IDs in workbook.")
  long <- tidyr::pivot_longer(out, cols = -ID, names_to = "combined", values_to = "flag")
  long$flag <- trimws(long$flag)
  # Blank and '-' mean unselected; do not parse arbitrary text as a number.
  unusual <- !is.na(long$flag) & !long$flag %in% c("", "-", "0", "1")
  issues <- as.data.frame(long[unusual, c("ID", "combined", "flag")])
  selected <- long[!is.na(long$flag) & long$flag == "1", , drop = FALSE]
  tab <- data.frame(ID = selected$ID, combined = selected$combined,
                    status = rep(1, nrow(selected)),
                    month = stringr::str_extract(selected$combined, "M\\d+"),
                    act = stringr::str_extract(selected$combined, "A\\d+"))
  tab$prefix <- if (nrow(tab)) paste0(tab$ID, "-", tab$month, tab$act) else character()
  tab$path <- file.path("Data", tab$ID, paste0(tab$ID, "_", tab$month))
  tab$last_appeared_on_sheet <- rep(as.character(Sys.Date()), nrow(tab))
  tab$was_reviewed <- rep(FALSE, nrow(tab))

  state_dir <- file.path(data_root, "processed", "qc_state")
  lock <- acquire_qc_lock(state_dir)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  previous <- read_state(file.path(state_dir, "reference_current.tsv"))
  if (is.null(previous)) previous <- read_state(file.path(state_dir, "reference_log.tsv"))
  if (!is.null(previous) && !"prefix" %in% names(previous)) stop("Invalid previous reference snapshot.")
  new <- tab[!tab$prefix %in% previous$prefix, , drop = FALSE]
  removed <- if (is.null(previous)) tab[0, , drop = FALSE] else
    previous[!previous$prefix %in% tab$prefix, , drop = FALSE]
  atomic_write_tsv(issues, file.path(state_dir, "assignment_issues.tsv"))
  atomic_write_tsv(new, file.path(state_dir, "reference_new.tsv"))
  atomic_write_tsv(removed, file.path(state_dir, "reference_removed.tsv"))
  # Commit the authoritative snapshot last. QC does not depend on the diff files.
  atomic_write_tsv(tab, file.path(state_dir, "reference_current.tsv"))
  message(nrow(tab), " selected assignments; ", nrow(issues), " unexpected flag(s).",
          if (nrow(issues)) " See qc_state/assignment_issues.tsv." else "")
  invisible(tab)
}

if (!identical(getOption("cleanAnnotateR.autorun"), FALSE)) {
  project_dir <- Sys.getenv("QC_PROJECT_DIR")
  if (!nzchar(project_dir)) {
    here::i_am("qc_project.Rproj")
    project_dir <- here::here()
  }
  fetch_ids(Sys.getenv("QC_EXCEL_PATH", file.path(dirname(dirname(project_dir)),
                                                 "Reach_Assignments_2.xlsx")),
            project_dir, data_root = Sys.getenv("QC_DATA_ROOT", dirname(project_dir)),
            sheet = Sys.getenv("QC_SHEET", "Coding_Assignments"))
}
