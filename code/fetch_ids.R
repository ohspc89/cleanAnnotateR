#' Build Reference Table for Reach & Grasp Coding Video Anootations
#'
#' This script reads an Excel workbook with a two-row header that encodes
#' months and activities (e.g., \code{M3} and \code{A2}) for each subject,
#' identifies selected assignments (cells marked with \code{1}),
#' and produces a tab-separated reference table with subject, month, activity,
#' filename prefix, and target path. The output is written to the synchronized
#' OneDrive/SharePoint "Behavior Coding" folder under
#' \code{"Reach & Grasp/processed/reference.tsv"}.
#'
#' @section Inputs:
#' \itemize{
#'   \item \strong{Excel file} (default: \code{"Reach_Assignments_2.xlsx"}), located in the
#'         OneDrive "Behavior Coding" folder. The name can be changed by editing
#'         \code{EXCEL_FILE}.
#'   \item \strong{Sheet name} (default: \code{"Coding_Assignments"}). Can be changed via
#'         \code{SHEET_NAME}.
#'   \item Row 1 contains merged month labels (e.g., \code{"Month 3 (M3)"}).
#'         Row 2 contains activity codes as \code{"A#"} (e.g., \code{"A2"}).
#'   \item Data rows begin at row 2 (after headers), where column 1 is \code{ID}
#'         (e.g., \code{"TD17"}) and activity columns contain selection flags
#'         (numeric \code{1}).
#' }
#'
#' @section Outputs:
#' A tab-separated file \code{reference.tsv} with the columns:
#' \describe{
#'   \item{subj}{Subject ID (e.g., \code{"TD17"}).}
#'   \item{month}{Month tag (e.g., \code{"M3"}, \code{"M7"}).}
#'   \item{act}{Activity tag (e.g., \code{"A2"}, \code{"A7"}).}
#'   \item{prefix}{Filename prefix of the form \code{<ID>-<M#>_<A#>} (e.g., \code{"TD17-M3_A2"}).}
#'   \item{path}{Target relative path (e.g., \code{"Data/TD17/TD17_M3"}).}
#' }
#'
#' @author
#' Maintainer: Jinseok Oh <joh@chla.usc.edu>
#' Last edited: 2/21/2026

# Don't forget to install tidyr and dplyr in people's computers...
if (!requireNamespace("Require", quietly = T)) install.packages("Require")
package_list = c('readxl', 'stringr', 'fs', 'tidyr', 'dplyr', 'here')
Require::Require(package_list, require=T, cranCache=T)

# Settings
# -----------------------------------------
EXCEL_FILE = "Reach_Assignments_2.xlsx"
SHEET_NAME = "Coding_Assignments"
# -----------------------------------------

# (4/17/26) Using `here` package to find the top-level directory of your PROJECT,
# regardless of the computer or OS.
here::i_am("qc_project.Rproj")

# -----------------------------------------

# This will make the full file path
filepath <- file.path(dirname(dirname(here())), EXCEL_FILE)

if (!file.exists(filepath)){
    stop("Error: Excel file not found at: ", filepath)
}

# Read from the spreadsheet: Coding_Assignments_2.xlsx
month_tbl <- {
    if (!SHEET_NAME %in% readxl::excel_sheets(filepath)) {
        stop("Sheet '", SHEET_NAME, "' not found in: ", filepath)
    }
    # 1. Read the header rows (1 & 2) separately to build names (months + activities)
    hdr <- read_excel(filepath, sheet=SHEET_NAME,
                      range=readxl::cell_limits(c(1,1), c(2,NA)),
                      col_names=FALSE)

    row1 <- hdr[1, ] |> as.character()
    row2 <- hdr[2, ] |> as.character()

    # Treat empty strings as NA <- this necessary?
    row1[row1 == ""] <- NA
    row2[row2 == ""] <- NA

    # Forward-fill the Month header across merged cells
    month_header <- tibble(val=row1) |>
        fill(val, .direction = "down") |>
        pull(val)

    # Expect either "M3", "M10" or labels like "Month 3", "Month 10"
    month_tag <- str_extract(month_header, "M\\d+") %>%
        ifelse(
            is.na(.),
            paste0("M", str_extract(month_header, "(?<=Month )\\d+")),
            .
        )

    # Identify activity columns: those with A#, under a month
    is_activity <- str_detect(row2, "^A\\d+$") & !is.na(month_tag)

    keep_idx <- which(is_activity)
    new_names <- paste0(month_tag[keep_idx], row2[keep_idx])

    # 2. Read the actual data rows
    # Use col_types to force columns to be numeric/double where needed
    # Column 1 is ID (text), the rest are activities (numeric)

    dat <- read_excel(filepath, sheet=SHEET_NAME, skip=2)

    # Build final dataset
    out <- dat[, c(1, keep_idx)]
    names(out) <- c("ID", new_names)
    # Reject rows without valid IDs
    out <- out[!is.na(out$ID), ]
    out <- out %>%
        mutate(across(-ID, ~ readr::parse_number(as.character(.))))
}

#############################################
# Data Wrangling:                           #
#   - actual information extraction happens #
#############################################

# Reference table will have the following columns:
#   - ID (chr; ex. TD02)
#   - combined (chr; ex. M1A2)
#   - status (dbl - 0, 1, or NA; if 1, coder marked the video to have been coded)
#   - month (chr; ex. M1)
#   - act (chr; ex. A2)
#   - prefix (chr; ex. TD02-M1_A2)
#   - path (chr; ex. Data/TD02/TD02_M1)
tab <- month_tbl %>%
    pivot_longer(cols = -ID, names_to = "combined", values_to = "status") %>%
    filter(status == "1") %>%
    mutate(
           month = str_extract(combined, "M\\d+"),
           act = str_extract(combined, "A\\d+"),
           prefix = paste0(ID, "-", month, "_", act),
           path = file.path("Data", ID, paste0(ID, "_", month))
           )

# save the reference to a tab separated file
# (1/20/26) Describing the path..
# |- onedrive_path
# |  |- Reach & Grasp
# |  |  |- Quality Check
# |  |  |  |- R scripts (ex. fetch_ids.R)
# |  |  |- processed
# |  |  |  |- R script processed output (ex. reference.tsv)
# |  |  |- Data
# |  |  |  |- Coded data
reference_path = file.path(dirname(here()), "processed/reference.tsv")
qc_state_dir   = file.path(dirname(here()), "processed/qc_state")
dir.create(qc_state_dir, showWarnings=FALSE, recursive=TRUE)

prev_reference_path = file.path(qc_state_dir, "reference_prev.tsv")
diff_new_path	    = file.path(qc_state_dir, "reference_new.tsv")
diff_removed_path   = file.path(qc_state_dir, "reference_removed.tsv")

# Load previous reference if exists
prev <- NULL
if (file.exists(prev_reference_path)) {
    prev <- read.delim(prev_reference_path, sep="\t", stringsAsFactors=FALSE)
}

# Compute diffs (use prefix as unique key)
if (!is.null(prev) && "prefix" %in% names(prev)){
    new_rows     <- tab[!(tab$prefix %in% prev$prefix), , drop=FALSE]
    removed_rows <- prev[!(prev$prefix %in% tab$prefix), , drop=FALSE]
} else {
    new_rows <- tab
    removed_rows <- tab[0, , drop=FALSE]
}

# Save diff outputs
write.table(new_rows, file=diff_new_path, sep="\t",
	    row.names=FALSE, col.names=TRUE, quote=FALSE)
write.table(removed_rows, file=diff_removed_path, sep="\t",
	    row.names=FALSE, col.names=TRUE, quote=FALSE)

# Write the current reference (as before)
write.table(tab, file=reference_path, sep="\t",
	    row.names=FALSE, col.names=TRUE, quote=FALSE)

# Update snapshot for next run
write.table(tab, file=prev_reference_path, sep= "\t",
            row.names=F, col.names=T, quote=F)
