# Run ELAN QC, publish reports, then commit per-file processing state.
# Source with options(cleanAnnotateR.autorun = FALSE) to use run_qc() directly.

run_qc <- function(project_dir, data_root = dirname(project_dir), full = NULL,
                   workers = NULL) {
  required <- c("fs", "stringr", "purrr", "dplyr", "future.apply", "future")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "))
  # Existing report helpers use these attached functions.
  suppressPackageStartupMessages(library(purrr))
  suppressPackageStartupMessages(library(dplyr))
  source(file.path(project_dir, "qc_functions.R"), local = TRUE)
  data_root <- normalizePath(data_root, winslash = "/", mustWork = TRUE)
  processed_path <- file.path(data_root, "processed")
  qc_state_dir <- file.path(processed_path, "qc_state")
  lock <- acquire_qc_lock(qc_state_dir)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)

  if (is.null(full)) {
    today <- Sys.Date()
    first <- as.Date(format(today, "%Y-%m-01"))
    monday <- first + (8L - as.integer(format(first, "%u"))) %% 7L
    full <- today >= monday && today <= monday + 4
    env <- Sys.getenv("QC_FULL")
    args <- commandArgs(trailingOnly = TRUE)
    if (nzchar(env) && !env %in% c("0", "1")) stop("QC_FULL must be 0 or 1.")
    if (nzchar(env)) full <- env == "1"
    if (all(c("--full", "--incremental") %in% args)) stop("Choose only one QC mode.")
    if ("--full" %in% args) full <- TRUE
    if ("--incremental" %in% args) full <- FALSE
  }
  if (length(full) != 1L || is.na(full) || !full %in% c(TRUE, FALSE))
    stop("full must be TRUE or FALSE.")
  cat("QC mode:", if (full) "FULL" else "INCREMENTAL", "\n")

  ref_log <- file.path(qc_state_dir, "reference_log.tsv")
  current <- read_state(file.path(qc_state_dir, "reference_current.tsv"))
  if (is.null(current)) {
    # Migrate old installations without trusting assignment-level timestamps.
    current <- dplyr::bind_rows(read_state(file.path(qc_state_dir, "reference_new.tsv")),
                                read_state(ref_log))
    if (ncol(current)) current <- current[!duplicated(current$prefix), , drop = FALSE]
  }
  if (!all(c("prefix", "path") %in% names(current)))
    stop("No valid reference snapshot. Run fetch_ids.R first.")
  references <- current
  if (anyNA(references$prefix) || anyDuplicated(references$prefix) ||
      anyNA(references$path) || any(!nzchar(references$path)))
    stop("Reference prefixes must be unique and paths must be nonempty.")
  if (any(!grepl("^TD[0-9]+-M[0-9]+A[0-9]+$", references$prefix)) ||
      any(!grepl("^Data[/\\\\]TD[0-9]+[/\\\\]TD[0-9]+_M[0-9]+$", references$path)))
    stop("Unexpected assignment prefix or relative data path.")
  references$path <- gsub("\\\\", "/", references$path)

  file_log <- file.path(qc_state_dir, "file_log.tsv")
  state <- read_state(file_log)
  if (is.null(state)) state <- data.frame(path = character(), prefix = character(),
      size = double(), mtime = character(), processed_ok = logical(), qc_passed = logical(),
      last_qc = character(), report_dir = character())
  if (!all(c("path", "prefix", "size", "mtime", "processed_ok", "qc_passed",
             "last_qc", "report_dir") %in% names(state)) || anyDuplicated(state$path))
    stop("Invalid per-file state log: ", file_log)

  discovery <- data.frame(path = character(), issue = character(),
                          row_type = character(), status = character(), prefix = character())
  record_issue <- function(path, issue, row_type, status, prefix = NA_character_) {
    discovery <<- rbind(discovery, data.frame(path = path, issue = issue,
                        row_type = row_type, status = status, prefix = prefix))
  }
  candidates <- data.frame(path = character(), prefix = character(),
                           size = double(), mtime = character())
  for (subdir in unique(references$path)) {
    folder <- file.path(data_root, subdir)
    if (!dir.exists(folder)) {
      record_issue(subdir, "Assigned directory not visible on this computer",
                   "directory", "directory_unavailable")
      next
    }
    info <- tryCatch(fs::dir_info(folder, type = "file"),
                     error = function(e) {
                       record_issue(subdir, paste("Directory listing failed:", conditionMessage(e)),
                                    "directory", "listing_failed")
                       NULL
                     })
    if (is.null(info)) next
    info <- info[grepl("\\.txt$", info$path, ignore.case = TRUE), ]
    for (j in seq_len(nrow(info))) {
      bn <- basename(info$path[j])
      prefix <- stringr::str_extract(bn, "^TD\\d+-M\\d+A\\d+(?=R\\d|_|\\.txt$)")
      rel <- paste(subdir, bn, sep = "/")
      if (is.na(prefix) || !any(references$prefix == prefix & references$path == subdir)) {
        record_issue(rel, "Unmatched filename or assignment folder",
                     "file", "unmatched", prefix)
        next
      }
      metadata <- file.info(file.path(data_root, rel))
      candidates <- rbind(candidates, data.frame(path = rel, prefix = prefix,
                         size = as.numeric(metadata$size),
                         mtime = sprintf("t%.6f", as.numeric(metadata$mtime))))
    }
  }
  for (i in which(!references$prefix %in% candidates$prefix))
    record_issue(paste(references$path[i], references$prefix[i], sep = "/"),
                 "No matching annotation files found in the assigned folder during this run",
                 "assignment", "no_matching_file", references$prefix[i])
  absent <- state$prefix %in% references$prefix & !state$path %in% candidates$path
  for (i in which(absent & !state$path %in% discovery$path))
    record_issue(state$path[i], "Previously recorded file is missing or inaccessible",
                 "file", "missing_or_inaccessible", state$prefix[i])
  state$processed_ok[absent] <- FALSE
  state$qc_passed[absent] <- FALSE

  previous <- match(candidates$path, state$path)
  unchanged <- !is.na(previous) & state$processed_ok[previous] %in% TRUE &
    state$qc_passed[previous] %in% TRUE &
    candidates$size == state$size[previous] & candidates$mtime == state$mtime[previous]
  unchanged[is.na(unchanged)] <- FALSE
  selected <- candidates[full | !unchanged, , drop = FALSE]
  txt_files <- file.path(data_root, selected$path)
  cat("Files to check:", length(txt_files), "\n")

  dir.create(processed_path, showWarnings = FALSE, recursive = TRUE)
  # A unique suffix avoids collisions between runs started within one second.
  qc_by_date <- tempfile(paste0("qc_performed_", format(Sys.time(), "%Y-%m-%d_%H%M%S"), "_"),
                         tmpdir = processed_path)
  if (!dir.create(qc_by_date)) stop("Cannot create report directory.")
  old_options <- options(warn = 2)
  on.exit(options(old_options), add = TRUE)
  if (nrow(discovery)) atomic_write_tsv(discovery, file.path(qc_by_date, "discovery_issues.tsv"))

  process_qc <- function(txt) {
    tryCatch({
      before <- file.info(txt)
      out <- qc.all(txt)
      after <- file.info(txt)
      if (anyNA(c(before$size, after$size, before$mtime, after$mtime)) ||
          before$size != after$size || before$mtime != after$mtime)
        stop("File changed during QC; retry on the next run.")
      passed <- isTRUE(out$last_offsets_match) &&
        identical(out$continuously_coded, "No onset-offset mismatch found") &&
        nrow(out$proper_labels) == 0L
      list(success = TRUE, passed = passed, filename = txt, data = out,
           size = as.numeric(before$size), mtime = sprintf("t%.6f", as.numeric(before$mtime)), error = NULL)
    }, error = function(e) list(success = FALSE, passed = FALSE, filename = txt,
                                data = NULL, error = conditionMessage(e)))
  }
  if (is.null(workers)) workers <- Sys.getenv("QC_WORKERS", "2")
  workers <- suppressWarnings(as.numeric(workers))
  if (length(workers) != 1L || !is.finite(workers) || workers < 1 || workers != floor(workers))
    stop("QC_WORKERS must be a positive integer.")
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  if (workers == 1 || length(txt_files) < 2) {
    all_results <- lapply(txt_files, process_qc)
  } else {
    future::plan(future::multisession, workers = min(workers, length(txt_files)))
    all_results <- future.apply::future_lapply(txt_files, process_qc, future.seed = FALSE)
  }

  # Only the parent writes reports. Mirror input directories to avoid collisions.
  for (i in seq_along(all_results)) {
    all_results[[i]]$prefix <- selected$prefix[i]
    all_results[[i]]$key <- selected$path[i]
    result <- all_results[[i]]
    if (!result$success) next
    dest <- file.path(qc_by_date, "per_file", selected$path[i])
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    if (!result$data$last_offsets_match)
      writeLines(selected$path[i], paste0(dest, "_offset_error.txt"))
    if (!identical(result$data$continuously_coded, "No onset-offset mismatch found"))
      write.csv(result$data$continuously_coded, paste0(dest, "_cont_issues.csv"), row.names = FALSE)
    if (nrow(result$data$proper_labels))
      write.csv(result$data$proper_labels, paste0(dest, "_label_issues.csv"), row.names = FALSE)
  }

  # Coder: extract 2-3 uppercase letter coder ID
  extract_coder <- function(x) {
    fname <- fs::path_file(x)
    m <- stringr::str_match(fname, ".*_([A-Z]{2,3})\\.txt$")[, 2]
    # fallback: take the last token before .txt and strip non-letters
    ifelse(is.na(m),
           stringr::str_replace(stringr::str_extract(fname, "[^_]+(?=\\.txt$)"),
                                "[^A-Za-z]", ""),
           m)
  }

  dump_section <- function(out_file, title, df) {
    writeLines(paste0("\n===== ", title, " ====="),
               con = out_file,
               sep = "\n",
               useBytes = TRUE)
    if (is.null(df) || nrow(df) == 0) {
      writeLines("(none)\n", con = out_file, sep = "\n", useBytes = TRUE)
    } else {
      # Write as TSV blocks inside the txt file
      write.table(df,
                  file = out_file,
                  sep = "\t",
                  row.names = FALSE,
                  col.names = TRUE,
                  quote = FALSE)
      writeLines("", con = out_file, useBytes = TRUE)
    }
  }

  # 1. Reconstruct the global data frames from the 'all_results' list
  success_data <- keep(all_results, ~ .x$success)

  offset_with_coder <- success_data |>
    keep(~ !.x$data$last_offsets_match) |>
    map_df(~ data.frame(filename = .x$key, coder = extract_coder(.x$key)))
  # If there's none, at least provide a coder column
  if (nrow(offset_with_coder) == 0) {
    offset_with_coder <- data.frame(filename = character(),
                                    coder = character())
  }

  labels_with_coder <- success_data |>
    map_df(~ {
      df <- .x$data$proper_labels
      if (nrow(df) > 0) {
        df$filename <- .x$key
        df$coder <- extract_coder(.x$key)
      }
      df
    })
  if (nrow(labels_with_coder) == 0) {
    labels_with_coder <- data.frame(row = integer(), label = character(),
      UPPER = logical(), PROPER = logical(), filename = character(), coder = character())
  }

  continuous_with_coder <- success_data |>
    map_df(~ {
      res <- .x$data$continuously_coded
      if (!(length(res) == 1 && res == "No onset-offset mismatch found")) {
        df <- as.data.frame(res)
        df$filename <- .x$key
        df$coder <- extract_coder(.x$key)
        return(df)
      }
      return(data.frame(filename = character(),
                        coder = character()))
    })

  if (nrow(continuous_with_coder) == 0) {
    continuous_with_coder <- data.frame(filename = character(), coder = character())
  }

  # Build the failed-file table from the parallel results so unreadable
  # files (e.g. cloud-only placeholders that failed to download) are
  # reported instead of dropped silently.
  failed_data <- keep(all_results, ~ !.x$success) |>
    map_df(~ data.frame(
      filename = .x$filename, error = .x$error,
      stringsAsFactors = FALSE
    ))
  if (nrow(failed_data) > 0) {
    atomic_write_tsv(failed_data, file.path(qc_by_date, "failed_files.tsv"))
    failed_log_coded <- failed_data |> mutate(coder = extract_coder(filename))
  } else {
    failed_log_coded <- data.frame(filename = character(),
                                   error = character(),
                                   coder = character())
  }

  # 2. Identify Problematic Coders
  problem_coders <- sort(unique(na.omit(c(failed_log_coded$coder,
                                          offset_with_coder$coder,
                                          labels_with_coder$coder,
                                          continuous_with_coder$coder))))

  problem_coders <- problem_coders[problem_coders != ""]

  # 3. Create Dumps
  qc_by_coder_dir <- file.path(qc_by_date, "qc_by_coder")
  dir.create(qc_by_coder_dir,
             showWarnings = FALSE,
             recursive = TRUE)

  write_coder_report <- function(cd) {
      out_file <- file.path(qc_by_coder_dir, paste0("qc_issues_", cd, ".txt"))
      con <- file(out_file, open = "wt", encoding = "UTF-8")
      on.exit(close(con), add = TRUE)

      writeLines(c(
        paste0("QC issues for coder: ", cd),
        paste0("Generated at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        paste0("Processed path: ", processed_path)
      ), con = con, useBytes = TRUE)

      dump_section(con,
                   "Failed Files (System Errors)",
                   dplyr::filter(failed_log_coded, coder == cd))
      dump_section(con,
                   "Offset Issues",
                   dplyr::filter(offset_with_coder, coder == cd))
      dump_section(con,
                   "Label Issues",
                   dplyr::filter(labels_with_coder, coder == cd))
      dump_section(con,
                   "Continuous Coding Issues",
                   dplyr::filter(continuous_with_coder, coder == cd))

  }
  for (cd in problem_coders) write_coder_report(cd)

  # One file row per discovered export, including clean and skipped files.
  # Assignment/directory rows describe visibility gaps, not proof of no upload.
  checked_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
  summary <- data.frame(row_type = rep("file", nrow(candidates)),
    path = candidates$path, filename = basename(candidates$path),
    prefix = candidates$prefix,
    coder = stringr::str_match(basename(candidates$path), "_([A-Z]{2,3})\\.txt$")[, 2],
    status = rep("skipped_previous_pass", nrow(candidates)),
    checked_this_run = rep(FALSE, nrow(candidates)),
    last_qc = as.character(state$last_qc[previous]),
    report_dir = as.character(state$report_dir[previous]),
    detail = rep("Previously passed; size and modification time unchanged. Not checked this run.",
                 nrow(candidates)))
  for (i in seq_along(all_results)) {
    result <- all_results[[i]]
    row <- match(selected$path[i], summary$path)
    summary$status[row] <- if (!result$success) "processing_failed" else
      if (result$passed) "passed" else "qc_issues"
    summary$checked_this_run[row] <- TRUE
    summary$last_qc[row] <- checked_at
    summary$report_dir[row] <- basename(qc_by_date)
    summary$detail[row] <- if (!result$success) result$error else if (result$passed) {
      "Passed all QC checks."
    } else {
      issues <- character()
      if (!result$data$last_offsets_match) issues <- c(issues, "Final offsets differ")
      continuity <- result$data$continuously_coded
      if (!identical(continuity, "No onset-offset mismatch found"))
        issues <- c(issues, paste(length(continuity), "continuity issue(s)"))
      if (nrow(result$data$proper_labels))
        issues <- c(issues, paste(nrow(result$data$proper_labels), "invalid label(s)"))
      paste(issues, collapse = "; ")
    }
  }
  if (nrow(discovery)) {
    is_file <- discovery$row_type == "file"
    known <- match(discovery$path, state$path)
    summary <- rbind(summary, data.frame(row_type = discovery$row_type,
      path = discovery$path,
      filename = ifelse(is_file, basename(discovery$path), NA_character_),
      prefix = discovery$prefix,
      coder = ifelse(is_file,
        stringr::str_match(basename(discovery$path), "_([A-Z]{2,3})\\.txt$")[, 2], NA_character_),
      status = discovery$status, checked_this_run = FALSE,
      last_qc = as.character(state$last_qc[known]),
      report_dir = rep(basename(qc_by_date), nrow(discovery)), detail = discovery$issue))
  }
  summary <- summary[order(summary$row_type, summary$path), , drop = FALSE]
  atomic_write_tsv(summary, file.path(qc_by_date, "qc_summary.tsv"))

  # All reports, including the summary, are complete before state is committed.
  for (i in seq_along(all_results)) {
    result <- all_results[[i]]
    row <- data.frame(path = selected$path[i], prefix = selected$prefix[i],
      size = if (result$success) result$size else selected$size[i],
      mtime = if (result$success) result$mtime else selected$mtime[i],
      processed_ok = result$success, qc_passed = result$passed,
      last_qc = checked_at, report_dir = basename(qc_by_date))
    state <- rbind(state[state$path != row$path, , drop = FALSE], row)
  }
  references$was_reviewed <- vapply(references$prefix, function(prefix) {
    rows <- state[state$prefix == prefix, , drop = FALSE]
    nrow(rows) > 0 && all(rows$path %in% candidates$path) && all(rows$processed_ok %in% TRUE)
  }, logical(1))
  references$qc_passed <- vapply(references$prefix, function(prefix) {
    rows <- state[state$prefix == prefix, , drop = FALSE]
    nrow(rows) > 0 && all(rows$path %in% candidates$path) && all(rows$qc_passed %in% TRUE)
  }, logical(1))
  references$last_qc <- rep(checked_at, nrow(references))
  atomic_write_tsv(references, ref_log)
  # This is the authoritative checkpoint; a failed commit causes safe rechecking.
  atomic_write_tsv(state, file_log)
  cat("Summary:", file.path(qc_by_date, "qc_summary.tsv"), "\n")
  if (nrow(summary)) {
    counts <- as.data.frame(table(status = summary$status), responseName = "rows")
    print(counts, row.names = FALSE)
  }
  cat("Reports:", qc_by_date, "\n")
  invisible(list(results = all_results, discovery = discovery, summary = summary, state = state,
                 report_dir = qc_by_date))
}

if (!identical(getOption("cleanAnnotateR.autorun"), FALSE)) {
  project_dir <- Sys.getenv("QC_PROJECT_DIR")
  if (!nzchar(project_dir)) {
    here::i_am("qc_project.Rproj")
    project_dir <- here::here()
  }
  run_qc(project_dir, data_root = Sys.getenv("QC_DATA_ROOT", dirname(project_dir)))
}
