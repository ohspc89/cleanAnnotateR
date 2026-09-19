# Run from the repository root: Rscript tests/regression.R
options(cleanAnnotateR.autorun = FALSE)
source('code/qc_functions.R')
source('code/fetch_ids.R')
source('code/perform_qc.R')
project <- normalizePath('code')
root <- tempfile('qc-regression-')
dir.create(root)
expect_error <- function(expr, pattern) {
  error <- tryCatch({force(expr); NULL}, error = identity)
  stopifnot(inherits(error, 'error'), grepl(pattern, conditionMessage(error)))
}
write_annotation <- function(path, lines = c('LA 0 10 10 MO', 'RA 0 10 10 N')) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path)
  invisible(path)
}
for (lines in list(c('LA 10 0 -10 MO'), c('LA x y z MO'), c('LA 0 NA 10 MO'),
                   c('LA 0 Inf Inf MO'), c('LA 0 10 11 MO'),
                   c('LA 0 10 10 MO', 'LA NA 20 10 MO'), character())) {
  p <- write_annotation(file.path(root, 'bad.txt'), lines)
  expect_error(qc.all(p), 'Error reading')
}
stopifnot(qc.all(write_annotation(file.path(root, 'valid.txt')))$last_offsets_match)
six <- write_annotation(file.path(root, 'six.txt'), c('LA trial 0 10 10 MO', 'RA trial 0 10 10 Q'))
stopifnot(nrow(qc.all(six)$proper_labels) == 0)
label <- write_annotation(file.path(root, 'label.txt'), 'LA 0 10 10 mdx')
stopifnot(identical(qc.all(label)$proper_labels$row, 1L))

state_dir <- file.path(root, 'processed/qc_state')
refs <- data.frame(prefix = c('TD01-M1A2', 'TD01-M1A3'), path = rep('Data/TD01/TD01_M1', 2))
atomic_write_tsv(refs, file.path(state_dir, 'reference_current.tsv'))
a <- write_annotation(file.path(root, refs$path[1], 'TD01-M1A2R1_CC.txt'))
b <- write_annotation(file.path(root, refs$path[1], 'TD01-M1A2R2_CC.txt'), 'LA 0 10 10 mdx')
c <- write_annotation(file.path(root, refs$path[1], 'TD01-M1A3R1_AG.txt'), 'LA 0 NA 10 MO')
write_annotation(file.path(root, refs$path[1], 'typo.txt'))
old_plan <- future::plan()
first <- run_qc(project, root, full = TRUE, workers = 2)
stopifnot(length(first$results) == 3, sum(first$state$processed_ok) == 2,
          sum(first$state$qc_passed) == 1, nrow(first$discovery) == 1,
          identical(class(future::plan()), class(old_plan)) && future::nbrOfWorkers() == 1L)
stopifnot(file.exists(file.path(first$report_dir, 'per_file', refs$path[1],
                               'TD01-M1A2R2_CC.txt_label_issues.csv')))
stopifnot(nrow(first$summary) == 4L,
          setequal(first$summary$status, c('passed', 'qc_issues', 'processing_failed', 'unmatched')),
          !anyDuplicated(first$summary$path),
          all(first$summary$checked_this_run[first$summary$status != 'unmatched']),
          !first$summary$checked_this_run[first$summary$status == 'unmatched'],
          grepl('invalid label', first$summary$detail[first$summary$status == 'qc_issues']))
saved_summary <- read_state(file.path(first$report_dir, 'qc_summary.tsv'))
stopifnot(nrow(saved_summary) == 4L, setequal(saved_summary$path, first$summary$path))
second <- run_qc(project, root, full = FALSE, workers = 1)
skipped <- second$summary[second$summary$status == 'skipped_previous_pass', ]
stopifnot(nrow(skipped) == 1L, !skipped$checked_this_run,
          skipped$report_dir == basename(first$report_dir),
          skipped$last_qc == first$state$last_qc[first$state$qc_passed])
stopifnot(length(second$results) == 2) # retry issues and processing failures, not clean sibling
write_annotation(b)
write_annotation(c)
third <- run_qc(project, root, full = FALSE, workers = 2)
stopifnot(length(third$results) == 2, all(third$state$qc_passed))
fourth <- run_qc(project, root, full = FALSE, workers = 1)
stopifnot(sum(fourth$summary$status == 'skipped_previous_pass') == 3L,
          all(!fourth$summary$checked_this_run))
stopifnot(length(fourth$results) == 0) # unchanged metadata survives TSV round trip
# New files must be checked even if their upload preserves an old modification time.
d <- write_annotation(file.path(root, refs$path[1], 'TD01-M1A2R3_CC.txt'))
Sys.setFileTime(d, as.POSIXct('2000-01-01', tz = 'UTC'))
fifth <- run_qc(project, root, full = FALSE, workers = 1)
stopifnot(length(fifth$results) == 1)
unlink(a)
sixth <- run_qc(project, root, full = FALSE, workers = 1)
stopifnot(any(grepl('Previously recorded', sixth$discovery$issue)),
          !sixth$state$processed_ok[match(sub(paste0(root, '/'), '', a), sixth$state$path)])
stopifnot(sum(sixth$summary$status == 'missing_or_inaccessible') == 1L,
          !anyDuplicated(sixth$summary$path))
# Changed files are detected even if size stays the same and mtime moves backwards.
write_annotation(c, 'LA 0 10 10 mdx')
Sys.setFileTime(c, as.POSIXct('2001-01-01', tz = 'UTC'))
changed <- run_qc(project, root, full = FALSE, workers = 1)
stopifnot(length(changed$results) == 1, !changed$results[[1]]$passed)
write_annotation(c)
# Empty and missing folders must produce explicit discovery issues.
refs <- rbind(refs, data.frame(prefix = c('TD02-M1A2', 'TD03-M1A2'),
                   path = c('Data/TD02/TD02_M1', 'Data/TD03/TD03_M1')))
dir.create(file.path(root, refs$path[3]), recursive = TRUE)
atomic_write_tsv(refs, file.path(state_dir, 'reference_current.tsv'))
seventh <- run_qc(project, root, full = FALSE, workers = 1)
stopifnot(any(seventh$discovery$status == 'directory_unavailable'),
          sum(grepl('No matching annotation', seventh$discovery$issue)) == 2)
stopifnot(sum(seventh$summary$status == 'no_matching_file') == 2L,
          all(seventh$summary$row_type[seventh$summary$status == 'no_matching_file'] == 'assignment'))
# A summary write failure must also prevent checkpoint updates.
summary_before <- readBin(file.path(state_dir, 'file_log.tsv'), 'raw', n = 1e7)
blocked_summary <- new.env(parent = globalenv())
blocked_summary$source <- function(file, local, ...) {
  target <- if (isTRUE(local)) parent.frame() else local
  base::source(file, local = target, ...)
  original <- target$atomic_write_tsv
  target$atomic_write_tsv <- function(x, path) {
    if (basename(path) == 'qc_summary.tsv') stop('simulated summary failure')
    original(x, path)
  }
}
summary_runner <- run_qc
environment(summary_runner) <- blocked_summary
expect_error(summary_runner(project, root, full = FALSE, workers = 1), 'simulated summary failure')
stopifnot(identical(summary_before, readBin(file.path(state_dir, 'file_log.tsv'), 'raw', n = 1e7)))
# Hold the shared lock: no concurrent run can overwrite state.
lock <- acquire_qc_lock(state_dir)
expect_error(run_qc(project, root, full = FALSE, workers = 1), 'locked')
unlink(lock, recursive = TRUE)
# A late report error must preserve authoritative state and restore the plan.
before <- readBin(file.path(state_dir, 'file_log.tsv'), 'raw', n = 1e7)
write.csv <- function(...) stop('simulated report failure')
write_annotation(b, 'LA 0 10 10 mdx')
expect_error(run_qc(project, root, full = TRUE, workers = 2), 'simulated report failure')
rm(write.csv)
stopifnot(identical(before, readBin(file.path(state_dir, 'file_log.tsv'), 'raw', n = 1e7)),
          identical(class(future::plan()), class(old_plan)) && future::nbrOfWorkers() == 1L, !dir.exists(file.path(state_dir, '.qc.lock')))
# Replacement failure must leave the existing destination intact.
blocked <- file.path(root, 'blocked.tsv'); dir.create(blocked)
suppressWarnings(expect_error(atomic_write_tsv(data.frame(x = 1), blocked), 'atomically'))
stopifnot(dir.exists(blocked))
# Full runs include newly added assignments, and removed assignments stop scanning.
new_ref <- data.frame(prefix = 'TD04-M1A2', path = 'Data/TD04/TD04_M1')
write_annotation(file.path(root, new_ref$path, 'TD04-M1A2R1_CC.txt'))
atomic_write_tsv(new_ref, file.path(state_dir, 'reference_current.tsv'))
new_full <- run_qc(project, root, full = TRUE, workers = 1)
stopifnot(length(new_full$results) == 1, new_full$results[[1]]$prefix == new_ref$prefix)
# Empty current assignments must not resurrect history or terminate the R session.
atomic_write_tsv(refs[0, ], file.path(state_dir, 'reference_current.tsv'))
empty <- run_qc(project, root, full = TRUE, workers = 1)
stopifnot(length(empty$results) == 0, nrow(empty$summary) == 0,
          file.exists(file.path(empty$report_dir, 'qc_summary.tsv')),
          'status' %in% names(read_state(file.path(empty$report_dir, 'qc_summary.tsv'))))
# Exercise the public CLI entry point and explicit mode override.
Sys.setenv(QC_PROJECT_DIR = project, QC_DATA_ROOT = root, QC_WORKERS = "1", QC_FULL = "1")
cli <- system2(file.path(R.home('bin'), 'Rscript'),
               c(shQuote(file.path(project, 'perform_qc.R')), '--incremental'),
               stdout = TRUE, stderr = TRUE)
stopifnot(is.null(attr(cli, 'status')), any(grepl('QC mode: INCREMENTAL', cli)))
Sys.unsetenv(c('QC_PROJECT_DIR', 'QC_DATA_ROOT', 'QC_WORKERS', 'QC_FULL'))
# Exercise the supplied workbook if available, without changing it.
workbook <- 'docs/Reach_Assignments_2 - Copy.xlsx'
if (file.exists(workbook)) {
  excel_root <- tempfile('qc-workbook-'); dir.create(excel_root)
  tab <- fetch_ids(workbook, project, excel_root)
  stopifnot(nrow(tab) == 1529L, !anyDuplicated(tab$prefix), all(grepl('^TD[0-9]+$', tab$ID)))
  issues <- read_state(file.path(excel_root, 'processed/qc_state/assignment_issues.tsv'))
  stopifnot(nrow(issues) == 1L, issues$flag == 'red')
  fetch_ids(workbook, project, excel_root)
  stopifnot(nrow(read_state(file.path(excel_root, 'processed/qc_state/reference_new.tsv'))) == 0)
  unlink(excel_root, recursive = TRUE)
}
unlink(root, recursive = TRUE)
cat('All regression checks passed.\n')
