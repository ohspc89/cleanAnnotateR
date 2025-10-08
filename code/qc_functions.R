# qc_functions.R is an R script of R functions used in
# reading a .txt file exported from ELAN and carrying out
# quality checks including:
#   1) if the last offset in miliseconds is the same for all tiers
#   2) if each row's offset is identical to the onset of the following row
#   3) if a label is in capital letter(s) and is from a predefined choices

# Written by Jinseok Oh, March 5, 2025
# per request of Dr. Erika Garcia Mora & Christina Choi.

# Editied: July 26, 2025


make.table = function(path){

    #' Read a txt file and return a table with column names
    #'
    #' @param path path to the .txt file to read

    # Each .txt should have SIX columns, if properly exported.
    colnames5 = c('tier', 'onset', 'offset', 'duration', 'label')   # old .txt
    colnames6 = c('tier', 'activity', 'onset', 'offset', 'duration', 'label')
    # Read the txt file and save to a variable: `table`.
    # Provide the column names - so your `table` will be like:
    #
    # | tier | activity  | onset | offset | duration | label |
    # |------|-----------|-------|--------|----------|-------|
    # | LA   | TD05-M4A3 | 0     | 5000   | 5000     | MO    |
    # |------|-----------|-------|--------|----------|-------|
    # | ...  | ...       | ...   | ...    | ...      | ...   |
    result = tryCatch({
        table = read.table(path, stringsAsFactors = FALSE)
        n = ncol(table)
        if (n == length(colnames5)) {
            colnames(table) <- colnames5
        }
        else if (n == length(colnames6)) {
            colnames(table) <- colnames6
        }
        else {
            stop("Unexpected number of columns (", n, ") in file: ", path)
        }
        return(table)
    }, error=function(e) {
        stop("Error reading file ", path, "; ", conditionMessage(e))
    })
    return(result)
}


qc.lastoffset = function(table){

    #' Perform the quality check #1
    #'
    #' @param table table prepared by `make.table()`

    # QC 1. Compare last 'offset' values for different tiers
    # One simple way to do this is to save tier-specific
    # last offset values and compare if all of them
    # are the same value.

    # Get unique tiers
    # `table$tier`: 'tier' column of `table`.
    tiers = unique(table$tier)

    # last offset values will be saved: preallocate memory
    last_offsets = numeric(length(tiers))

    for (i in seq_along(tiers)){
        # `table[a, b]` will get the value at
        # a-th row, b-th column. Here, b=3.
        # The third column of `table` is 'offset'.
        # Row indices are given conditionally (get the rows
        # whose 'tier' column values are the same as the i-th tier. 
        offsets = table[table$tier == tiers[i], 'offset']
        last_offsets[i] = tail(offsets, 1) # Get the last offset directly
    }
    # ALL `last_offset` values should be the same.
    # In other words, there's a SINGLE unique `last_offset` value.
    return (length(unique(last_offsets)) == 1)
}


qc.continuous = function(table){

    #' Perform the quality check #2
    #'
    #' @param table table prepared by `make.table()`

    # QC 2. Check if the 'offset' of a previous row (if exists)
    # is identical to the 'onset' of the current row.
    # There can be different ways to report errors, but one simple
    # way is to report the rows without the 'offset-onset' match.
    # This should be done for each tier.

    tiers = unique(table$tier)  # Get unique tiers

    continuous_log = character()

    for (tr in tiers){   # Iterate over tiers

        # `subset` will contain rows of a specific tier.
        subset = table[table$tier == tr, ]

        # Iterate over rows. Indexing starts from 2, because
        # the first row does not have a 'previous' row to compare.
        # `dim()` returns the dimension of `subset`. The two numbers
        # are the number of rows and the number of columns.
        # By choosing the first ([1]) output of `dim(subset)`,
        # we are getting the number of rows of `subset`.
        for (i in 2:dim(subset)[1]){
            prev_offset = subset[i-1, 'offset']
            current_onset = subset[i, 'onset']
            # If there's a mismatch, a message will be stored.
            # The format of the message will be:
            # 'Tier: {tier}; rows: {i}-{i+1}; values differ: {prev_offset} vs. {current_onset}'
            if (prev_offset != current_onset){
                continuous_log = c(continuous_log,
                                   paste0('Tier: ', tr, '; rows: ', i-1,
                                          '-', i, '; values differ: ',
                                          prev_offset, ' vs. ',
                                          current_onset))
            }
        }
    }
    # If no mistmatch was found, then report: "No mismatch found"
    if (!(length(continuous_log)))
        continuous_log = c(continuous_log,
                           'No onset-offset mismatch found')
    return(continuous_log)
}


qc.label = function(table){

    #' Perform the quality check #3
    #'
    #' @param table table prepared by `make.table()`

    # QC 3. Check if all labels of the table are uppercase &
    # belong to the predefined labels.

    # Labels - there are three levels
    # Top level:
    #   M (Moving) | N (Not moving) | Z (pause) | Q (Not visible)
    # Second level (if top level was M):
    #   D (towards toy) | O (not towards toy)
    # Third level (if second level was D):
    #   T (touch) | G (grasp) | X (no touch)

    your_labels = table[, 'label']    # Get all labels

    # check if all labels are uppercase
    all_upper = your_labels == toupper(your_labels)

    # local function to check if `str` is a proper label
    if.proper = function(str){
        return(grepl("^(N|Z|Q)$", str) |  # if 1 letter: 'N', 'Z', or 'Q'
               grepl("^MO$", str) |       # if 2 letters: 'MO'
               grepl("^MD[TGX]$", str)    # if 3 letters: 'MDG', 'MDT', or 'MDX'
               )
    }

    all_proper = sapply(your_labels, if.proper)

    # Combine the results to make one data frame.
    out = data.frame('label'=your_labels,
                     'UPPER'=all_upper,
                     'PROPER'=all_proper)

    return(out[!(out$UPPER & out$PROPER),])
}


# Now, you can use individual function to perform
# different quality chekcs one by one.
# However, you can combine all three steps in one function.
# This is one example of <modularization>.

qc.all = function(path){

    #' Perform the quality check #3
    #'
    #' @param path path to the .txt file to read

    table = make.table(path)
    last_offsets_match = qc.lastoffset(table)
    continuously_coded = qc.continuous(table)
    labels_properly_formed = qc.label(table)

    return(list('last_offsets_match'=last_offsets_match,
                'continuously_coded'=continuously_coded,
                'proper_labels'=labels_properly_formed))
}

# (10/7/25) Utility function to handle an edge case
make.proper.dataframe = function(vector){
    # If a 1-by-n vector, transpose first, then make a dataframe.
    if (is.null(dim(vector)))
        return(data.frame(t(vector)))
    # If not, just return a plain dataframe.
    return(data.frame(vector))
