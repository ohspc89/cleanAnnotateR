# fetch_ids.R is prepared to list up files for quality check
# using a reference table: `Reach_Assignments_2.xlsx`.
# It will output a file: `reference.tsv`
# This script should be run before running `perform_qc.R`.

# last time manipulated by EGM on 7/25/2025
# JO edited on 1/20/2026

##########################
# Packages: install/load #
##########################
# Require: Installing and Loading R packages for Reproducible Workflows
# stringr: play a big role in many data cleaning and prep tasks
# readxl: makes it easy to get data out of Excel

# These lines are to load packages and install if necessary.
# Do not worry if you dont' understand.
if (!requireNamespace("Require", quietly = T)) install.packages("Require")
package_list = c('readxl', 'stringr', 'fs')
Require::Require(package_list, require=T, cranCache=T)

################################
# Reading xlsx file:           #
#   - Reach_Assignments_2.xlsx #
################################

# [JO] Column names of the excel sheet:
#            |    Month 1 (M1)    |    Month 2 (M2)    |
# ------------------------------------------------------
# ID | Coder | A2 | A3 | ... | A7 | A2 | A3 | ... | A7 | << `colnames`
# As 'A2', 'A3', ..., belong to a month (ex. 'M1'), make each 'A@' -> 'M@A@'
colnames = c('ID', 'Coder',
             paste0('M1', paste0('A', 2:7)),
             paste0('M2', paste0('A', 2:7)),
             paste0('M3', paste0('A', 2:7)),
             paste0('M4', paste0('A', 2:7)),
             paste0('M5', paste0('A', 2:7)),
             paste0('M6', paste0('A', 2:7)),
             paste0('M7', paste0('A', 2*:7)))

# make sure that the A2-A7 are lined up on the left hand side of the excel file sheet- EGM

####################################
# Cross-platform operation enabled #
####################################

# DO NOT CHANGE LINES BELOW:
# -------------------------------------
os.name <- Sys.info()["sysname"]
HOME <- fs::path_home()
EXCEL_FILE <- "Reach_Assignments_2.xlsx"
# -------------------------------------

# Windows helper function
find_folder <- function(pattern, start="C:/Users", exact.match=TRUE, is.recursive=FALSE) {
    paths <- list.dirs(start, recursive=is.recursive, full.names=TRUE)
    # If 'partial', filter with partial matching
    if (exact.match)
        hits <- paths[basename(paths) == pattern]
    else
        hits <- paths[grepl(pattern, paths, ignore.case=T)]

    return(hits)
}

# `onedrive_path` needs to be defined based on which OneDrive folder you sync on your computer.
# I assume people will be syncing 'Behavior Coding' folder.
# -------------------------------------
synced_sharepoint <- find_folder("ChildrensHospitalLosAngeles",
                                 paste0(HOME, "/Library/CloudStorage"),
                                 exact.match=F)
# If it returns you an error, just check if
# `synced_sharepoint[1]` contains 'OneDrive-ChildrensHospitalLosAngeles'
onedrive_path <- find_folder("Behavior Coding",
                             start=synced_sharepoint[1],
                             exact.match=F)
if (os.name == "Windows"){
    synced_sharepoint <- find_folder("CHILDRENS HOSPITAL LOS ANGELS", start=HOME)
    # folder: 'Behavior Coding'
    onedrive_path <- find_folder("Behavior Coding",
                                 start=synced_sharepoint,
                                 exact.match=F)
# -------------------------------------

filepath <- file.path(onedrive_path, EXCEL_FILE)

# sheet='CC'
if (!file.exists(filepath)){
    stop("Error: Excel file not found at: ", filepath)
}
# [JO] 'record' has 81 rows and 68 columns (check with `dim(record)`)
record = read_excel(onedrive_path, sheet='Coding_Assignments', skip=1)
# [JO] This may not work as intended, because 'colnames' is a vector
# whose length is 32 (check with `length(colnames)`).
# What may happen to the remaining 36 columns?
# This will make the names of those columns 'NA'
colnames(record) = colnames

#############################################
# Data Wrangling:                           #
#   - actual information extraction happens #
#############################################

# M3, M4 entries - S:X, Z:AE, or 19:24, 25:31 - old reach sheet
# M3, M4 entries - Q:V, X:AC, AE:AJ, OR 14:22, 24:29, 31:36, for reach_assignments sheet
m345 = record[,c(1, 15:20, 21:27, 28:33)]
m345colnames = colnames(m345)

subj = vector()     # ex. TD17
months = vector()   # ex. M3
acts = vector()     # ex. A4
prefixes = vector() # ex. TD17-M3_A4
                    # full filename ex: TD17-M3_A4R3_CC.txt
paths = vector()    # ex. TD17/TD17_M3
# [JO] The conditional (i.e. `row[j] %in% c('1')`) returned TRUE
# because of R's type coercion behavior.
# In other programming languages this may return an error, because
# the value of row[j] is double ('dbl'), not character ('chr')
# `row[j] %in% c('1')` translates to:
#   "Check if 'row[j]' matches any element of a vector in the right-hand side.
#    Oh, and if data types don't match, coerce the numeric value to
#    a character string."
# Previously I used the grammer (`%in%`) because the mark indicating
# completion was a character ('x' or 'X'). Given that the code has changed,
# consider replacing '1' with 1 (I already changed it).
for (i in 1:dim(m345)[1]){
    row = m345[i,]       # `row` is a tibble
    for (j in 2:length(row)){
        if (row[j] %in% c(1)){
            subjstr = row[1]$ID
            # `stringr::str_split()` splits a string into pieces
            temp = str_split(m345colnames[j], "")[[1]]
            # `stringr::str_c()` joins multiple strings into one
            monstr = str_c(temp[1], temp[2])
            astr = str_c(temp[3], temp[4])
            txtstr = str_c(subjstr, '-',
                           monstr, '_',
                           astr)
            # `stringr::str_c()` is very similar to `paste0()`
            # so you can join strings in the following way.
            # [EGM] this is 3 folders deep
            # pathstr = file.path('Data', subjstr,
            #                     paste0(subjstr, monstr),
            #                     paste0(subjstr, monstr, astr))
            # [JO] I noticed that folder names changed (ex. TD37M3 -> TD37_M3)
            # and all activities are saved in the same month folder.
            # You could still include 'Data' and `subjstr` when you define
            # `pathstr` - then you don't include 'Data' in `Mac_OneDrive_Path`
            # at line 41 of perform_qc.R. Just saying!
            # (10/7/25) EGM is using the commented line below
            # `pathstr = file.path('Data', subjstr, paste0(subjstr, "_", monstr))`
            pathstr = file.path(subjstr,
                                paste0(subjstr, "_", monstr))

            # add items to vectors
            subj = c(subj, subjstr)
            months = c(months, monstr)
            acts = c(acts, astr)
            prefixes = c(prefixes, txtstr)
            paths = c(paths, pathstr)
        }
    }
}

# Reference table looks like...
# +------+-------+-----+------------+---------------+
# | subj | month | act | prefix     | path          |
# |------|-------|-----|------------|---------------|
# | TD17 | M3    | A2  | TD17-M3_A2 | TD17/TD17_M3  |
# |------|-------|-----|------------|---------------|
# | ...  | ...   | ... | ...        | ...           |
tab = data.frame(subj=subj,
                 month=months,
                 act=acts,
                 prefix=prefixes,
                 path=paths)

# Save the refernce to a tab separated file
# (1/20/26) Describing the path:
# |- onedrive_path ('Behavior Coding')
# |  |- Reach & Grasp
# |  |  |- Quality Check
# |  |  |  |- R scripts (ex. fetch_ids.R)
# |  |  |- processed
# |  |  |  |- output of R scripts (ex. reference.tsv)
# |  |  |- Data
# |  |  |  |- Coded data

# save the reference to a tab separated file
reference_path = file.path(onedrive_path, "Reach & Grasp/processed/reference.tsv")
write.table(tab, file=reference_path, sep= "\t",
            row.names=F, col.names=T,
            quote=F)
