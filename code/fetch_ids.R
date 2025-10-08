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

# `onedrive_path` will change... adapt based on the OneDrive folder synced on your computer.
# (10/7/27) Just matching with what EGM/JC works
# -------------------------------------
onedrive_path <- "Library/CloudStorage/OneDrive-ChildrensHospitalLosAngeles/EEG reaching R01/Analysis/Behavior Coding"
if (os.name == "Windows")
    onedrive_path <- "Childrens Hospital Los Angeles/Smith, Beth - EEG reaching R01/Analysis/Behavior Coding"
# -------------------------------------

filepath = file.path(HOME, onedrive_path, EXCEL_FILE)

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

# save the reference to a tab separated file
write.table(tab, file='../processed/reference.tsv', sep= "\t",
            row.names=F, col.names=T,
            quote=F)

# your referece.tsv file will be put in one folder below the directory that is listed above - ask Jin why this happens? 
# [JO] One folder below? I am not sure what you mean...
# Right now it is assumed that 'reference.tsv' will be saved
# in the 'processed' folder, that's one level up from where you
# run the code ('..' means parent directory, or one level up).
# To add a little more... this is related to how I structured things.

# folder    | content
# --------------------
# codes     | R codes
# processed | code output
# docs      | relevant documents

# Folder structure was as follows:

# NEUR490
# |-- code                  << this is the working directory I worked in
# |   |-- fetch_ids.R
# |   |-- perform_qc.R
# |   |-- qc_functions.R
# |-- processed
# |-- docs
# |   |-- pdf/json files

# When my working directory is 'code', one level up is 'NEUR490'.
# '../processed/reference.tsv' == 'NEUR490/processed/reference.tsv'

# I also explained in 'Adapting_the_code.pdf'
# One note I missed was that you need to have "processed" folder created
# before running the last line, or you will get an error.


#Notes for code
#in order to see exactly which rows your script is pikcing up run
#print(head(m345, 3)) 
#in the directoy - it will output what the script is picking up - you can change the 3 to add more rows.

#last time manipulated by EGM on 7/25/2025
