# Script for Checking Completion Status of Modelling Construction Process
# Date: November 15th, 2022
#
# Description:
# This script is designed to determine whether a process of modelling construction,
# which utilizes modelling routines from the repository "https://github.com/PEM-Humboldt/biomodelos-sdm/tree/master/Modelling",
# has been completed. The script extracts information from log files to identify any issues encountered during the process.
#
# Return:
# The script generates a data.frame that provides insights into the completion status of the modelling construction process.
# The resulting data.frame contains columns for species names, evaluation status, final models status,
# ensembles of future models status, time taken, and any errors encountered.

# Preamble: Ensure that all species folders are organized inside a single main folder.
# For instance, if you have run the process for 5 species, move those folders into a single directory before running this script.

library(dplyr)
library(stringr)

# Path to the folder containing species run folders
path_in_models <- "XXX"

# Extract species names and folder paths
sps_nm <- list.dirs(path_in_models, recursive = FALSE, full.names = FALSE) %>% gsub(pattern = "\\.", replacement = " ")
sps_folder <- list.dirs(path_in_models, recursive = FALSE, full.names = TRUE)

# Create an empty data.frame to store information
df <- data.frame("species" = character(), "eval" = character(), "final" = character(),
                 "ensembles" = character(), "time" = character(),
                 "Error" = character())

# Loop through each species folder and extract relevant information
for (i in 1:length(sps_folder)) {
  require(stringr)
  
  # Extract species name
  df[i, "species"] <- sps_nm[i]
  
  # Identify subfolders within the species folder
  folders <- list.dirs(sps_folder[i], recursive = FALSE, full.names = TRUE)
  
  # Check for the presence of 'generalizacion' folder
  gener <- list.dirs(folders, full.names = FALSE, recursive = TRUE)
  if (length(which(str_detect(gener, "generalizacion") == TRUE)) > 0) {
    df[i, "eval"] <- NA
    df[i, "final"] <- NA
    df[i, "ensembles"] <- "generalizacion"
    next()
  }
  
  # Check if evaluations were performed
  evalindex <- which(str_detect(folders, "eval_results") == TRUE)
  if (length(evalindex) > 0) df[i, "eval"] <- "x"
  
  # Check if final models were created
  finalindex <- which(str_detect(folders, "final_models") == TRUE)
  if (length(finalindex) > 0) df[i, "final"] <- "x"
  
  # Check for ensembles of future models
  ensindex <- which(str_detect(folders, "ensembles") == TRUE)
  if (length(ensindex) > 0) {
    ensfol <- folders[ensindex]
    ensfols <- list.dirs(ensfol, recursive = FALSE, full.names = TRUE)
    enscurindex <- which(str_detect(ensfols, "current") == TRUE)
    enscur <- ensfols[enscurindex]
    enscurs <- list.files(enscur, recursive = TRUE, full.names = TRUE)
    if (length(enscurs) > 5) df[i, "ensembles"] <- "x"
  }
  
  # Check for errors in the log file
  x <- read.delim(paste0(sps_folder[i], "/log_file.txt"))
  errindex <- which(str_detect(x[, 1], "Error") == TRUE)
  if (length(errindex) > 0) {
    err <- paste(x[(errindex[1] - 1), 1], "\n", x[errindex[1], 1])
    df[i, "Error"] <- err
  }
  
  # Extract time taken
  timeindex <- which(str_detect(x[, 1], "mins") == TRUE)
  timedata <- x[timeindex[length(timeindex)], 1]
  timenums <- strsplit(timedata, "Completed in  ")
  df[i, "time"] <- timenums[[1]][2]
}

# Write the data.frame to a CSV file
write.csv(df, file = "modelling_completion_status.csv", row.names = FALSE)
