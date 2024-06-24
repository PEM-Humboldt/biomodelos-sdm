# Download and Process Species Records
# Date: February 24, 2021

# Packages: Specifies the required packages and provides a commented loop for installing them if necessary.
# Data Reading: Generalizes the reading of species names from a CSV file. Alternatives for reading from an Excel file or manually defining species are provided.
# MongoDB Connection: Establishes a connection to MongoDB for two collections: species and records.
# RetrieveSpRecords Function: Retrieves species records from the MongoDB database.
# Processing Loop: Iterates over the species list, retrieving and storing records or logging species without data.
# Data Output: Combines all records into a single DataFrame and saves it to a CSV file. Also filters records based on certain criteria and saves the result to another CSV file.

# Required packages for downloading and processing

packages <- c("mongolite", "plyr", "dplyr", "devtools", "gtools", "openxlsx")

# Install packages
# Uncomment the following section if package installation is needed
# for(i in 1:length(packages)){
#   install.packages(packages[i])
# }

# Load packages

lapply(packages, require, character.only = TRUE)

# Read species names list from a CSV file
species_list <- read.csv("spListasDatos22022021.csv")
# Alternatively, read from an Excel file
# species_list <- read.xlsx("path/to/your/file.xlsx")[]
# Species list can also be defined manually
# species_list <- c("SpeciesName1", "SpeciesName2")

# MongoDB Connection
# see: https://docs.google.com/document/d/1nOi1hg6sujxJUI9uolEFrgZOpagDZfGCSOFSmp87yzo/edit

con1 <- mongo(db = "produccion", collection = "records",
              url ="connection_url_not_shown", verbose = FALSE)
con2 <- mongo(db = "produccion", collection = "species",
              url ="connection_url_not_shown", verbose = FALSE)

# Convert the species list to a character vector
spp_list <- species_list[,1] %>% as.character()

# Function to retrieve species records
RetrieveSpRecords <- function(spList, con){
  occs <- data.frame()
  log.occs <- data.frame(spList, nOccs=0)
  
  print(spList)
  query.result <- con$find(paste0('{"acceptedNameUsage":"', spList, '"}'))
  query.result <- cbind(con$find(paste0('{"acceptedNameUsage":"', spList, '"}'), fields = '{"_id":1}'), query.result)
  query.result$`__v` <- NULL
  
  if(nrow(query.result) > 0){
    rownames(occs) <- NULL
    occs <- rbind(occs, query.result)
    log.occs$nOccs <- nrow(query.result)
  }
  
  return(list(occs=occs, log.occs=log.occs))
}

# Initialize lists to store records and species without records
occ_allsp <- list()
zero <- list()

# Loop to obtain records for all species
for(i in 1:length(spp_list)){
  occ.file1 <- RetrieveSpRecords(spList = spp_list[i], con1)
  if(occ.file1$log.occs[1, 2] >= 1){  
    occs1 <- data.frame(lapply(occ.file1$occs, as.character), stringsAsFactors=FALSE)
    occ_allsp[[i]] <- occs1
  } else {
    zero[[i]] <- spp_list[i]
    names(zero[[i]]) <- spp_list[i]
  }
}

# Assign names to the records list
names(occ_allsp) <- spp_list

# Combine all records into a single DataFrame
occ_allspDF <- do.call("bind_rows", occ_allsp)

# Save the records to a CSV file
write.csv(occ_allspDF, "occurrences_all_species.csv", row.names = FALSE) 

# Display the number of records obtained
nrow(occ_allspDF)

# Filter out reported data based on certain criteria
occ_allspDF_clean <- with(occ_allspDF, occ_allspDF[occ_allspDF$spatialDuplicated == FALSE & is.na(occ_allspDF$reportedDate), ])

# Save the filtered records to a CSV file
write.csv(occ_allspDF_clean, "cleaned_species_records.csv", row.names = FALSE)
