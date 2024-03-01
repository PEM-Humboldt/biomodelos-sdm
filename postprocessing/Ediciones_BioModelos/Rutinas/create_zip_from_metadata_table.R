# This script reads metadata from a specified CSV file, extracts key data for each model,
# constructs names for models, generates additional information such as thumbnails, zip files, and PNG images,
# and organizes these files in a designated folder. The script also copies raster TIFF files for each model,
# creates individual CSV files, and zips them, organizing everything in a specified output folder.

# This will work straightforward if you construct models, make metadata and generate PNG and thumbnails
# using biomodelos-sdm tool

# Read metadata from CSV file
meta <- read.csv("path_to_metadata", 1)
# Example: meta <- openxlsx::read.xlsx("D:/humboldt/Invemar/peces_invemar_modelos/_metadata_Modelos_Humboldt_Invemar_2023-12-06_.xlsx", 1)

# Extract key data from each model
threshold <- meta$thresholdType
threshold[which(threshold != "Continuous")] <- paste0(threshold[which(threshold != "Continuous")], "_")
threshold[which(threshold == "Continuous")] <- ""

algo <- meta$modelingMethod

sp <- meta$acceptedNameUsage
sp <- gsub(" ", "_", sp)

# Construct model names
modelnm <- paste0(sp, "_", threshold, algo)

# Construct PNG, thumb, and zip information to add to the metadata file
meta$thumb <- paste0(modelnm, "_thumb.png")
meta$zip <- paste0(modelnm, ".zip")
meta$png <- paste0(modelnm, ".png")

# Save metadata with additional data and images to a new CSV file
write.csv(meta, "path_to_save_meta_with_data_images")
# Example: write.csv(meta, "c:/humboldt/miscelanea/Invemar_areas_interes/peces_invemar_modelos/inExtent_BM/_metadata_Modelos_Humboldt_Invemar_2024-01-24_.csv", row.names = F)

# Create folder for storing PNG and thumb images
zip_path <- paste0("path_to_folder_which_store_the_png_and_thumb_images", "/zip/")
# Example: zip_path <- paste0("D:/humboldt/Invemar/peces_invemar_modelos", "/zip/")
dir.create(zip_path)
setwd(zip_path)

# Define path for raster TIFF files
tif_path <- "path_of_folder_where_are_store_raster_tif_files"
# Example: tif_path <- "D:/humboldt/Invemar/peces_invemar_modelos/Extent/"

# Loop through each row in the metadata and process each model
for(i in 1:nrow(meta)){
  message(i)
  metai <- meta[i, ]
  sp <- metai$acceptedNameUsage
  sp <- gsub(" ", "_", sp)
  
  threshold <- metai$thresholdType
  if(threshold == "sen") threshold <- "30" 
  if(threshold == "Continuous"){
    threshold <- ""
  }else{
    threshold <- paste0(threshold, "_")
  }
  
  algo <- metai$modelingMethod
  
  modelnm <- paste0(sp, "_", threshold, algo)
  
  path <- paste0(tif_path, modelnm, ".tif")
  
  # Save individual model metadata to a CSV file
  write.csv(metai, paste0(zip_path, modelnm, ".csv"), row.names = FALSE)
  
  # Copy raster TIFF files to the zip folder
  if(file.exists(path)){
    file.copy(path, to = zip_path, overwrite = TRUE)
  }else{
    unlink(paste0(zip_path, modelnm, ".csv"))
  }
  
  # Get a list of files matching the model name pattern in the zip folder
  x <- list.files(zip_path, pattern = modelnm, full.names = FALSE)
  
  # Create a zip file for the model
  zip(zipfile = paste0(zip_path, modelnm), files = x)
  
  # Remove individual files after zipping
  unlink(x)
}

# Close all open connections
closeAllConnections()
