# This script reads metadata from a specified CSV file, extracts key data for each model,
# constructs names for models, generates additional information such as thumbnails, zip files, and PNG images,
# and organizes these files in a designated folder. The script also copies raster TIFF files for each model,
# creates individual CSV files, and zips them, organizing everything in a specified output folder.

# This will work straightforward if you construct models, make metadata and generate PNG and thumbnails
# using biomodelos-sdm tool.

# Read metadata from CSV file
meta <- read.csv("path_to_metadata")
# Example1: meta <- openxlsx::read.xlsx("D:/humboldt/Invemar/peces_invemar_modelos/_metadata_Modelos_Humboldt_Invemar_2023-12-06_.xlsx", 1)
# Example2: meta <- as.data.frame(data.table::fread("D:/humboldt/bm_ediciones_upload/actualizacion_carnivoros/carnivora_metadata_2024-06-20_imagenes.csv", encoding = "Latin-1")) 

# Create folder for storing PNG and thumb images
zip_path <- paste0("path_to_folder_which_store_the_png_and_thumb_images", "/zip/")
# Example: zip_path <- paste0("D:/humboldt/Invemar/peces_invemar_modelos", "/zip/")
# Example2: zip_path <- ("D:/humboldt/bm_ediciones_upload/actualizacion_carnivoros/flujo_imagenes/zip")

dir.create(zip_path)
setwd(zip_path)

# Define path for raster TIFF files
tif_path <- "path_of_folder_where_are_store_raster_tif_files"
# Example1: tif_path <- "D:/humboldt/Invemar/peces_invemar_modelos/Extent/"
# Example2: tif_path <- "D:/humboldt/bm_ediciones_upload/actualizacion_carnivoros/flujo_imagenes/"

# raster files were created using biomodelos-sdm tool?
bMtool <- FALSE # TRUE or FALSE

# Loop through each row in the metadata and process each model
for(i in 1:nrow(meta)){
  
  #i <- 1
  message(i)
  metai <- meta[i, ]
  
  # Get species name and replace spaces with underscores
  sp <- metai$acceptedNameUsage
  sp <- gsub(" ", "_", sp)
  
  if(bMtool == TRUE){
    
    # Get threshold type and adjust it if necessary
    threshold <- metai$thresholdType
    
    if(is.na(threshold)){
      threshold <- ""
    }else if(threshold == "Continuous"){
      threshold <- ""
    }else if(threshold == "sen"){ # only for invemar taxon
      threshold <- "_30" 
    }else{
      threshold <- paste0("_", threshold)
    }
    
    # Get modeling method and adjust the name
    algo <- metai$modelingMethod
    algo <- paste0("_", algo)
    
    # Construct model name
    modelnm <- paste0(sp, threshold, algo)
    
  } else {
    
    # Model name is taken from disk, however this method might throw issues
    modelnm <- list.files(tif_path, pattern = sp, full.names = FALSE)
    modelnm <- sub(".tif", "", modelnm)
  }
  
  # Update metadata with file names for thumbnails, PNGs, and ZIPs
  metai$thumb <- paste0(modelnm, "_thumb.png")
  metai$png <- paste0(modelnm, ".png")
  metai$zip <- paste0(modelnm, ".zip") 
  
  if(metai$isActive == TRUE){
    metai$isActive <- "true"
  }else{
    metai$isActive <- "false"
  }
  
  if(metai$published == TRUE){
    metai$isActive <- "true"
  }else{
    metai$published <- "false"
  }
  
  # Construct the path to the TIFF file
  path <- paste0(tif_path, modelnm, ".tif")
  
  # Save individual model metadata to a CSV file
  write.csv(metai, paste0(zip_path, "/", modelnm, ".csv"), row.names = FALSE, na = "")
  
  # Copy raster TIFF files to the zip folder if they exist
  if(file.exists(path)){
    file.copy(path, to = zip_path, overwrite = TRUE)
  } else {
    unlink(paste0(zip_path, modelnm, ".csv"))
  }
  
  # Get a list of files matching the model name pattern in the zip folder
  x <- list.files(zip_path, pattern = modelnm, full.names = FALSE)
  
  # Create a zip file for the model
  zip(zipfile = paste0(zip_path, "/", modelnm), files = x)
  
  # Remove individual files after zipping
  unlink(x)
}


# Close all open connections
closeAllConnections()
