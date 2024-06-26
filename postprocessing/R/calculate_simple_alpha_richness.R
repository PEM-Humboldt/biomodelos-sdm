# Progressive Accumulation of Raster Data for Total Richness Calculation

# Update date: june 26 2024

# This R script incrementally accumulates raster data from TIFF files to calculate total richness. 
# It processes raster files in a loop, summing cell values and periodically saving the accumulated 
# result. This approach enables efficient handling of large spatial datasets, particularly on 
# computers with limited processing power.

library(dplyr)
library(terra)
library(stringr)

# Directory containing the raster files
F1 <- "path_models"
files_path <- list.files(F1, pattern = ".tif", recursive = TRUE, full.names = TRUE)

# Initialize the total richness accumulation process
for(i in seq_along(files_path)){
  print(i)
  
  # Load the current raster file
  if(i == 1){
    BioModelos_1 <- rast(files_path[i])
    BioModelos_1[is.na(BioModelos_1)] <- 0
  } else if(i == 2){
    
    # In the second iteration, accumulate the first raster
    BioModelos_2 <- rast(files_path[i])
    BioModelos_2[is.na(BioModelos_2)] <- 0
    BioModelos_i <- c(BioModelos_1, BioModelos_2)
  } else {
    
    # In subsequent iterations, accumulate the current raster
    BioModelos_3 <- rast(files_path[i])
    BioModelos_3[is.na(BioModelos_3)] <- 0
    add(BioModelos_i) <- BioModelos_3
    
    # Calculate the total accumulated richness
    Riqueza <- sum(BioModelos_i)
    
    # Save results every 100 iterations and at the last iteration
    if(i %% 100 == 0 || i == length(files_path)){
      writeRaster(Riqueza, paste0("riq_", i, ".tif"), overwrite = TRUE)
      rm(BioModelos_i)
      gc()
      BioModelos_i <- rast(paste0("riq_", i, ".tif"))
    }
  }
}

# Plot the final accumulated richness raster
plot(Riqueza)
# Save the final accumulated richness raster
writeRaster(Riqueza, "name_XXX_XXX.tif", overwrite = TRUE)
