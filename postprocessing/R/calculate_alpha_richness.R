# This R script performs the progressive accumulation of raster data from TIFF files in a specified directory. 
# During the loop, the values of cells in each file are loaded and summed, accumulating them into a total raster. 
# Every 100 iterations or at last iteration, the accumulated raster is saved to a new TIFF file, memory is cleaned 
# and the accumulated result is loaded into a new TIFF file. The accumulated result is loaded as the new base raster. 
# At the end of the loop, a graph of the accumulated raster is generated and the final result is saved in a TIFF file 
# with a specific name. This approach seeks to calculate the total richness of raster data accumulated incrementally, facilitating the 
# processing of large spatial datasets on computers with low processing power.

library(dplyr)
library(terra)
library(stringr)

F1 <- "path_models"
files_path <- list.files(F1, pattern = ".tif", recursive = T, full.names = F)

# Riqueza total acumulada

for(i in 1:length(files_path)){
  print(i)
  
  # Cargar el archivo raster actual
  if(i == 1){
    BioModelos_1 <- rast(files_path[i])
    BioModelos_1[is.na(BioModelos_1)] <- 0
  }else if(i == 2){
    
    # En la segunda iteración, acumular el primer raster
    BioModelos_2 <- rast(files_path[i])
    BioModelos_2[is.na(BioModelos_2)] <- 0
    BioModelos_i <- c(BioModelos_1, BioModelos_2)
  }else{
    
    # En iteraciones subsiguientes, acumular el raster actual
    BioModelos_3 <- rast(files_path[i])
    BioModelos_3[is.na(BioModelos_3)] <- 0
    add(BioModelos_i) <- BioModelos_3
    
    # Calcular la riqueza total acumulada
    Riqueza <- sum(BioModelos_i)
    
    # Guardar resultados cada 100 iteraciones y en la última iteración
    if(i%%100 == 0|i == length(files_path)){
      writeRaster(Riqueza, paste0("riq_", i, ".tif"), overwrite = T)
      rm(BioModelos_i); gc()
      BioModelos_i <- rast(paste0("riq_", i, ".tif"))
    }
  }
}

plot(Riqueza)
writeRaster(Riqueza, "name_XXX_XXX.tif")

