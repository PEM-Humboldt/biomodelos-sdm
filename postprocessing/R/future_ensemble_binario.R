# future_ensemble
# This function is designed to read in raster files representing predicted species distributions under 
# different environmental scenarios, and create an ensemble prediction of these distributions based on 
# the maximum frequency value of the raster cells across all scenarios.
#
# folder_in, character. The folder_in argument is a character string that specifies the path to a folder 
# containing raster files that represent predicted species distributions under different environmental scenarios. 
# The folder must contain a subfolder named "ensembles/future/", which contains subfolders with names
# corresponding to the algorithm used to fit models (e.g., "MAXENT"). The standard version is constructed with
# routines find in https://github.com/PEM-Humboldt/biomodelos-sdm/tree/master/modelling
# algo: character. The algo argument is an optional character string that specifies the name of the algorithm 
# used to fit models. If not specified, it defaults to "MAXENT". This string is used to find the name folder 
# inside the species future folder
#
# return: The output of this function is an ensemble prediction of species distributions under the different 
# environmental scenarios, which can be useful for making predictions under future environmental conditions 
# or assessing the sensitivity of predictions to different modeling approaches.
#
#Example: future_ensemble(folder_in = "modelos/Drymonia.pendula")

library(stringr)
library(dplyr)
library(terra)

future_ensemble <- function(folder_in =spp[969], algo = "MAXENT", threshold = 10, erase_old = TRUE, 
                            uncertainty = FALSE) {

  message(folder_in)
  path_fut <- "/ensembles/future/"
  
  new_folder <- paste0(folder_in, path_fut, algo, "/future_ensemble")
  if(dir.exists(new_folder)){
    if(erase_old){
      unlink(x = new_folder,recursive = T, force = T)
    }
  }
  
  dir.create(new_folder, showWarnings = FALSE, recursive = TRUE)
  
  rasters <- list.files(paste0(folder_in, path_fut, algo, "/"), pattern = "*.tif")  
  if(length(rasters) >= 30){
    try(
      {
        variability <- grepl(pattern = "_sum_|_devstd_|_CV_", x = rasters)
        if(sum(variability) >= 1){
          rasters <- rasters[!variability]
        }else{
          rasters <- rasters
        }  
      }
    )
    
    # extract model information, it has to be in the standard result of fit_biomodelos function
    try(rasters2 <- gsub(pattern = "_Cor", replacement = "", x = rasters))
    rasters_str <- stringr::str_split(rasters2, "_", simplify = TRUE)
    
    mdl <- rasters_str[, 3] %>% unique()
    yr <- rasters_str[, 4] %>% unique()
    path <- rasters_str[, 5] %>% unique()
    bin <- rasters_str[, 6] %>% unique()
    try(bin <- lapply(X = bin, FUN = function(X){gsub(pattern = ".tif", replacement = "", x = X) }) |> unlist())
    index_bin <- which(bin == as.character(threshold) | bin == paste0("X",threshold), "")
    bin <- bin[index_bin]
    
    progress_bar <- txtProgressBar(min = 0, max = length(yr) * length(path) * length(bin), style = 3)
    progress_count <- 0
    
    for (i in 1:length(yr)) {
      #i <- 1
      rasters_yr_i <- rasters[grepl(pattern = yr[i], x = rasters)]
      
      for (a in 1:length(path)) {
        #a <- 1
        rasters_yr_path_a <- rasters_yr_i[grepl(pattern = path[a], x = rasters_yr_i)]
        
        for (b in 1:length(bin)) {
          #b <- 1 
          pattern_bin <- paste0("_", bin[b], ".tif|_", bin[b],"_")
          
          rasters_yr_path_bin_b <- rasters_yr_path_a[grepl(pattern = pattern_bin, x =  rasters_yr_path_a)]
          raster_data <- paste0(folder_in, path_fut, algo, "/", rasters_yr_path_bin_b) %>% 
            terra::rast()
          
          ens <- sum(raster_data)
          if(uncertainty == T){
            writeRaster(
              ens,
              paste0(folder_in, path_fut, algo, "/future_ensemble/", yr[i], "_", path[a], "_sum_", bin[b], ".tif"),
              datatype = "INT1U",
              NAflag = 255,
              overwrite = TRUE
            )
          }
          
          ens_values <- unique(ens)
          index_max <- which(ens_values == max(ens_values))
          m <- matrix(data = c(t(ens_values), rep(0, nrow(ens_values))), 
                      nrow = nrow(ens_values),
                      ncol = 2,
                      byrow = FALSE)
          m[index_max, 2] <- 1
          ens <- terra::classify(ens, m)
          writeRaster(
            ens,
            paste0(folder_in, path_fut, algo, "/future_ensemble/", yr[i], "_", path[a], "_", bin[b], ".tif"),
            datatype = "INT1U",
            NAflag = 255,
            overwrite = TRUE
          )
          
          progress_count <- progress_count + 1
          setTxtProgressBar(progress_bar, progress_count)
        }
      }
    }
    
    close(progress_bar)
    
  }
  gc()
}

spp <- as.character()

spp_i <- list.dirs("N:/peces/", full.names = F, recursive = F)
spp_i <- paste0("N:/peces/", spp_i)
spp <- c(spp, spp_i)

# tiempo sin parelizar lapply 10 especies
# user  system elapsed 
# -119.57  -26.57 -152.46

# tiempo parelizado lapply 10 especies
# execution_time
# user  system elapsed 
# -12.95   -0.36  -66.48 

invisible(lapply(X = spp, FUN = function(X){future_ensemble(folder_in = X, threshold = 10, erase_old = T)}))



# library(parallel)
#   
# num_cores <- 3
# cl <- makeCluster(num_cores)
# clusterEvalQ(cl, {
#   library(stringr)
#   library(dplyr)
#   library(terra)
#   library(parallel)
#   library(doParallel)
# })
#   
# invisible(parLapply(cl, spp, future_ensemble))
#   
# stopCluster(cl)
