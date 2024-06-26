#' Future Ensemble Processing
#'
#' This function processes future ensemble raster data, calculating statistics like
#' standard deviation, median, and generating binary outputs based on specified thresholds.
#'
#' The function processes raster data from the specified folder, filtering by algorithm,
#' calculating statistics such as standard deviation and median, and optionally generating
#' binary outputs based on threshold values. It handles two main methods of processing:
#' binary processing, which sums overlapping rasters to generate binary outputs, and continuous
#' processing, which calculates statistics like median and standard deviation.
#'
#' folder_in, Character: Path to the main folder containing the ensemble raster data.
#' algo, Character: Algorithm used for modeling (e.g., "MAXENT").
#' threshold, Character: In case of `ensemble_method = "binary"` threshold that is going to be ensembled. If 
#' `ensemble_method = "continuos" threshold value for binarizing media of each future scenario.
#' erase_old, Logical: indicating whether to erase existing output folders.
#' ensemble_method, Character: Method for processing ensemble data. Options are "binary" or "continuos".
#' uncertainty, Logical: indicating whether to calculate uncertainty metrics. If `ensemble_method = "binary"
#' is equal to the sum of the thresholds otherwise is the standard deviation of each future scenario.
#'
#' return This function generates processed raster outputs in the specified output folder.
#'
#' examples
#' future_ensemble(folder_in = "/path/to/ensemble_data", algo = "MAXENT", threshold = 10,
#'                 erase_old = TRUE, ensemble_method = "binary", uncertainty = FALSE)

future_ensemble <- function(folder_in, algo = "MAXENT", threshold = 10, erase_old = TRUE, 
                            ensemble_method = "binary", uncertainty = FALSE) {
  
  message(folder_in)
  
  # Define paths and create new folders if necessary
  path_fut <- "/ensembles/future/"
  new_folder <- paste0(folder_in, path_fut, algo, "/future_ensemble")
  
  if (dir.exists(new_folder) && erase_old) {
    unlink(x = new_folder, recursive = TRUE, force = TRUE)
  }
  
  dir.create(new_folder, showWarnings = FALSE, recursive = TRUE)
  
  # Read and filter raster files
  rasters <- list.files(paste0(folder_in, path_fut, algo, "/"), pattern = "*.tif")
  
  if (grepl(pattern = "_sum_", x = rasters) < 2) {
    stop("Not enough rasters to process.")
  }
  
  variability <- grepl(pattern = "_sum_|_devstd_|_CV_", x = rasters)
  rasters <- rasters[!variability]
  
  rasters2 <- gsub(pattern = "_Cor", replacement = "", x = rasters)
  rasters_str <- stringr::str_split(rasters2, "_", simplify = TRUE)
  
  mdl <- unique(rasters_str[, 3])
  yr <- unique(rasters_str[, 4])
  path <- unique(rasters_str[, 5])
  bin <- unique(rasters_str[, 6])
  bin <- sapply(bin, function(x) gsub(pattern = ".tif", replacement = "", x))
  
  index_bin <- which(bin == as.character(threshold) | bin == paste0("X", threshold))
  bin <- bin[index_bin]
  
  progress_bar <- txtProgressBar(min = 0, max = length(yr) * length(path) * length(bin), style = 3)
  progress_count <- 0
  
  # Read threshold values
  bin_table <- read.csv(paste0(folder_in, "/ensembles/current/", algo, "/binValues_", algo, ".csv"))
  if(threshold != 0){
    bin_value <- bin_table[1, grepl(pattern = threshold, x = names(bin_table))]  
  } else {
    bin_value <- bin_table[1, c(names(bin_table) == 0 | names(bin_table) == "X0")]
  }
  
  # Main Processing Loop
  for (i in seq_along(yr)) {
    rasters_yr_i <- rasters[grepl(pattern = yr[i], x = rasters)]
    
    for (a in seq_along(path)) {
      rasters_yr_path_a <- rasters_yr_i[grepl(pattern = path[a], x = rasters_yr_i)]
      
      for (b in seq_along(bin)) {
        
        if(ensemble_method == "continuos"){

          rasters_yr_path_a <- rasters_yr_i[grepl(pattern = path[a], x = rasters_yr_i)]     
          pattern_con <- paste0("_", continuo)      
          raster_data <- paste0(folder_in, path_fut, algo, "/", rasters_yr_path_a) %>% 
            terra::rast()

          # Calculate standard deviation
          if (uncertainty) {
            ens_devStd <- round(terra::app(raster_data, sd), 3)
            writeRaster(ens_devStd, paste0(new_folder, "/", yr[i], "_", path[a], "_devstd_maxent.tif"), 
                        NAflag = -9999, overwrite = TRUE, datatype = "FLT4S")
          }
          
          # Calculate median
          if (continuos_median) {
            ens_median <- round(median(raster_data), 3)
            names(ens_median) <- paste0(folder_in, "_", yr[i], "_", path[a], "_maxent.tif")
            writeRaster(ens_median, paste0(new_folder, "/", yr[i], "_", path[a], "_maxent.tif"), 
                        NAflag = -9999, overwrite = TRUE, datatype = "FLT4S")
          }

          if (continuos_binary) {
            binary <- (ens_median >= bin_value) * 1
            binary <- terra::classify(binary, cbind(0, NA), cbind(1, 1))
            writeRaster(binary, paste0(new_folder, "/", yr[i], "_", path[a], "_", threshold, ".tif"), 
                        datatype = "INT1U", NAflag = 255, overwrite = TRUE)
          }
        }
        
        if(ensemble_method == "binary"){

          pattern_bin <- paste0("_", bin[b], ".tif|_", bin[b], "_")
          rasters_yr_path_bin_b <- rasters_yr_path_a[grepl(pattern = pattern_bin, x = rasters_yr_path_a)]
          raster_data <- terra::rast(paste0(folder_in, path_fut, algo, "/", rasters_yr_path_bin_b))

          ens <- sum(raster_data)
            ens_values <- unique(ens)
            index_max <- which(ens_values == max(ens_values))
            m <- matrix(data = c(t(ens_values), rep(0, nrow(ens_values))), 
                        nrow = nrow(ens_values),
                        ncol = 2,
                        byrow = FALSE)
            m[index_max, 2] <- 1
            binary <- terra::classify(ens, m)

            writeRaster(binary, paste0(new_folder, "/", yr[i], "_", path[a], "_", threshold, ".tif"), 
                      datatype = "INT1U", NAflag = 255, overwrite = TRUE)

          if (uncertainty) {
            ens_sum <- sum(raster_data)
            writeRaster(ens_sum, paste0(new_folder, "/", yr[i], "_", path[a], "_sum_", bin[b], ".tif"), 
                        datatype = "INT1U", NAflag = 255, overwrite = TRUE)
          }
          
        }        
        
        progress_count <- progress_count + 1
        setTxtProgressBar(progress_bar, progress_count)
      }
    }
  }
  
  close(progress_bar)
  gc()
}

spp <- list.dirs("path_to_models", full.names = T, recursive = F)

# tiempo sin parelizar lapply 10 especies
# user  system elapsed 
# -119.57  -26.57 -152.46

invisible(lapply(X = spp, FUN = function(X){future_ensemble(folder_in = X, threshold = 10, erase_old = T,
            ensemble_method = "binary", uncertainty = FALSE)}))

# tiempo parelizado lapply 10 especies
# execution_time
# user  system elapsed 
# -12.95   -0.36  -66.48 

library(parallel)
  
num_cores <- 3
cl <- makeCluster(num_cores)
clusterEvalQ(cl, {
   library(stringr)
   library(dplyr)
   library(terra)
   library(parallel)
   library(doParallel)
})
  
invisible(parLapply(cl, spp, future_ensemble))

stopCluster(cl)
