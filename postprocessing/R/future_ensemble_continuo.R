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

future_ensemble_continuo <- function(folder_in =spp[1787], algo = "MAXENT", threshold = 10, devstd = TRUE, continuos_median = TRUE,
                                     continuos_binary = FALSE, erase_old = FALSE) {
  
  message(folder_in)
  
  # values of thresholds
  bin_table <- read.csv(paste0(folder_in, "/ensembles/current/MAXENT/binValues_MAXENT.csv"))
  if(threshold != 0){
    bin_value <- bin_table[1, grepl(pattern = threshold, x = names(bin_table))]  
  }else{
    bin_value <- bin_table[1, c(names(bin_table) == 0 | names(bin_table) == "X0")]
  }
  
  path_fut <- "/ensembles/future/"
  
  new_folder <- paste0(folder_in, path_fut, algo, "/future_ensemble")
  if(dir.exists(new_folder)){
    if(erase_old){
      unlink(x = new_folder,recursive = T, force = T)
    }
  }
  
  dir.create(new_folder, showWarnings = FALSE, recursive = TRUE)
  
  rasters <- list.files(paste0(folder_in, path_fut, algo, "/"), pattern = "*.tif")  
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
  continuo <- rasters_str[, 6]
  index_continuo <- which(continuo == "MAXENT.tif"| continuo == "maxent.tif")
  
  rasters <- rasters[index_continuo]
  
  progress_bar <- txtProgressBar(min = 0, max = length(yr) * length(path), style = 3)
  progress_count <- 0
  
  for (i in 1:length(yr)) {
    #i <- 1
    rasters_yr_i <- rasters[grepl(pattern = yr[i], x = rasters)]
    
    for (a in 1:length(path)) {
      #a <- 1
      rasters_yr_path_a <- rasters_yr_i[grepl(pattern = path[a], x = rasters_yr_i)]
      
      pattern_con <- paste0("_", continuo)
      
      raster_data <- paste0(folder_in, path_fut, algo, "/", rasters_yr_path_a) %>% 
        terra::rast()
      
      if(devstd){
        ens_devStd <- terra::app(x = raster_data, sd) %>% round(3)
        
        # error propagation?
        # if(sum(variability) >= 1){
        #   
        #   legacy_var <- list.files(paste0(folder_in, path_fut, algo, "/"), pattern = "*.tif")[variability]
        #   pattern_con <- paste0("*_devstd_", "*&*", yr[i], "_", path[a],"_*")
        #   legacy_devStd <- legacy_var[grepl(pattern = pattern_con, x = legacy_var)]
        #    
        #   
        # }
        
        writeRaster(
          ens_devStd,
          paste0(folder_in, path_fut, algo, "/future_ensemble/", yr[i], "_", path[a],"_devstd_maxent.tif"),
          NAflag = -9999,
          overwrite = TRUE,
          datatype = "FLT4S"
        )
      }
      # standard deviation
     
      
      # PCAm proticol: https://doi.org/10.1016/j.ecolmodel.2021.109502
      # sampt <- dismo::randomPoints(raster::raster(raster_data), 10000)
      # sampt <- terra::extract(raster_data, sampt)
      # 
      # pca_ <- princomp(sampt, scores = F, cor = F)
      # l <- pca_$loadings
      # l_dataframe <-  data.frame(matrix(as.numeric(l), attributes(l)$dim, dimnames=attributes(l)$dimnames))
      # 
      # selected_model <- l_dataframe[order(l_dataframe$Comp.1, decreasing = F)[1:(nrow(l_dataframe)/2)], ]
      # row.names(selected_model)
      # 
      # names(ens)
      
      if(continuos_median){
        ens <- median(raster_data) %>% round(3) %>% mask(ens_devStd)
        names(ens) <- paste0(folder_in, "_", yr[i], "_", path[a], "_maxent.tif")
        writeRaster(
          ens,
          paste0(folder_in, path_fut, algo, "/future_ensemble/", yr[i], "_", path[a], "_maxent.tif"),
          NAflag = -9999,
          overwrite = TRUE,
          datatype = "FLT4S"
        )
      }
     
      
      # binario
      if(continuos_binary){
        binary <- (ens >= bin_value)*1
        m <- rbind(c(0, NA), c(1, 1)) #clasificar rasters
        binary <- terra::classify(binary, m)
        names(binary) <- paste0( yr[i], "_", path[a], "_", threshold, "_maxent.tif")
        
        writeRaster(
          binary,
          paste0(folder_in, path_fut, algo, "/future_ensemble/", yr[i], "_", path[a], "_", threshold, ".tif"),
          datatype = "INT1U",
          NAflag = 255,
          overwrite = TRUE
        )
      }
      
      progress_count <- progress_count + 1
      setTxtProgressBar(progress_bar, progress_count)
      
    }
  }
  
  close(progress_bar)
  gc()
}

spp_problemas <- read.csv("plantas_problemas.csv")[,1]
spp <- list.dirs("modelos/plantas/", full.names = F, recursive = F)
spp <- spp[!( spp %in% spp_problemas)]
spp <- paste0("modelos/plantas/", spp)

# tiempo sin parelizar lapply 10 especies
# user  system elapsed 
# -119.57  -26.57 -152.46 

#invisible(lapply(spp[1:10], future_ensemble_continuo))

# tiempo parelizado lapply 10 especies
# execution_time
# user  system elapsed 
# -12.95   -0.36  -66.48 

library(parallel)

num_cores <- 4
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
