#' Calculate Future Climatic Refugia
#'
#' calculate_future_refugia_bin() function calculates future climatic refugia based on present and future
#' distribution projections in raster data (tif format). It takes an input folder containing the raster
#' files and saves the results to an output folder.
#'
#' folder_in, character: The path to the input folder containing the raster files.
#' folder_out The path to the output folder where the results will be saved.
#' algo, character (optional): The algorithm used for calculating refuges. Default is "MAXENT".
#' threshold, character (optional): The threshold used for raster classification. Default is 10.
#' refine, Logical: indicating whether to refine the model using occurrence points. Default is TRUE.
#' shp_to_mask, character (optional): Path to a shapefile used for masking. Default is "data/Colombia_FINAL_wwgs84.shp".
#' uncertainty, Logical: indicating whether to calculate uncertainty. Default is FALSE.
#'
#' details
#' This function uses the threshold value to construct a search pattern in the raster file names. It loads
#' current and future rasters from the specified paths in the input folder. It performs raster classification
#' using a classification matrix `m`. If `refine` is TRUE, it refines the current rasters using
#' occurrence points. It masks future rasters using current rasters, calculates the sum of current and future
#' rasters to obtain the number of refugia, and saves the results in the specified output folders.
#'
#' return
#' The function does not explicitly return a value but saves the output rasters in the specified folder_out.
#'
#' examples
#' calculate_future_refugia_bin(folder_in = "path_to_input_folder", folder_out = "path_to_output_folder",
#'                             algo = "MAXENT", threshold = 10, refine = TRUE,
#'                             shp_to_mask = "path_to_shapefile", uncertainty = FALSE)
#'
#' import stringr terra sf dplyr
#' importFrom parallel makeCluster clusterEvalQ clusterExport stopCluster
#' 
#'
#' seealso: refine_model_using_occurrences
#'
#' references:
#' Brambilla, M., Rubolini, D., Appukuttan, O., Calvi, G., Karger, D. N., Kmecl, P., Mihelič, T., Sattler, T., Seaman, B., 
#' Teufelbauer, N., Wahl, J., y Celada, C. (2022). Identifying climate refugia for high-elevation Alpine birds under current
#' climate warming predictions. Global Change Biology, 28, 4276– 4291. https://doi.org/10.1111/gcb.16187
#' Araújo, M. B., y New, M. (2007). Ensemble forecasting of species distributions. Trends in ecology & evolution, 22(1), 42-47.


library(stringr)
library(terra)
library(sf)
library(dplyr)

source(".../postprocessing/R/refine_model_using_occurrences.R")

calculate_future_refugia_bin <- function(folder_in = spp[1], folder_out, algo = "MAXENT", threshold = 10, refine = T,
                                         shp_to_mask = ".../modelling/Data/Colombia_FINAL_wwgs84.shp", uncertainty = F) {
  
  message(folder_in)
  
  #conversion matrix
  m <- rbind(c(0, NA), c(1, 1)) # convert to one class
  m2 <- rbind(c(NA, 0), c(1, 0), c(2, 1)) # from three classes to two classes
  m3 <- rbind(c(NA, 0), c(1, 1)) # reconvert to two classes 
  
  
  dir.create(paste0(folder_in, "/Temp"), showWarnings = FALSE)
  terra::terraOptions(tempdir = paste0(folder_in, "/Temp"))

  str_threshold <- paste0("_X", threshold, "|_", threshold)

  spp_ <- str_split(string = folder_in, pattern = "/") |> 
    unlist()
  spp_ <- spp_[length(spp_)] |>
    gsub(pattern = "\\.", replacement = "_")
  
  path_current_ensambles <- file.path(folder_in, "ensembles", "current", algo)
  if(dir.exists(path_current_ensambles)){
    rasters_current <- list.files(path_current_ensambles, pattern = "*.tif$")    
    index_current <- grepl(pattern = str_threshold, x = rasters_current)
    nm_current <- rasters_current[index_current]
    rasters_current <- paste0(path_current_ensambles, "/", nm_current) |>
      terra::rast() |>
      terra::classify(m)
    names(rasters_current) <- nm_current
    
    if(refine){
      occ_fl <- list.files(paste0(folder_in, "/occurrences/"), full.names = T) 
      occ_path <- occ_fl[grepl(pattern = "joint", x = occ_fl)]
      occ_ <- read.csv(occ_path)
      cols <- c("lon","lat","decimalLongitude",	"decimalLatitude", "latitude", "longitude", "LONG_X", "LAT_Y")
      cols_exist <- cols[cols %in% colnames(occ_)]
      occ_ <- dplyr::select(occ_, cols_exist)
      occ_ <- occ_ %>%
        rename(X = all_of(cols_exist[1]), Y = all_of(cols_exist[2]))
      rasters_current <- refine_model_using_occurrences(map = rasters_current, sp.points = occ_)
      message("refine [step 1]")
    }
    
    path_fut <- list.files(path = file.path(folder_in, "ensembles", "future", algo), pattern = ".tif") |> length()
    if(path_fut >= 30){
      path_fut_ensambles <- file.path(folder_in, "ensembles", "future", algo, "future_ensemble")
      
      # Verificar si el archivo path_fut_ensambles existe
      iter <- 1
      while (!file.exists(path_fut_ensambles)) {
        # Imprimir mensaje de espera y número de iteración
        message(paste("Esperando archivo en iteración", iter))
        
        # Esperar 10 minutos antes de revisar nuevamente
        Sys.sleep(1 * 60)  # 10 minutos * 60 segundos/minuto
        
        iter <- iter + 1
      }
      
      rasters_orig <- list.files(path_fut_ensambles, pattern = "*.tif")
      
      # consensos
      index_fut <- grepl(pattern = str_threshold, x = rasters_orig ) & !grepl(pattern = "sum", x = rasters_orig )
      nm_fut <- rasters_orig[index_fut]
      
      if(!file.exists(paste0(path_fut_ensambles, "/", nm_fut)[1])){
        stop("future file doesnt exist, are you sure threshold consensus was calculated?")
      }
      
      rasters_fut <- paste0(path_fut_ensambles, "/", nm_fut) |>
        terra::rast() |> 
        mask(rasters_current)
      
      
      refugees <- sum(rasters_current, rasters_fut) |> 
        terra::classify(m2)
      names(refugees) <- paste0(spp_, "_", nm_fut)
      
      # reconvert to two classes current period. It is easty to transform
      rasters_current <- terra::classify(rasters_current, m3)
      message("refugia¨[step 2]")
      # sumas
      if(uncertainty == T){
        index_fut_sum <- grepl(pattern = str_threshold, x = rasters_orig ) & grepl(pattern = "sum", x = rasters_orig )
        nm_fut_sum <- rasters_orig [index_fut_sum]
        refugees_inc_sum <- paste0(path_fut_ensambles, "/", nm_fut_sum) |>
          terra::rast() |> 
          mask(rasters_current)
        
        for(i in 1:nlyr(refugees_inc_sum)){
          refugees_inc_sum[[i]] <- (refugees_inc_sum[[i]]/terra::minmax(refugees_inc_sum[[i]])[2,1]) |>
            round(2)*100
        }  
        names(refugees_inc_sum) <- paste0(spp_, "_", nm_fut_sum)
      }
      
      if(!is.null(shp_to_mask)){
        shp_mask <- terra::vect(shp_to_mask)
        rasters_current <- rasters_current |> terra::crop(shp_mask) |> terra::mask(shp_mask)
        refugees <- refugees |> terra::crop(shp_mask) |> terra::mask(shp_mask)
        if(exists("refugees_inc_sum")){
          refugees_inc_sum <- refugees_inc_sum |> terra::crop(shp_mask) |> terra::mask(shp_mask)
        } 
        rm(shp_mask)
      } 
      
      # folders to write
      folder_out_current <- paste0(folder_out, "/", "presente")
      dir.create(folder_out_current, showWarnings = F)
      
      # write rasters in diferent folder
      
      writeRaster(
        rasters_current,
        paste0(folder_out_current, "/", names(rasters_current)),
        datatype = "INT1U",
        NAflag = 255,
        overwrite = TRUE
      )
      
      for(i in 1:length(nm_fut)){
        #i <- 1
        nm_fut_i <- gsub(pattern = ".tif", replacement = "", x = nm_fut[i])
        folder_out_fut_i <- paste0(folder_out, "/", nm_fut_i)  
        dir.create(folder_out_fut_i, showWarnings = F, recursive = T) 
        
        writeRaster(
          refugees[[i]],
          paste0(folder_out_fut_i, "/", names(refugees)[i]),
          datatype = "INT1U",
          NAflag = 255,
          overwrite = TRUE
        )
        
        if(uncertainty == TRUE){
          writeRaster(
            refugees_inc_sum,
            paste0(folder_out_spp_fut, "/", names(refugees_inc_sum)),
            datatype = "INT1U",
            NAflag = -255,
            overwrite = TRUE
          )
          rm(refugees_inc_sum)
        }
      }
      
      try(rm(rasters_current, refugees))
      invisible(gc())
    }  
  }
  unlink(paste0(folder_in, "/Temp"), recursive = T, force = T)
}

# Plantas: hecho a 28062023
spp <- list.dirs("path_to_models", full.names = T, recursive = F)

# sequential processing

invisible(lapply(X = spp, FUN = function(X){calculate_future_refugia_bin(folder_in = X, 
                        folder_out = "G:/BioModelos_4k2022-2023/postprocesamiento/peces", 
                        threshold = 10, refine = T)}))


# parallel processing

library(parallel)

num_cores <- 3
cl <- makeCluster(num_cores)
clusterEvalQ(cl, {
  library(stringr)
  library(terra)
  library(parallel)
  library(doParallel)
  library(sf)
  library(dplyr)
})

# Export necessary libraries and functions to worker nodes
clusterExport(cl, c("calculate_future_refugia_bin", "refine_model_points"))

parLapply(cl = cl, X = spp, 
                    fun = function(X) calculate_future_refugia_bin(folder_in = X, 
                    folder_out = "G:/BioModelos_4k2022-2023/postprocesamiento/refugios_2030"))

stopCluster(cl)

