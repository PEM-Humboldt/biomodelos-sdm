# Description:
#   calculate_future_refugia  calculates future climatic refugia based on present and future distribution projections 
#   raster data in tif format. It takes an input folder folder_in containing the raster files and saves the results to 
# an output folder folder_out.
# 
# Arguments:
#   
# folder_in: The path to the input folder containing the raster files.
# folder_out: The path to the output folder where the results will be saved.
# algo (optional): The algorithm used for calculating refuges. The default value is "MAXENT".
# threshold (optional): The threshold used for raster classification. The default value is 10.

# Details:
#   
#   The function uses the threshold value to construct a search pattern in the raster file names.
# It loads the current and future rasters from the specified paths in the input folder.
# Performs raster classification using a classification matrix m.
# Applies a mask to the future rasters using the current rasters.
# Calculates the sum of the current and future rasters to obtain the number of refugees.
# Saves the current rasters and future refugee rasters in the corresponding output folders.
#
# Return Value:
#   The function does not explicitly return a value, but it saves the output rasters in the specified folders.
#

library(stringr)
library(terra)
library(sf)
library(dplyr)

refine_model_points <- function(map, sp.points) {
  tmp.mask <- map >= 0
  map[map == 0] <- NA
  
  map.patch <- suppressWarnings(terra::as.polygons(map, na.rm = TRUE) |> st_as_sf() |> st_cast("POLYGON"))
  pts <- suppressWarnings(st_as_sf(sp.points, coords = c("X", "Y"), crs = "+proj=longlat +datum=WGS84"))
  map.patch <- suppressWarnings(map.patch[pts,] |> terra::vect())
  
  map.raster <- suppressWarnings(rast(map.patch, nrows = nrow(tmp.mask), ncols = ncol(tmp.mask), res = res(tmp.mask), 
                                      ext = ext(map)))
  values(map.raster) <- NA
  map.raster[map.patch] <- 1
  terra::crs(map.raster) <- terra::crs(map)
  names(map.raster) <- names(map)
  
  return(map.raster)
  
  rm(map, map.patch, pts)
  invisible(gc())
  

}

calculate_future_refugia_bin <- function(folder_in = spp[1], folder_out, algo = "MAXENT", threshold = 10, refine = T,
                                         shp_to_mask = "data/Colombia_FINAL_wwgs84.shp", uncertainty = F) {
  
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
      rasters_current <- refine_model_points(map = rasters_current, sp.points = occ_)
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

data_ <- list.files("postprocesamiento/refugios_2030/presente/", pattern = ".tif")
data_ <- stringr::str_split(string = data_, pattern = "_", simplify = T )
data_ <- paste0(data_[ , 1], ".", data_[ , 2])
write.csv(data_, "spp_procesed_29062023.csv", row.names = F)

spp <- as.character()

# Plantas: hecho a 28062023
spp_i <- list.dirs("M:/BioModelos_4k_2022-2023/plantas/", full.names = F, recursive = F)
spp_i <- paste0("M:/BioModelos_4k_2022-2023/plantas/", spp_i)
spp <- c(spp, spp_i)

# peces
spp_i <- list.dirs("N:/peces/", full.names = F, recursive = F)
spp_i <- paste0("N:/peces/", spp_i)
spp <- c(spp, spp_i)

# aves
spp_i <- list.dirs("N:/aves/", full.names = F, recursive = F)
index <- !(spp_i %in% data_)
spp_i <- spp_i[index]
spp_i <- paste0("N:/aves/", spp_i)
spp <- c(spp, spp_i)

# mamiferos
spp_i <- list.dirs("N:/mamiferos/", full.names = F, recursive = F)
index <- !(spp_i %in% data_)
spp_i <- spp_i[index]
spp_i <- paste0("N:/mamiferos/", spp_i)
spp <- c(spp, spp_i)

# squamata
spp_i <- list.dirs("N:/squamata/", full.names = F, recursive = F)
index <- !(spp_i %in% data_)
spp_i <- spp_i[index]
spp_i <- paste0("N:/squamata/", spp_i)
spp <- c(spp, spp_i)

# crocodilia/reptilia
spp_i <- list.dirs("N:/crocodylia_reptilia/", full.names = F, recursive = F)
index <- !(spp_i %in% data_)
spp_i <- spp_i[index]
spp_i <- paste0("N:/crocodylia_reptilia/", spp_i)
spp <- c(spp, spp_i)

# anfibios
spp_i <- list.dirs("N:/anfibios/", full.names = F, recursive = F)
spp_i <- paste0("N:/anfibios/", spp_i)
spp <- c(spp, spp_i)


indexStop <- which(spp == "M:/WW/Dalechampia.dioscoreifolia")

invisible(lapply(X = spp, FUN = function(X){calculate_future_refugia_bin(folder_in = X, 
                        folder_out = "G:/BioModelos_4k2022-2023/postprocesamiento/peces", 
                        threshold = 10, refine = T)}))


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

