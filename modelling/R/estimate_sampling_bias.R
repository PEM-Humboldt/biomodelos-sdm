#' Estimate the bias for a set of species occurrences
#'
#' @description Calculates the bias of a set of species occurrences using the Target Group Sampling (TGS) method.
#' TGS estimates sampling by using the presence locations of taxonomically related species observed
#' using the same techniques as the focal species, under the assumption that those surveys would have
#' recorded the focal species had it occurred there. This function creates a probability surface for
#' the study area, with a Kernel Density Estimator (KDE), where higher probability values are assigned
#' to areas with more locality points. Then, pseudo-absences are selected probabilistically, based on that surface.
#' 
#' @param data. data frame containing the species occurrences.
#' @param TGS.kernel character string of the path to the raster file with the kernel for TGS.
#' @param shape.M shapefile with the extent and shape of the study area.
#' @param env.M raster stack of environmental variables for the study area.
#' @param folder.sp character string folder path to save the output files.
#' @param col.lon character string of column name for the longitude in the data frame.
#' @param col.lat character string of column name for the latitude in the data frame.
#' @param col.sp character string of column name for the species names in the data frame.
#' 
#' @details TGS uses the presence locations of taxonomically related species observed using the same techniques as 
#' the focal species (usually from the same database) to estimate sampling, under the assumption that those surveys 
#' would have recorded the focal species had it occurred there
#'  
#' Cory Merow  Matthew J. Smith  John A. Silander Jr. Volume 36, Issue 10. A practical guide to MaxEnt for modeling
#' species’ distributions: what it does, and why inputs and settings matter
#' https://onlinelibrary.wiley.com/doi/10.1111/j.1600-0587.2013.07872.x
#' This code was adapted from supplementary materials of Fitzpatrick et al. 2013. Ecosphere 4(5): Article 55. This 
#' involves creating a probability surface for the study area, with a Kernel Density Estimator (KDE), where higher 
#' probability values are assigned to areas with more locality points. Then, pseudo-absences are selected 
#' probabilistically, based on that surface.
#' 
#' @return A data frame with the latitude, longitude, and probability of occurrence for each cell in the 
#' probability surface created by the KDE.

estimate_sampling_bias <- function(data. = interest_areas$occurrences, TGS.kernel = "bias_layer/aves.tif",
                       shape.M = interest_areas$shape_M, env.M = envars$M, folder.sp = folder_sp, 
                       col.lon = col_lon, col.lat = col_lat, col.sp = col_sp) {

  Biasfile <- rast(TGS.kernel) * 1000
  Biasfile <- terra::round(Biasfile)
  
  # Read and transform shape.M to match the CRS of Biasfile
  if(crs(shape.M) != crs(Biasfile)){
    shape.M <- st_as_sf(shape.M) %>% 
      st_transform(crs(Biasfile)) %>% 
      vect()
  }

  # Bias in M
  BiasfileM <- crop(Biasfile, shape.M) %>%
    mask(shape.M)
  
  # Project BiasfileM to the CRS of env.M
  env.M <- rast(env.M)
  if(crs(BiasfileM) != crs(env.M)){
    BiasfileM <- project(BiasfileM, env.M)
  }
  
  terra::writeRaster(BiasfileM, paste0(folder.sp, "/BiasfileM.tif"), overwrite = TRUE, 
              NAflag = -9999, datatype = "FLT4S")
  
  # Create SpatialPointsDataFrame from data
  data.spat <- st_as_sf(data., coords = c(col.lon, col.lat), crs = st_crs(BiasfileM))
  
  # Create a buffer around each occurrence point
  data.buffer <- st_buffer(data.spat, dist = res(env.M)[1])
  
  # Mask BiasfileM with buffer to exclude occurrences
  BiasfileNO_Occ <- mask(BiasfileM, vect(data.buffer), inverse = TRUE)
  
  # Convert masked raster to points
  BiasfilePo <- as.data.frame(BiasfileNO_Occ, xy = T)
  colnames(BiasfilePo) <- c("lon", "lat", "prob")
  
  return(BiasfilePo)
}
