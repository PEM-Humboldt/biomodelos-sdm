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

  ## import raster(bias_layer) and crop to the extent and shape of M composed

  Biasfile <- raster::raster(TGS.kernel) * 1000 %>% 
    trunc()
  
  # Bias in M
  BiasfileM <- Biasfile %>%
    raster::crop(shape.M) %>%
    raster::mask(shape.M)
  
  # round(Biasfile, digits = 0) %>%

  BiasfileM <- raster::projectRaster(BiasfileM, env.M)
  
  raster::writeRaster(BiasfileM, paste0(folder.sp, "/BiasfileM.asc"),
    overwrite = T,
    NAflag = -9999,
    datatype = "FLT4S",
    options = "COMPRESS=LZW"
  )

  data.spat <- data.

  data.spat <- sp::SpatialPointsDataFrame(coords = data.[, c(col.lon, col.lat)], data = data.frame(data.[, col.sp]))
  
  # giving a buffer to each occurrence not to get background near to register

  data.buffer <- rgeos::gBuffer(data.spat, width = res(env.M)[1])
  
  BiasfileNO_Occ <- raster::mask(BiasfileM, data.buffer, inverse = TRUE)

  BiasfilePo <- raster::rasterToPoints(BiasfileNO_Occ, spatial = F)

  colnames(BiasfilePo) <- c("lon", "lat", "prob")

  return(BiasfilePo)
}
