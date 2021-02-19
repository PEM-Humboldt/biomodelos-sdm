## BIAS


# Adding bias TGS
# TGS uses the presence locations of taxonomically related species observed using the same
# techniques as the focal species (usually from the same database) to estimate sampling, under the
# assumption that those surveys would have recorded the focal species had it occurred there
#
# Cory Merow  Matthew J. Smith  John A. Silander Jr. Volume 36, Issue 10.
# A practical guide to MaxEnt for modeling species’ distributions: what it does, and why inputs and
# settings matter
# https://onlinelibrary.wiley.com/doi/10.1111/j.1600-0587.2013.07872.x
#
# This code was adapted from supplementary materials of Fitzpatrick et al. 2013. Ecosphere
# 4(5): Article 55. This involves creating a probability surface for the study area, with a Kernel
# Density Estimator (KDE), where higher probability values are assigned to areas with more locality   # points. Then, pseudo-absences are selected probabilistically, based on that surface.

get_BiasSp <- function(data., TGS.kernel, shape.M, env.M, ext, area.G, folder.sp,
                       col.lon = col_lon, col.lat = col_lat, col.sp = col_sp) {

  ## import raster(bias_layer) and crop to the extent and shape of M composed

  Biasfile <- raster::raster(TGS.kernel)*1000
  
  # Bias in M
  
  BiasfileM <- Biasfile %>% raster::crop(shape.M) %>%
    raster::mask(shape.M)
  
  #round(Biasfile, digits = 0) %>% 
  
  BiasfileM <- raster::projectRaster(BiasfileM, env.M)
  #BiasfileM[BiasfileM <= 0] <- 0.000009
  
  raster::writeRaster(BiasfileM, paste0(folder.sp, "/BiasfileM.asc"), overwrite = T)
  
#  if(proj.models == "M-G"){
#    # Bias in G
#    ras.G <- raster::raster(area.G)
#    
#    BiasfileG <- round(Biasfile, digits = 0) %>% 
#      raster::crop(ras.G) %>%
#      raster::mask(ras.G)
#    
#    
#    d <- crop(bias, g)
#    a <- projectRaster(from = d, to = g)
#    e <- raster::mask(a, g)
#    raster::writeRaster(e, paste0("BiasfileG.asc"), overwrite = T)
#  }

  data.spat <- data.

  data.spat <- sp::SpatialPointsDataFrame(coords = data.[, c(col.lon, col.lat)], data = data.frame(data.[, col.sp]))
  
  # giving a buffer to each occurrence not to get background near to register

  data.buffer <- rgeos::gBuffer(data.spat, width = res(env.M)[1])

  BiasfileNO_Occ <- raster::mask(BiasfileM, data.buffer, inverse = TRUE)

  BiasfilePo <- raster::rasterToPoints(BiasfileNO_Occ, spatial = F)

  colnames(BiasfilePo) <- c("lon", "lat", "prob")

  return(BiasfilePo)
}