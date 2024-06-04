 # species distribution maps were refined following the Occurrences-based threshold restriction (OBR) 
 # approach (Mendes et al. 2020). This approach involves overlaying the binary distribution map without 
 # spatial constraints onto species occurrence records, and then selecting only the potential distribution 
 #  areas supported by concrete evidence of the species' presence. This method operates under the assumption 
 #that suitable patches that overlap with species occurrences are more likely to be components of species 
 # distributions, in contrast to suitable patches that do not intersect with any occurrences (Mendes et al. 2020).

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