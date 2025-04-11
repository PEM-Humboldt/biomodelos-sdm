#' Refine Species Distribution Model Points
#'
#' Refines a species distribution model by overlaying occurrence points, ensuring spatially constrained distribution patches
#' using observed occurrences
#' 
#' map, Raster: Binary distribution map to refine.
#' sp.points, SpatialPointsDataFrame: Species occurrence points.
#'
#' return Raster object representing the refined distribution map.
#'
#' references
#' Mendes et al. (2020). Occurrences-based threshold restriction (OBR) approach for refining species
#' distribution maps.


library(stringr)
library(terra)
library(sf)
library(dplyr)

refine_model_using_occurrences <- function(map, sp.points) {
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

### Limitations and Considerations

- **More computationally intensive**: Converting from raster to polygons and back can be slower and more memory-demanding, especially with large spatial datasets.

- **Increased dependency on `sf`**: While powerful, using the `sf` package introduces additional complexity related to projections, geometric precision, and data conversions.

- **Assumes geographic coordinates**: The function assumes that coordinates are in latitude/longitude (`+proj=longlat`), which may not hold true for all raster inputs.

- **Loss of original values**: The output raster assigns a value of 1 to all selected areas and does not retain the original suitability or habitat values from the input raster.
