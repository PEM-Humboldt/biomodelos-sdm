# Estimate distribution by MCP

do.DE.MCP <-function(occ., col.lon = col_lon, col.lat = col_lat, folder.sp = folder_sp,
                     dist.Mov = dist_MOV){
  
  MCP <- do.MCP(
    dat = occ.,
    collon = col.lon,
    collat = col.lat,
    distMov = dist.Mov)
  
  raster.ref <- raster::raster(res= 1/120) %>% 
    crop(MCP) # raster 1 km 
  
  MCP.rasterized <- raster::rasterize(MCP, raster.ref)
  
  writeRaster(MCP.rasterized, paste0(folder.sp, "/","DE_mcp.tif"), overwrite = TRUE)
  
}

do.MCP <- function(dat, collon, collat, distMov){
  
  # lon lat  data frame to spatial points data frame
  
  occs.xy <- dat[, c(collon, collat)]
  occs.xy <- sp::coordinates(occs.xy)
  
  # spatial points
  
  xy <- as.data.frame(sp::coordinates(occs.xy))
  coords.t <- chull(xy[, 1], xy[, 2])
  xy.bord <- xy[coords.t, ]
  xy.bord <- rbind(xy.bord[nrow(xy.bord), ], xy.bord)
  bgMCP <- SpatialPolygons(list(Polygons(list(Polygon(as.matrix(xy.bord))), 1)))
  
  # from km to grades
  dist.buf <- distMov/111
  
  bgMCPbuf <- rgeos::gBuffer(spgeom = bgMCP, width = dist.buf)
  
  return(bgMCPbuf)
  
}