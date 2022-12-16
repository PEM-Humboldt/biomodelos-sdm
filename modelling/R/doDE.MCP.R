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

