do.ensemble <- function(respathA, respathB1, respathB2, do.future,
                        occ., threshold., col.lon, col.lat, folder.sp,
                        crs.proyect){
  
  # empty raster to save layers from modeling process 
  ras_current <- raster::stack()
  
  # results from small samples maxent modeling
  if(!is.null(respathA)){
    ras_current <-  raster::stack(respathA$c_proj, ras_current)
  }
  
  # results from large samples maxent modeling
  if(!is.null(respathB1)){
    ras_current <- raster::stack(respathB1$c_proj, ras_current)
  }
  
  # results from small samples other modeling
  if(!is.null(respathB2)){
    ras_biomod01 <- (respathB2$c_proj)/1000
    ras_current <- raster::stack(ras_biomod01, ras_current)
  }
  
    raster::crs(ras_current) <- sp::CRS(crs.proyect)
  # MISSING only one model
  
  # branch for species with only one best model
  if(raster::nlayers(ras_current) < 1){
    
    stop("Number of models is one (1). Cannot compute ensembles")
  
  }else if(raster::nlayers(ras_current) > 1){
  
  # branch for species with more than one best model
  
  # extracting values for all occurrences from models and organize in a data.frame  
  rasvalue <- raster::extract(ras_current, occ.[ , c(col.lon, col.lat)]) %>%
    na.omit() %>% apply(., MARGIN = 2, sort) %>% as.data.frame()
  
  # percentile data of E
  EData <- ceiling((nrow(occ.) * threshold.) / 100)
  
  # value for each model to binarize to E
  EValue <- rasvalue[EData, ]
  
  # folder to save ensembles
  dir.create(paste0(folder.sp, "/ensembles"), showWarnings = FALSE)
  dir.create(paste0(folder.sp, "/ensembles/current"), showWarnings = F)

  # writing data.frame of E values to convert to binary with E
  write.csv(EValue, paste0(folder.sp, "/ensembles/current/EValue.csv"), row.names = F)
  
  ## Converting continuous prediction to binary according to bin_threshold
  
  binE <- raster::stack()
  for(i in 1:raster::nlayers(ras_current)){
    EValue_layer <- EValue[ , i]
    bin_layer <- ras_current[[i]] > EValue_layer
    binE <- raster::stack(binE, bin_layer)
  }
  
  names(binE) <- names(ras_current)
  
  dir.create(paste0(folder.sp, "/ensembles/current/binariesE"), showWarnings = F)
  for(i in 1:raster::nlayers(binE)){
    writeRaster(binE[[i]], paste0(
      folder.sp, "/ensembles/current/binariesE/",
      names(binE[[i]]), "binE.tif"
    ),
    format = "GTiff",
    overwrite = T
    )
  }
  
  # variation coeficient function
  CV <- function(x, na.rm = TRUE) {
    sd(x, na.rm=na.rm) / mean(x, na.rm=na.rm)
  }
  
  # taking statistics from binary stack
  binE_med <- raster::calc(x = binE, median)
  binE_devstd <- raster::calc(x = binE, sd)
  binE_cv <- raster::calc(x = binE, CV)
  binE_sum <- raster::calc(x =binE, sum)/nlayers(binE)
  
  # stacking results of ensembles
  Resensembles <- stack(binE_med, binE_devstd, binE_cv, binE_sum)
  names(Resensembles) <- c(paste0(folder.sp,"_mediana"), paste0(folder.sp,"_devstd"), 
                           paste0(folder.sp,"_CV"), paste0(folder.sp,"_sumNorm"))
  # writing ensembles
  for(i in 1:raster::nlayers(Resensembles)){
    writeRaster(Resensembles[[i]], paste0(
      folder.sp, "/ensembles/current/",
      names(Resensembles[[i]]), ".tif"
    ),
    format = "GTiff",
    overwrite = T
    )
  }
  
  }
  
  #if(do.future == T){
  #  
  #}
     
}

