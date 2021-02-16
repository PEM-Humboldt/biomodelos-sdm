do.ensemble <- function(resenmEval, resKuenm, resBiomod, do.future,
                        occ., threshold., col.lon, col.lat, folder.sp){
  
  ras_current <- raster::stack()
  
  if(!is.null(NULL)){
    ras_current <-  raster::stack(resenmEval$c_proj, ras_current)
  }
  
  if(!is.null(resKuenm)){
    ras_current <- raster::stack(resKuenm$c_proj, ras_current)
  }
  
  if(!is.null(resBiomod)){
    ras_biomod01 <- (resBiomod$c_proj)/1000
    ras_current <- raster::stack(ras_biomod01, ras_current)
  }
  
  # MISSING only one model
  
  if(raster::nlayers(ras_current) < 1){
    stop("Number of models is one (1). Cannot compute ensembles")
  
  }else if(raster::nlayers(ras_current) > 1){
  
  rasvalue <- raster::extract(ras_current, occ.[ , c(col.lon, col.lat)]) %>%
    na.omit() %>% apply(., MARGIN = 2, sort) %>% as.data.frame()
  
  EData <- ceiling((nrow(occ.) * threshold.) / 100)
  
  EValue <- rasvalue[EData, ]
  
  dir.create(paste0(folder.sp, "/ensembles"), showWarnings = FALSE)
  dir.create(paste0(folder.sp, "/ensembles/current"), showWarnings = F)
  write.csv(EValue, paste0(folder.sp, "/ensembles/current/EValue.csv"), row.names = F)
  
  ## Converting continuos prediction to binary according to bin_threshold
  
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
    
  CV <- function(x, na.rm = TRUE) {
    sd(x, na.rm=na.rm) / mean(x, na.rm=na.rm)
  }
  
  binE_med <- raster::calc(x = binE, median)
  binE_devstd <- raster::calc(x = binE, sd)
  binE_cv <- raster::calc(x = binE, CV)
  binE_sum <- raster::calc(x =binE, sum)/nlayers(binE)
  
  Resensembles <- stack(binE_med, binE_devstd, binE_cv, binE_sum)
  names(Resensembles) <- c(paste0(folder.sp,"_mediana"), paste0(folder.sp,"_devstd"), 
                           paste0(folder.sp,"_CV"), paste0(folder.sp,"_sumNorm"))
    
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

