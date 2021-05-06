
currentEns_byAlg <- function(ras.Stack, data., collon, collat, e, algorithm, foldersp, tim, esc.nm) {
  if (is.null(ras.Stack)) {
    message("any model")
  } else {
    if (nlayers(ras.Stack) == 0) {
      message("any model")
    } else {
      # folder to save ensembles
      dir.create(paste0(foldersp, "/ensembles"), showWarnings = FALSE)
      dir.create(paste0(foldersp, "/ensembles/", tim), showWarnings = F)
      dir.create(paste0(foldersp, "/ensembles/", tim, "/", algorithm), showWarnings = F)
      # branch for algorithms with more than one best model

      if (nlayers(ras.Stack) > 1) {

        # 1. Logistic ensembles by algorithm

        # taking statistics from logistic stack
        
        Ras.med <- raster::calc(x = ras.Stack, median)
        Ras.devstd <- raster::calc(x = ras.Stack, sd)
        Ras.cv <- raster::calc(x = ras.Stack, CV)
        Ras.sum <- raster::calc(x = ras.Stack, sum)

        # stacking results of ensembles
        Resensembles <- stack(Ras.med, Ras.devstd, Ras.cv, Ras.sum)
        names(Resensembles) <- c(
          paste0(foldersp, "_",esc.nm, "_",algorithm), paste0(foldersp, "_devstd_",esc.nm, "_", algorithm),
          paste0(foldersp, "_CV_",esc.nm, "_", algorithm), paste0(foldersp, "_sum",esc.nm,"_", algorithm)
        )

        # writing ensemble
        for (i in 1:raster::nlayers(Resensembles)) {
          writeRaster(Resensembles[[i]], paste0(
            foldersp, "/ensembles/", tim, "/", algorithm, "/",
            names(Resensembles[[i]]), ".tif"
          ),
          format = "GTiff",
          overwrite = T
          )
        }
      } else {
        # pseudo-result of ensemble 1 model (median), to conserve parsimony
        Ras.med <- ras.Stack
        names(Ras.med) <- paste0(foldersp, esc.nm, "_", algorithm)
        # writing pseudo median
        writeRaster(Ras.med, paste0(
          foldersp, "/ensembles/", tim, "/", algorithm, "/",
          names(Ras.med), ".tif"
        ),
        format = "GTiff",
        overwrite = T
        )
      }

      # thresholds

      biomodelos.thresh <- c(e, 1, 10, 20, 30)
      names(biomodelos.thresh) <- c("E", "0", "10", "20", "30")

      # converting to binary the median ensemble for each biomodelos threshold
      Bins <- list()
      for (i in 1:length(biomodelos.thresh)) {
        Binsi <- do.bin(
          Ras = Ras.med, dat = data., lon = collon,
          lat = collat, thresh = biomodelos.thresh[i]
        )
        Bins[[i]] <- Binsi
      }

      # extracting threshold value from list of binaries. Bins file is a list o sub-list. Each sub-list
      # refers to one threshold conversion. Inside each sub-list there are two files, the binary raster
      # and the value of threshold
      BinsDf <- list()
      for (i in 1:length(Bins)) {
        listi <- Bins[[i]]
        Df <- listi[[2]]
        BinsDf[[i]] <- Df
      }
      # convert list to a vector
      BinsDf <- data.frame(BinsDf)
      # giving names to the Df
      names(BinsDf) <- names(biomodelos.thresh)

      write.csv(BinsDf, paste0(
        foldersp, "/ensembles/", tim, "/", algorithm, "/",
        "binValues", esc.nm, "_", algorithm, ".csv"
      ),
      row.names = F
      )

      #
      BinsRas <- raster::stack()
      for (i in 1:length(Bins)) {
        listi <- Bins[[i]]
        Ras <- listi[[1]]
        BinsRas <- raster::stack(BinsRas, Ras)
      }

      names(BinsRas) <- paste0(foldersp, "_", names(biomodelos.thresh), "_", esc.nm, "_", algorithm)

      for (i in 1:nlayers(BinsRas)) {
        writeRaster(BinsRas[[i]], paste0(
          foldersp, "/ensembles/", tim, "/", algorithm, "/",
          names(BinsRas[[i]]), ".tif"
        ),
        format = "GTiff",
        overwrite = T
        )
      }
    }
  }
}

#------------------------
# variation coeficient function
CV <- function(x, na.rm = TRUE) {
  sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}

#------------------------------

do.bin <- function(Ras, dat, lon, lat, thresh) {

  # extracting values for all occurrences from models and organize in a data.frame
  rasvalueT <- raster::extract(Ras, dat[, c(lon, lat)]) %>%
    na.omit() %>%
    sort()

  # percentile data of E
  TData <- ceiling((nrow(dat) * thresh) / 100)

  # value for each model to binarize to E
  TValue <- rasvalueT[TData]
  names(TValue) <- names(thresh)

  ## Converting continuous prediction to binary according to bin_threshold

  binT <- Ras > TValue
  names(binT) <- names(thresh)

  return(list(binT, TValue))
}
#------------------------------------------
futAuxiliar <- function(fut.list.ras){
  listras.lenght <- length(fut.list.ras)
  layers.lenght <- nlayers(fut.list.ras[[1]]) #example of layers lenght
  
  list.layersFEsc <- list()
  nm_vector <- as.numeric() 
  
  for(c in 1:layers.lenght){
    layersbyEscenario <- raster::stack()
    
    for(d in 1:listras.lenght){
      
      stackc <- fut.list.ras[[d]]
      rasc <- stackc[[c]] 
      nms <- unlist(strsplit(names(rasc), "\\."))[3]
      nm_vector[c] <- nms
      layersbyEscenario <- raster::stack(layersbyEscenario, rasc)  
    }
    
    list.layersFEsc[[c]] <- layersbyEscenario
  }
  names(list.layersFEsc) <- nm_vector
  return(list.ras = list.layersFEsc)
}
