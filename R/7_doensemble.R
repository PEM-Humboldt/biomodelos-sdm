do.ensemble <- function(reslist, do.future,
                        occ., threshold., col.lon, col.lat, folder.sp,
                        crs.proyect) {

  length(reslist)
  
  # ensemble from small samples maxent modeling if exist
  if (!is.null(respathA)) {
    pathA_current <- raster("Coendou.vestitus/final_models_sdmtune/current/model_fc_q_reg_0.5_bias.tif")
    raster::crs(pathA_current) <- sp::CRS(crs.proyect)
    ensA <- currentEns_byAlg(
      ras.Stack = pathA_current, data. = occ., collon = col.lon, collat = col.lat,
      e = 10, algorithm = "mxnt", foldersp = folder.sp # MISSING let user choice e
    ) ########################
  }

  # ensemble from large samples maxent modeling  if exist
  if (!is.null(respathB1)) {
    pathB1_current <- respathB1$c_proj
    raster::crs(pathB1_current) <- sp::CRS(crs.proyect)
    ensB1 <- currentEns_byAlg(
      ras.Stack = pathB1_current, data. = occ., collon = col.lon, collat = col.lat,
      e = 5, algorithm = "mxnt", foldersp = folder.sp # MISSING let user choice e
    )
  }

  # ensemble from large samples other algorithms modeling  if exist
  if (!is.null(respathB2)) {
    pathB2_current <- respathB2$c_proj
    raster::crs(pathB2_current) <- sp::CRS(crs.proyect)

    # MISSING let the user specify the algorithms used in biomod, it could be solve using a
    # flag name of maxent (mxnt) in the raster names (in kuenm or enmeval) to change the ensemble algorithm to
    # use the name to know what algorith was used
    # Temporal solution:

    pathB2GBM <- pathB2_current[[grep(pattern = "GBM", names(pathB2_current))]]
    ensB2GBM <- currentEns_byAlg(
      ras.Stack = pathB2GBM, data. = occ., collon = col.lon, collat = col.lat,
      e = 5, algorithm = "GBM", foldersp = folder.sp ############ MISSING let user choice e
    )

    pathB2ANN <- pathB2_current[[grep(pattern = "ANN", names(pathB2_current))]]
    ensB2ANN <- currentEns_byAlg(
      ras.Stack = pathB2ANN, data. = occ., collon = col.lon, collat = col.lat,
      e = 5, algorithm = "ANN", foldersp = folder.sp ############ MISSING let user choice e
    )
  }

  # if(do.future == T){
  #
  # }
}

#--------------------------

currentEns_byAlg <- function(ras.Stack, data., collon, collat, e, algorithm, foldersp) {
  if (is.null(ras.Stack)) {
    message("any model")
  } else {
    if (nlayers(ras.Stack) == 0) {
      message("any model")
    } else {
      # folder to save ensembles
      dir.create(paste0(foldersp, "/ensembles"), showWarnings = FALSE)
      dir.create(paste0(foldersp, "/ensembles/current"), showWarnings = F)
      dir.create(paste0(foldersp, "/ensembles/current", "/", algorithm), showWarnings = F)
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
          paste0(foldersp, algorithm, "_med"), paste0(foldersp, algorithm, "_devstd"),
          paste0(foldersp, algorithm, "_CV"), paste0(foldersp, algorithm, "_sum")
        )

        # writing ensemble
        for (i in 1:raster::nlayers(Resensembles)) {
          writeRaster(Resensembles[[i]], paste0(
            foldersp, "/ensembles/current", "/", algorithm, "/",
            names(Resensembles[[i]]), ".tif"
          ),
          format = "GTiff",
          overwrite = T
          )
        }
      } else {
        # pseudo-result of ensemble 1 model (median), to conserve parsimony
        Ras.med <- ras.Stack
        names(Ras.med) <- paste0(foldersp, algorithm, "_med")
        # writing pseudo median
        writeRaster(Ras.med, paste0(
          foldersp, "/ensembles/current", "/", algorithm, "/",
          names(Ras.med), ".tif"
        ),
        format = "GTiff",
        overwrite = T
        )
      }

      # thresholds

      biomodelos.thresh <- c(e, 1, 10, 20, 30)
      names(biomodelos.thresh) <- c("E", "MTP", "TTP", "20TP", "30TP")

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
        foldersp, "/ensembles/current/", algorithm, "/",
        "binValues.csv"
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

      names(BinsRas) <- paste0(foldersp, algorithm, "_Bin", names(biomodelos.thresh))

      for (i in 1:nlayers(BinsRas)) {
        writeRaster(BinsRas[[i]], paste0(
          foldersp, "/ensembles/current/", algorithm, "/",
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
