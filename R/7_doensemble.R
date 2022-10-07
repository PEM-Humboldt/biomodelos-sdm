
currentEns_byAlg <- function(ras.Stack, data., collon, collat, e, algorithm, foldersp,
                             tim, esc.nm, crs.proyect, extent.ensembles, transf.biomo.ext,
                             areas = M_, proj.models = proj_models, compute.F = compute_F) {
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

      # species name with hyphen not dot
      sp_hyphen <- gsub(foldersp, pattern = "\\.", replacement = "_")

      #
      if (tim == "current") esc.nm <- "_"
      if (tim == "future")  esc.nm <- paste0("_", esc.nm, "_")
      

      # comparing extent with "biomodelos" extent}

      if (transf.biomo.ext == TRUE) {
        
        if (compute.F == FALSE){
          if (proj.models == "M-M"){
            ras.Stack <- raster::mask(ras.Stack, areas$shape_M)
          }
          if (proj.models == "M_G"){
            ras.Stack <- raster::mask(ras.Stack, areas$shape_G)
          }
        }
        
        biomodelos.ext <- c(-83, -60, -14, 13)
        equ.ext <- equal.extent(a = ras.Stack, b = biomodelos.ext, limit = 0.005)

        if (equ.ext == TRUE) {
          extent(ras.Stack) <- extent(biomodelos.ext)
        } else {
          dir.create(paste0(foldersp, "/Temp/extent.transf"), showWarnings = F)
          for (i in 1:nlayers(ras.Stack)) {
            writeRaster(ras.Stack[[i]], paste0(foldersp, "/Temp/extent.transf/", names(ras.Stack[[i]]), ".tif"), overwrite = TRUE)
          }
          ras.list <- list.files(paste0(foldersp, "/Temp/extent.transf/"), pattern = ".tif$", full.names = T)
          if (length(ras.list) == 1) {
            ras.Stack <- raster::raster(ras.list)
          } else {
            ras.Stack <- raster::stack(ras.list)
          }
          ras.Stack <- extend(ras.Stack, biomodelos.ext)
          extent(ras.Stack) <- extent(biomodelos.ext)

          for (i in 1:length(ras.list)) {
            unlink(ras.list[i], recursive = T, force = T)
          }
        }
      }


      # branch for algorithms with more than one best model
      if (nlayers(ras.Stack) > 1) {

        # 1. Logistic ensembles by algorithm

        # taking statistics from logistic stack

        Ras.med <- raster::calc(x = ras.Stack, median)

        if (tim == "current") {
          Ras.devstd <- raster::calc(x = ras.Stack, sd)
          Ras.cv <- raster::calc(x = ras.Stack, CV)
          Ras.sum <- raster::calc(x = ras.Stack, sum)

          # stacking results of ensembles
          Resensembles <- stack(Ras.med, Ras.devstd, Ras.cv, Ras.sum)
          names(Resensembles) <- c(
            paste0(sp_hyphen, esc.nm, algorithm), paste0(sp_hyphen, "_devstd", esc.nm, algorithm),
            paste0(sp_hyphen, "_CV", esc.nm, algorithm), paste0(sp_hyphen, "_sum", esc.nm, algorithm)
          )
        } else {
          Resensembles <- Ras.med
          names(Resensembles) <- c(
            paste0(sp_hyphen, esc.nm, algorithm)
          )
        }

        # writing ensemble
        for (i in 1:raster::nlayers(Resensembles)) {
          RasResen.i <- Resensembles[[i]]
          raster::crs(RasResen.i) <- sp::CRS(crs.proyect)
          writeRaster(RasResen.i, paste0(
            foldersp, "/ensembles/", tim, "/", algorithm, "/",
            names(Resensembles[[i]]), ".tif"
          ),
          format = "GTiff",
          overwrite = T,
          NAflag = -9999,
          datatype = "FLT4S",
          options = "COMPRESS=LZW"
          )
        }
      } else {
        # pseudo-result of ensemble 1 model (median), to conserve parsimony
        Ras.med <- ras.Stack
        names(Ras.med) <- paste0(sp_hyphen, esc.nm, algorithm)
        raster::crs(Ras.med) <- sp::CRS(crs.proyect)
        lyer.01 <- rasterToPoints(Ras.med)
        # writing pseudo median
        writeRaster(Ras.med, paste0(
          foldersp, "/ensembles/", tim, "/", algorithm, "/",
          names(Ras.med), ".tif"
        ),
        format = "GTiff",
        overwrite = T,
        NAflag = -9999,
        datatype = "FLT4S",
        options = "COMPRESS=LZW"
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
        "binValues", esc.nm, algorithm, ".csv"
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

      names(BinsRas) <- paste0(sp_hyphen, esc.nm, names(biomodelos.thresh), "_", algorithm)

      for (i in 1:nlayers(BinsRas)) {
        BinsRas.i <- BinsRas[[i]]
        raster::crs(BinsRas.i) <- sp::CRS(crs.proyect)
        writeRaster(BinsRas.i, paste0(
          foldersp, "/ensembles/", tim, "/", algorithm, "/",
          names(BinsRas[[i]]), ".tif"
        ),
        format = "GTiff",
        overwrite = T,
        datatype = "INT2S",
        NAflag = -9999,
        options = "COMPRESS=LZW"
        )
      }
    }
  }
}

#------------------------
# variation coeficient function6
CV <- function(x, na.rm = TRUE) {
  x1 <- sd(x, na.rm = na.rm) / median(x, na.rm = na.rm)
  x2 <- x1*100
}

#------------------------------

do.bin <- function(Ras, dat, lon, lat, thresh) {

  # extracting values for all occurrences from models and organize in a data.frame
  rasvalueT <- raster::extract(Ras, dat[, c(lon, lat)]) %>%
    na.omit() %>%
    sort()

  # percentile data of E
  Data <- nrow(dat) * thresh / 100
  if (Data < 1) TData <- ceiling(Data)
  if (Data >= 1) TData <- round(Data, 0)
    
  # value for each model to binarize to E
  TValue <- rasvalueT[TData]
  names(TValue) <- names(thresh)

  ## Converting continuous prediction to binary according to bin_threshold

  binT <- Ras >= TValue
  names(binT) <- names(thresh)

  return(list(binT, TValue))
}

#------------------------------------------

futAuxiliar <- function(fut.list.ras) {
  listras.lenght <- length(fut.list.ras)
  layers.lenght <- nlayers(fut.list.ras[[1]]) # example of layers length

  list.layersFEsc <- list()
  nm_vector <- as.numeric()

  for (c in 1:layers.lenght) {
    layersbyEscenario <- raster::stack()

    for (d in 1:listras.lenght) {
      stackc <- fut.list.ras[[d]]
      rasc <- stackc[[c]]
      nms <- unlist(regmatches(x = names(rasc), regexpr("_", names(rasc)), invert = TRUE))[2]
      nm_vector[c] <- nms
      layersbyEscenario <- raster::stack(layersbyEscenario, rasc)
    }

    list.layersFEsc[[c]] <- layersbyEscenario
  }
  names(list.layersFEsc) <- nm_vector
  return(list.ras = list.layersFEsc)
}

#-----------------------------------------
# auxiliary compare extent

equal.extent <- function(a, b, limit) { # a must be  a raster and b a vector extent

  # extents
  ext.a <- extent(a)
  ext.b <- extent(b)

  # differences between extents
  diff.ext <- matrix(ext.a) - matrix(ext.b)
  diff.abs <- abs(diff.ext)
  # limit to decide if the extents are different, 0.005 grades
  diff.limit <- diff.abs > limit

  # how many rows are upper of extent limit, if there are more than one use extend

  if (length(which(diff.limit == T)) != 0) {
    result <- FALSE
  } else {
    result <- TRUE
  }
  return(result)
}
