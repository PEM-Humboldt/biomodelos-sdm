
currentEns_byAlg <- function(rasM.Stack, rasG.Stack, rasF.Stack, data., collon, collat, e, algorithm, foldersp,
                             tim, esc.nm, crs.proyect, extent.ensembles, transf.biomo.ext,
                             areas = M_, proj.models = proj_models, bins, wr.Bin.Matrix) {
  if (is.null(rasM.Stack)) {
    message("rasM stack is null")
  } else {
    if (terra::nlyr(rasM.Stack) == 0) {
      message("number of layers of rasM stack is zero")
    } else {
      # folder to save ensembles
      dir.create(paste0(foldersp, "/ensembles/", tim, "/", algorithm), showWarnings = F, recursive = T)

      # species name with hyphen not dot
      sp_hyphen <- gsub(foldersp, pattern = "\\.", replacement = "_")

      #
      if (tim == "current") {
        esc.nm <- "_"

        # comparing extent with "biomodelos" extent

        if (transf.biomo.ext == TRUE) {
          biomodelos.ext <- c(-83, -60, -14, 13)

          if (proj.models == "M-M") {
            equ.ext <- equal.extent(a = rasM.Stack, b = biomodelos.ext)
            rasM.Stack <- equalize.extents(equal.1 = equ.ext[[1]], equal.2 = equ.ext[[2]], ras = rasM.Stack,
                                           folder.sp = foldersp, biomodelosext = biomodelos.ext)
          }
          if (proj.models == "M-G") {
            rasM.Stack <- rasM.Stack

            equ.ext <- equal.extent(a = rasG.Stack, b = biomodelos.ext)
            rasG.Stack <- equalize.extents(equal.1 = equ.ext[[1]], equal.2 = equ.ext[[2]], ras = rasG.Stack,
                                           folder.sp = foldersp, biomodelosext = biomodelos.ext)
          }
        }

        if (terra::nlyr(rasM.Stack) > 1) {
          Ras.med <- terra::median(rasM.Stack)
        } else {
          Ras.med <- rasM.Stack
        }

        # thresholds

        biomodelos.thresh <- c(e, 1, 10, 20, 30)
        names(biomodelos.thresh) <- c("E", "0", "10", "20", "30")

        # converting to binary the median ensemble for each biomodelos threshold
        Bins <- list()
        for (i in 1:length(biomodelos.thresh)) {
          Binsi <- do.bin(
            Ras = Ras.med, dat = data., lon = collon,
            lat = collat, thresh = biomodelos.thresh[i],
            wrBinMatrix = wr.Bin.Matrix
          )
          if(wr.Bin.Matrix){
            write.csv(Binsi[[3]], paste0(
              foldersp, "/ensembles/", tim, "/", algorithm, "/",
              "binMatrix", esc.nm, algorithm,"_", biomodelos.thresh, "_.csv"
            ),
            row.names = F
            )
          }
          
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

        if (proj.models == "M-M") {

          # extraer binarios de la lista creada por la función do.bin
          BinsRas <- terra::rast()
          for (i in 1:length(Bins)) {
            listi <- Bins[[i]]
            Ras <- listi[[1]]
            BinsRas <- c(BinsRas, Ras)
          }

          if (nlyr(rasM.Stack) > 1) {
            Ras.devstd <- terra::app(x = rasM.Stack, sd)
            Ras.cv <- Ras.devstd / Ras.med
            Ras.sum <- terra::app(x = rasM.Stack, sum)

            # stacking results of ensembles
            Resensembles <- c(Ras.med, Ras.devstd, Ras.cv, Ras.sum)
            names(Resensembles) <- c(
              paste0(sp_hyphen, esc.nm, algorithm), paste0(sp_hyphen, "_devstd", esc.nm, algorithm),
              paste0(sp_hyphen, "_CV", esc.nm, algorithm), paste0(sp_hyphen, "_sum", esc.nm, algorithm)
            )
            terra::crs(Resensembles) <- crs.proyect
          } else {
            Resensembles <- rasM.Stack
            names(Resensembles) <- c(
              paste0(sp_hyphen, esc.nm, algorithm)
            )
          }
        }

        if (proj.models == "M-G") {
          # Read thresholds
          Ras.med <- median(rasG.Stack)
          # apply thresholds to G layer
          BinsRas <- lapply(X = names(biomodelos.thresh), function(X) {
            Ras.med >= BinsDf[1, X]
          })
          BinsRas <- rast(BinsRas)
          names(BinsRas) <- names(biomodelos.thresh)

          if (nlyr(rasG.Stack) > 1) {
            Ras.devstd <- terra::app(x = rasG.Stack, sd)
            Ras.cv <- Ras.devstd / Ras.med
            Ras.sum <- terra::app(x = rasG.Stack, sum)

            # stacking results of ensembles
            Resensembles <- c(Ras.med, Ras.devstd, Ras.cv, Ras.sum)
            names(Resensembles) <- c(
              paste0(sp_hyphen, esc.nm, algorithm), paste0(sp_hyphen, "_devstd", esc.nm, algorithm),
              paste0(sp_hyphen, "_CV", esc.nm, algorithm), paste0(sp_hyphen, "_sum", esc.nm, algorithm)
            )
            terra::crs(Resensembles) <- crs.proyect
          } else {
            Resensembles <- rasG.Stack
            names(Resensembles) <- c(
              paste0(sp_hyphen, esc.nm, algorithm)
            )
          }
        }
      }
      
      #----------------------
      # Future branch

      if (tim == "future") {
        esc.nm <- paste0("_", esc.nm, "_")

        # comparing extent with "biomodelos" extent

        # if (transf.biomo.ext == TRUE) {
        #   biomodelos.ext <- c(-83, -60, -14, 13)
        # 
        #   equ.ext <- equal.extent(a = rasF.Stack, b = biomodelos.ext)
        #   rasF.Stack <- equalize.extents(equal.1 = equ.ext[[1]], equal.2 = equ.ext[[2]], ras = rasF.Stack,
        #                                  folder.sp = foldersp, biomodelosext = biomodelos.ext)
        # }

        # Read thresholds
        # thresholds

        biomodelos.thresh <- bins
        Ras.med <- median(rasF.Stack)

        # apply thresholds to G layer
        BinsRas <- lapply(X = names(biomodelos.thresh), function(X) {
          Ras.med >= bins[1, X]
        })
        BinsRas <- rast(BinsRas)

        if (nlyr(rasF.Stack) > 1) {
          Ras.devstd <- terra::app(x = rasF.Stack, sd)
          Ras.cv <- Ras.devstd / Ras.med
          Ras.sum <- terra::app(x = rasF.Stack, sum)

          # stacking results of ensembles
          Resensembles <- c(Ras.med, Ras.devstd, Ras.cv, Ras.sum)
          names(Resensembles) <- c(
            paste0(sp_hyphen, esc.nm, algorithm), paste0(sp_hyphen, "_devstd", esc.nm, algorithm),
            paste0(sp_hyphen, "_CV", esc.nm, algorithm), paste0(sp_hyphen, "_sum", esc.nm, algorithm)
          )
          terra::crs(Resensembles) <- crs.proyect
        } else {
          Resensembles <- rasF.Stack
          names(Resensembles) <- c(
            paste0(sp_hyphen, esc.nm, algorithm)
          )
        }
      }

      # writing binaries

      names(BinsRas) <- paste0(sp_hyphen, esc.nm, names(biomodelos.thresh), "_", algorithm)

      for (i in 1:nlyr(BinsRas)) {
        BinsRas.i <- BinsRas[[i]]
        terra::crs(BinsRas.i) <- crs.proyect
        terra::writeRaster(BinsRas.i, paste0(
          foldersp, "/ensembles/", tim, "/", algorithm, "/",
          names(BinsRas[[i]]), ".tif"
        ),
        filetype = "GTiff",
        overwrite = T,
        datatype = "INT2S",
        NAflag = -9999,
        gdal = c("COMPRESS = LZW")
        )
      }

      # writing continuos

      for (i in 1:terra::nlyr(Resensembles)) {
        RasResen.i <- Resensembles[[i]]
        terra::crs(RasResen.i) <- crs.proyect
        writeRaster(RasResen.i, paste0(
          foldersp, "/ensembles/", tim, "/", algorithm, "/",
          names(Resensembles[[i]]), ".tif"
        ),
        filetype = "GTiff",
        overwrite = T,
        NAflag = -9999,
        datatype = "FLT4S",
        gdal = c("COMPRESS = LZW")
        )
      }
    }
  }
  if(exists("BinsDf")){
    return(BinsDf = BinsDf)  
  }else{
    return(BinsDf = NULL)
  }
  
}

#------------------------
# variation coeficient function
CV <- function(x, na.rm = TRUE) {
  x1 <- sd(x, na.rm = na.rm) / median(x, na.rm = na.rm)
  x2 <- x1 * 100
}

#------------------------------

do.bin <- function(Ras, dat, lon, lat, thresh, wrBinMatrix) {

  # extracting values for all occurrences from models and organize in a data.frame
  rasvalueT <- terra::extract(Ras, dat[, c(lon, lat)])
  rasvalueT <- rasvalueT[, 2] %>%
    na.omit() %>%
    unlist() %>%
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
  
  if(wrBinMatrix){
    DatvalueT <- terra::extract(binT, dat[, c(lon, lat)])
  }
  
  return(list(binT, TValue, DatvalueT))
}

#------------------------------------------

futAuxiliar <- function(fut.list.ras) {
  listras.lenght <- length(fut.list.ras)
  layers.lenght <- nlyr(fut.list.ras[[1]]) # example of layers length

  list.layersFEsc <- list()
  nm_vector <- as.numeric()

  for (c in 1:layers.lenght) {
    layersbyEscenario <- terra::rast()

    for (d in 1:listras.lenght) {
      stackc <- fut.list.ras[[d]]
      rasc <- stackc[[c]]
      nms <- unlist(regmatches(x = names(rasc), regexpr("_", names(rasc)), invert = TRUE))[2]
      nms <- gsub(".asc", "", nms)
      nm_vector[c] <- nms
      layersbyEscenario <- c(layersbyEscenario, rasc)
    }

    list.layersFEsc[[c]] <- layersbyEscenario
  }
  names(list.layersFEsc) <- nm_vector
  return(list.ras = list.layersFEsc)
}

#-----------------------------------------
# auxiliary compare extent

equal.extent <- function(a, b) { # a must be  a raster and b a vector extent

  # to experiment
  # a <- biomodelos.ext <- c(-83, -60, -14, 13)
  # b <- biomodelos.ext <- c(-83, -60, -14, 13)

  # extents
  ext.a <- a %>%
    terra::ext() %>%
    terra::as.polygons() %>%
    sf::st_as_sf() %>%
    mutate(value = 1)
  ext.b <- b %>%
    terra::ext() %>%
    terra::as.polygons() %>%
    sf::st_as_sf() %>%
    mutate(value = 1)

  are_same <- (ext.b == ext.a)[1]

  if (are_same) {
    result <- T
    diff.direction <- NA
  } else {

    # differences between extents
    result <- FALSE

    # directions
    # what if b is bigger than a
    diff.ext <- st_difference(ext.b, ext.a) %>% st_area()
    diff.direction <- "b>a"

    if (length(diff.ext) == 0) {
      diff.ext <- st_difference(ext.a, ext.b) %>% st_area()
      diff.direction <- "a>b"
    }
  }
  return(list(result, diff.direction))
}

#-----------------------------------------
#

equalize.extents <- function(equal.1, equal.2, ras, folder.sp, biomodelosext = biomodelos.ext) {
  if (equal.1 == TRUE) {
    ras.tmp <- ras
    ext(ras.tmp) <- ext(biomodelosext)
  } else {
    dir.create(paste0(folder.sp, "/Temp/extent.transf"), showWarnings = F)

    if (equal.2 == "b>a") {
      tmp1 <- ras
      ext1 <- terra::ext(biomodelosext)
      ras.tmp <- terra::extend(tmp1, ext1)
    } else if (equal.2 == "a>b") {
      tmp1 <- ras %>% terra::rast()
      ext1 <- terra::ext(biomodelosext)
      ras.tmp <- terra::crop(tmp1, ext1)
    }
    terra::ext(ras.tmp) <- terra::ext(biomodelosext)
  }

  return(ras.tmp)
}

