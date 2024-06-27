#' auto_metadata()
#'
#' This function automates the generation of metadata for species distribution models (SDMs) by filling a 
#' template with relevant information from model outputs.
#'
#' dirmodels Character. Path to the directory containing model output folders.
#' dirtowrite Character. Path to the directory where metadata and other outputs will be written.
#'
#' meta_template, Data frame: Template for metadata that will be filled with information.
#' algos, Character vector: Algorithms used for modeling (e.g., "MAXENT", "bioclim").
#' fut_proj, Logical: Whether future projections are included (default is TRUE).
#' Date, Character string: format "yyyy-mm-dd" indicating when the models were generated.
#' transf_ext, Logical: Whether to transform extents (default is FALSE).
#' ext_template, Raster: Template for the extent transformation (default is NULL).
#' crs_project, CRS: Coordinate reference system for projection (default is NULL).
#' authors, Character vector: Authors of the models (default is NULL).
#'
#' return Data frame containing the filled metadata template for each species.
#' 
#' examples
#' auto_metadata("path/to/models", "path/to/write", template, c("MAXENT", "bioclim"), TRUE, "2023-06-26")

auto_metadata <- function(dirmodels, dirtowrite, meta_template, algos, fut_proj = T, 
                          dates, transf_ext = FALSE, ext_template = NULL, 
                          crs_project = NULL, authors = NULL) {
  
  # each directory must represent a species, so, how many species do we have?
  sps <- list.dirs(paste0(dirmodels, "/"), full.names = F, recursive = F)
  
  # list to save template filled by tif mentioned above
  info <- list()
  
  for (i in 1:length(sps)) {
    #i <- 4
    message(sps[i])
    
    dirs_inside <- list.dirs(paste0(dirmodels, "/", sps[i]), full.names = T, recursive = F)
    
    ensembles_dir <- dirs_inside[grep("ensembles", dirs_inside)]
    
    if (length(ensembles_dir) != 0) {
      
      for(a in 1:length(algos)){
        
        ensembles_inside <- list.dirs(ensembles_dir)
        
        find_alg1 <- ensembles_inside %>% grep(pattern = algos[a], x = .) 
        
        if(length(find_alg1) != 0){
          
          alg1 <- algos[a]
          path_alg <- ensembles_inside[find_alg1]
          break()
        }
      }
      
      pathforsp <- paste0(path_alg, "/")  
      
      path_tifs <- list.files(path = pathforsp, pattern = "*.tif$", full.names = T, 
                              recursive = F)
      path_tifs_short <- list.files(path = pathforsp, pattern = "*.tif$", full.names = F, 
                                    recursive = F)
      
      if (length(path_tifs) !=0) {
        
        # find records and count
        occ.forsp <- paste0(dirmodels, "/", sps[i], "/occurrences/jointID_occ.csv")
        rec_data <- read.csv(occ.forsp)
        records <- nrow(rec_data)
        
        # writing records data in dir.write folder
        write.csv(rec_data, paste0(dirtowrite, "/", sps[i], "_records.csv"), row.names = F)
        
        if(alg1 == "generalizacion"){
          
          # copy tif
          file.copy(
            from = path_tifs,
            to = paste0(dirtowrite, "/"),
            overwrite = T, recursive = T
          )
          records_model <- records
          thresholdValue <- NA
          thresholdType <- NA
          validationType <- NA
          perfStatType <- NA
          pvalue <- NA
          best1 <- NA
          bes2 <- NA
          consensus <- NA
          or_organize <- NA
          n <- 1
          
        }else{
          # Selecting strings of binaries 0,10,20,30 and continuos
          continuos <- sps[i] %>% gsub(x = ., pattern = "[.]", replacement = "_") %>% 
            paste0("*",., "_", alg1,".tif$") %>% grep(pattern = ., path_tifs)
          
          strings <- path_tifs[c(1, 2, 3, 4, continuos)]
          strings_short <- path_tifs_short[c(1, 2, 3, 4, continuos)]
          
          
          # binaries: 0, 10p, 20p, 30p
          # find thrsholds and get names
          bin.forsp <- paste0(dirmodels, "/", sps[i], "/ensembles/current/", alg1, "/binValues_", alg1, ".csv")
          bins <- read.csv(bin.forsp)
          records_model <- rep(records, each = 5)
          thresholdValue <- bins[1, -1]
          thresholdValue$cont <- NA
          thresholdType <- c("0", "10", "20", "30", "Continuous")
          
          if(alg1 == "MAXENT"){
            # performance evaluation
            if (records <= 25) {
              validationType <- "Jackknife"
              perfStatType <- "AUC_test"
              indexbest <- grep(pattern = "best_models.csv", list.files(paste0(dirmodels, "/", sps[i]), recursive = T))
              best <- read.csv(list.files(paste0(dirmodels, "/", sps[i]), recursive = T, full.names = T)[indexbest])
              
              pvalue <- NA
              best1 <- median(best$auc.train)
              bes2 <- sd(best$auc.train)
            } else {
              validationType <- "Crossvalidate-Block"
              perfStatType <- "pROC"
              dirsAll <- list.files(paste0(dirmodels, "/", sps[i]), recursive = T)
              indexbest <- grep(pattern = "best_", dirsAll)[1]
              best <- read.csv(list.files(paste0(dirmodels, "/", sps[i]), recursive = T, full.names = T)[indexbest])
              
              indexpckg <- grep(pattern = "enmeval", dirsAll)
              
              if(length(indexpckg) != 0){
                #enmeval
                best1 <- median(best$proc_auc_ratio.avg)
                bes2 <- sd(best$proc_auc_ratio.avg)
                pvalue <- mean(best$proc_pval.avg)
              }else{
                #kuenm
                best1 <- median(best$Mean_AUC_ratio)
                bes2 <- sd(best$Mean_AUC_ratio)
                pvalue <- mean(best$pval_pROC)
              }
              
            }
          }
          
          
          if(alg1 == "bioclim"){
            validationType <- "Crossvalidate-Block"
            perfStatType <- "AUC_train"
            indexbest <- grep(pattern = "best_models", list.files(paste0(dirmodels, "/", sps[i]), recursive = T))
            best <- read.csv(list.files(paste0(dirmodels, "/", sps[i]), recursive = T, full.names = T)[indexbest])
            best1 <- median(best$auc.mean)
            bes2 <- sd(best$auc.mean)
            pvalue <- mean(best$pbinomial)
          }
          
          ras <- raster::stack(strings)
          names(ras) <- gsub(pattern = ".tif", replacement = "", strings_short)
          
          # omission
          rec_data <- rec_data[, c(3:4)]
          
          data_ras <- raster::extract(ras[[grep(pattern = "_0_|_10_|_20_|_30_", x = names(ras))]], rec_data)
          or <- 1 - (colSums(data_ras, na.rm = T) / records_model[1])
          or_organize <- c(or[1], or[2], or[3], or[4], NA)
          
          n <- length(thresholdType)
          
          if (transf_ext) {
            rm(ras2, bras, cras)
            ras2 <- terra::rast(ras)
            biomodelos.ext <- c(-83, -60, -14, 13)
            equ.ext <- equal.extent(a = ras2, b = biomodelos.ext)
            if(equ.ext[1] == FALSE){
              ras2 <- ras2 %>% terra::crop(biomodelos_template) %>% extend(biomodelos_template)
            }
            
            bras <- ras2[[1:4]]
            for (i in 1:nlyr(bras)) {
              #i <- 1
              bras.i <- bras[[i]]
              terra::crs(bras.i) <- terra::crs(ext_template)
              terra::writeRaster(bras.i, paste0(
                dirtowrite, "/",
                names(bras[[i]]), ".tif"
              ),
              filetype = "GTiff",
              overwrite = T,
              datatype = "INT2S",
              NAflag = -9999,
              gdal = c("COMPRESS = LZW")
              )
            }
            
            cras <- ras2[[5]]
            for (i in 1:terra::nlyr(cras)) {
              cras.i <- cras[[i]]
              terra::crs(cras.i) <- terra::crs(ext_template)
              writeRaster(cras.i, paste0(
                dirtowrite, "/",
                names(cras[[i]]), ".tif"
              ),
              filetype = "GTiff",
              overwrite = T,
              NAflag = -9999,
              datatype = "FLT4S",
              gdal = c("COMPRESS = LZW")
              )
            }
          } else {
            for (a in 1:length(strings)) {
              file.copy(
                from = strings[a],
                to = paste0(dirtowrite, "/"),
                overwrite = T, recursive = T
              )
            }
          }
          
          
          if(is.na(bes2)){
            consensus <- NA
          }else{
            consensus <- "Median"
          }
          
          
          
        }# maxent and bioclim and other algortithms
        
        # dates
        
        date_split <- strsplit(dates, "-") %>% unlist()
        
        # filling meta data template
        sp <- sps[i] %>% gsub(x = ., pattern = "[.]", replacement = " ")
        
        meta_template[1:n, "acceptedNameUsage"] <- sp
        meta_template[1:n, "modelingMethod"] <- alg1
        meta_template[1:n, "thresholdType"] <- thresholdType
        meta_template[1:n, "validationType"] <- validationType
        meta_template[1:n, "perfStatType"] <- perfStatType
        meta_template[1:n, "perfStatValue"] <- best1
        meta_template[1:n, "perfStatSD"] <- bes2
        meta_template[1:n, "pValue"] <- pvalue
        meta_template[1:n, "consensusMethod"] <- consensus
        meta_template[1:n, "thresholdValue"] <- thresholdValue
        meta_template[1:n, "omission"] <- or_organize
        meta_template[1:n, "recsUsed"] <- records_model
        meta_template[1:n, "modelLevel"] <- 1
        meta_template[1:n, "isActive"] <- "true"
        meta_template[1:n, "modelStatus"] <- "Developing"
        meta_template[1:n, "yyyy"] <- date_split[1]
        meta_template[1:n, "mm"] <- date_split[2]
        meta_template[1:n, "dd"] <- date_split[3]
        meta_template[1:n, "published"] <- "false"
        meta_template[1:n, "license"] <- "by-nc-sa"
        meta_template[1:n, "modelSeason"] <- "resident"
        meta_template[1:n, "modelOrigin"] <- "native"
        meta_template[1:n, "modelGeoExtent"] <- "national"
        meta_template[1:n, "modelEpoch"] <- "present" #MISSING COMPLETELY MISSED FUTURE METADATA
        
        #PNG, ZIP, THUMB
        if(thresholdType != "Continuous"){
          th_nm <- thresholdType
        }else{
          th_nm <- ""
        }
        
        base_nm <- paste0(sp, "_", th_nm, "_", alg1)
        
        meta_template[1:n, "thumb"] <- paste0(base_nm, "_thumb.png")
        meta_template[1:n, "zip"] <- paste0(base_nm, ".zip")
        meta_template[1:n, "png"] <- paste0(base_nm, ".png")
        
        if(is.null(authors)){
          meta_template[1:n, "modelAuthors"] <- "Instituto Humboldt"
        }else{
          meta_template[1:n, "modelAuthors"] <- authors
        }
        
        
        
        info[[i]] <- meta_template
      }
    }
  }
  info <- info[lengths(info) != 0]
  infoall <- do.call(rbind.data.frame, info)
}

#----------------------------------------------
#' Compare Extents Between Raster and Vector
#'
#' This function compares the extent of a raster object with a vector extent to determine if they are the same.
#'
#' a, Raster: Raster object whose extent is to be compared.
#' b, Numeric vector: Vector representing the extent to compare against.
#' 
#' return List containing the result of extent comparison (logical) and the direction of difference (character).
#'
#' examples
#' equal.extent(raster_object, c(-83, -60, -14, 13))



equal.extent <- function(a, b) { # a must be  a raster and b a vector extent
  
  # to debug
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
#' Equalize Extents of Raster Data
#'
#' This function adjusts the extent of a raster based on comparison results from `equal.extent()` function 
#' and a target extent (\code{biomodelosext}).
#'
#' equal.1, Logical: Indicates if extents are already equal.
#' equal.2, Character: Direction of extent adjustment ("b>a" or "a>b").
#' ras, Raster: Raster object to adjust extents for.
#' folder.sp, Character: Folder path for saving temporary files.
#' biomodelosext, Numeric vector: Target extent to adjust the raster to.
#' 
#' return Adjusted raster object with modified extent.
#' 
#' examples
#' equalize.extents(TFALSE, "b>a", ras_object, "path/to/folder", c(-83, -60, -14, 13))

equalize.extents <- function(equal.1, equal.2, ras, folder.sp, biomodelosext = biomodelos.ext) {
  if (equal.1 == TRUE) {
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
