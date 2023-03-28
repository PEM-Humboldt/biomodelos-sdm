#' Processing environmental variables for ecological niche modelling
#' @description  This function reads in environmental data files from the specified directories, and merges 
#' them into a single raster stack. It then crops and masks the raster stack to the specified shapefiles, 
#' and writes the resulting environmental layers to the specified output directory. The function can also 
#' perform correlation analysis on the processed data if desired.
#'
#' @param clim.dataset character string specifying the name of the dataset to be processed
#' @param clim.dir character string specifying the directory containing climate data
#' @param file.extension character string specifying the file extension of the data files to be processed
#' @param crs.proyect optional character string specifying the coordinate reference system to be used for projection 
#' (default is 'crs_proyect')
#' @param shape.M shapefile or sf object specifying the accessible area for training models
#' @param shape.G shapefile or sf object specifying the area for model projection
#' @param shape.F shapefile or sf object specifying the area for future climate change scenarios
#' @param env.other optional character string specifying the directory containing other environmental data
#' @param folder.sp character string specifying the output directory for the processed environmental data
#' @param do.future logical value indicating whether to process data for future scenarios
#' @param proj.models character string specifying the type of projection models to be used (either "M-G" or "M-F")
#' @param compute.G logical value indicating whether to compute data for the "G" projection area
#' @param compute.F logical value indicating whether to compute data for the "F" future area
#' @param dir.G character string specifying the directory containing data for the "G" projection area
#' @param dir.F character string specifying the directory containing data for the "F" future area
#' @param cor.eval logical value indicating whether to perform correlation analysis on on the environmental variables
#' @param cor.method character string specifying the method to be used for correlation analysis (e.g. "VIF")
#' @param cor.detail list specifying additional details for correlation analysis
#' 
#' @return this function does not return any value, it simply writes the processed environmental data to the 
#' specified output directory.
#' 
#' @note The function assumes that the required libraries and functions (e.g. raster, sf, terra) have already 
#' been loaded into the R environment.

# environmental variables

process_env_current <- function(clim.dataset, clim.dir, file.extension, crs.proyect = crs_proyect,
                                shape.M = M_$shape_M, shape.G = M_$shape_G, shape.F = M_$shape_F,
                                env.other = dir_other, folder.sp = folder_sp, do.future = do_future,
                                proj.models = proj_models, compute.G, compute.F, dir.G, dir.F,
                                cor.eval, cor.method, cor.detail) {

  # climatic folder paths

  clim_files <- list.files(
    path = paste0(clim.dir, clim.dataset, "/current/"),
    pattern = file.extension,
    recursive = F,
    full.names = T
  )

  # other variables apart from the climatic ones
  other_files <- list.files(
    path = paste0(env.other, "/current/"),
    pattern = file.extension,
    recursive = F,
    full.names = T
  )

  # Merging the other variables with climatic
  if (!identical(other_files, character(0))) {
    envfiles <- c(clim_files, other_files)
  } else {
    envfiles <- clim_files
  }

  envras <- raster::stack(envfiles)

  # Colinearity evaluation of variables inside each accesible area  
  if (isTRUE(cor.eval)) {
    if ("VIF" %in% cor.method) {
      envras2 <- terra::rast(envras)
      cor.result <- vif_apply(shapeM = shape.M, envars = envras2, vifdetails = cor.detail)
      rm(envras2)
      indexCol <- which((names(envras) %in% cor.result) == TRUE)
      envras <- envras[[indexCol]]
    }
  }

  envMstack <- envras %>%
    raster::crop(shape.M) %>%
    raster::mask(shape.M)

  if (proj.models == "M-G" & compute.G == TRUE) {
    # cut to G
    envGstack <- envras %>%
      raster::crop(shape.G) %>%
      raster::mask(shape.G)
  }

  #---------------------
  # writing environmental layers

  # area M

  # set M folder

  dir.create(paste0(folder.sp, "/M_variables"), showWarnings = F)
  dir.create(paste0(folder.sp, "/M_variables/Set_1"), showWarnings = F)

  # write M area, Maxent needs ".asc" files

  for (i in 1:nlayers(envMstack)) {
    envMi <- envMstack[[i]]

    value <- na.omit(values(envMi)) %>% max()
    y <- ndec(x = value)

    if (y == 0) {
      datTyp <- "INT2S"
      decinum <- 0
    }
    if (y >= 1) {
      datTyp <- "FLT4S"
      decinum <- 3
    }

    raster::writeRaster(
      x = round(envMi, digits = decinum),
      filename = paste0(
        folder.sp, "/M_variables/Set_1/", names(envMstack[[i]]), ".asc"
      ),
      overwrite = T,
      NAflag = -9999,
      datatype = datTyp
    )
  }

  if (proj.models == "M-G") {

    # area G

    # set G folder

    dir.create(paste0(folder.sp, "/G_variables"), showWarnings = F)
    dir.create(paste0(folder.sp, "/G_variables/Set_1"), showWarnings = F)
    dir.create(paste0(folder.sp, "/G_variables/Set_1/M"), showWarnings = F)
    dir.create(paste0(folder.sp, "/G_variables/Set_1/G"), showWarnings = F)


    if (compute.G == TRUE) {

      # write G area if it was computed

      for (i in 1:nlayers(envGstack)) {
        envGi <- envGstack[[i]]

        value <- na.omit(values(envGi)) %>% max()
        y <- ndec(x = value)

        if (y == 0) {
          datTyp <- "INT2S"
          decinum <- 0
        }
        if (y >= 1) {
          datTyp <- "FLT4S"
          decinum <- 1
        }

        raster::writeRaster(
          x = round(envGi, digits = decinum),
          filename = paste0(
            folder.sp, "/G_variables/Set_1/G/", names(envGstack[[i]]), ".asc"
          ),
          overwrite = T,
          NAflag = -9999,
          datatype = datTyp
        )
      }
    } else {

      # Because was not computed G vars, is necessary to copy them from directory used
      Gfiles <- list.files(dir.G, pattern = ".asc", full.names = T, recursive = F)

      for (a in 1:length(Gfiles)) {
        file.copy(
          from = Gfiles[a],
          to = paste0(folder.sp, "/G_variables/Set_1/G/"),
          overwrite = T, recursive = T
        )
      }
    }

    # copying M files into the G folder, in order to diminish time of computing when we dont want/have to compute G layers

    Mfiles <- list.files(paste0(folder.sp, "/M_variables/Set_1/"), pattern = ".asc", full.names = T, recursive = F)
    for (a in 1:length(Mfiles)) {
      file.copy(
        from = Mfiles[a],
        to = paste0(folder.sp, "/G_variables/Set_1/M/"),
        overwrite = T, recursive = T
      )
    }
  }

  # future

  if (do.future == TRUE) {
    envFstack <- process_env_future(
      climdataset = clim.dataset,
      climdir = clim.dir,
      otherfiles = other_files,
      extension = file.extension,
      crsproyect = crs.proyect,
      projMod = proj.models,
      envother = env.other,
      foldersp = folder.sp,
      names.ras = names(envMstack),
      NclimCurrent = length(clim_files),
      computeF = compute.F,
      dirF = dir.F,
      shapeM = shape.M,
      shapeG = shape.G,
      shapeF = shape.F
    )

    # results do.future == TRUE

    if (proj.models == "M-M" | compute.G == FALSE) {
      return(list(M = envMstack, G = NULL, Future = envFstack, layernames = names(envMstack)))
    } else {
      return(list(M = envMstack, G = envGstack, Future = envFstack, layernames = names(envMstack)))
    }
  }

  # results do.future == FALSE

  if (proj.models == "M-M" | compute.G == FALSE) {
    return(list(M = envMstack, G = NULL, Future = NULL, layernames = names(envMstack)))
  } else {
    return(list(M = envMstack, G = envGstack, Future = NULL, layernames = names(envMstack)))
  }
}

#-------------------------
process_env_future <- function(climdataset, climdir, otherfiles, extension, crsproyect,
                               envother, foldersp, projMod, names.ras, NclimCurrent,
                               computeF, dirF, shapeM, shapeG, shapeF) {

  # creating directories for M-M projections in future, for M-G is not necessary as the routine
  # in a former step do it already

  dir.create(paste0(foldersp, "/G_variables"), showWarnings = F)
  dir.create(paste0(foldersp, "/G_variables/Set_1"), showWarnings = F)
  dir.create(paste0(foldersp, "/G_variables/Set_1/M"), showWarnings = F)

  Mfiles <- list.files(paste0(foldersp, "/M_variables/Set_1/"), pattern = ".asc", full.names = T, recursive = F)
  for (a in 1:length(Mfiles)) {
    file.copy(
      from = Mfiles[a],
      to = paste0(foldersp, "/G_variables/Set_1/M/"),
      overwrite = T, recursive = T
    )
  }

  if (computeF == FALSE) {

    # as was not computed F vars, is necessary to copy them from directory in which them are stored
    Fdirs <- base::list.dirs(path = dirF, recursive = F, full.names = T)

    for (a in 1:length(Fdirs)) {
      base::file.copy(
        from = Fdirs[a],
        to = paste0(foldersp, "/G_variables/Set_1/"),
        overwrite = T, recursive = T
      )
    }
  }

  if (computeF == TRUE) {

    # Start

    # future climatic folders/ directories
    fut_dir <- base::list.dirs(
      path = paste0(climdir, climdataset, "/future"),
      recursive = T,
      full.names = T
    )

    # Because the structure of future variables is nested (folder inside folder),
    # we need to find the last levels of nesting in which are located the climatic
    # variables. It could be achieve with only read the raster layers with
    # raster::raster, but it miss the information about the future model source
    # and the concentration paths.

    # divide the path in strings
    strings_dir <- strsplit(fut_dir, "/")

    # number of folders nested in each path
    quan_dir_nested <- lapply(strings_dir, length) %>% unlist()

    # last level of nesting is where the variables are stored
    max_dir <- max(quan_dir_nested)
    index_vars <- which(quan_dir_nested == max_dir)

    # loop to crop to G or M and write the future variables
    for (b in 1:length(index_vars)) {

      # which one stores the climate change scenario
      idata <- index_vars[b]

      # information of climate change scenario
      model <- strings_dir[[idata]][5]
      year <- strings_dir[[idata]][6]
      concentration <- strings_dir[[idata]][7]

      # getting vector of future scenaries information
      info_cc <- paste0(model, "_", year, "_", concentration)

      # set folders with the climate change scenario
      dir.create(paste0(foldersp, "/G_variables/Set_1/", info_cc), showWarnings = F)

      # climatic folder paths
      clim_fut_files <- list.files(
        path = fut_dir[[idata]],
        pattern = extension,
        recursive = T,
        full.names = T
      )

      # Are there other files in future scenarios?

      other_F <- list.files(
        path = paste0(envother, "/future/"),
        pattern = extension,
        recursive = F,
        full.names = T
      )

      # Merging the other variables with climatic / truly # MISSING

      if (!identical(other_F, character(0))) {
        env_Ffiles <- c(clim_fut_files, other_F)
        # missing check the names of the variables to ensure compatibility
      } else {

        # loading other variables from base scenario to use in the model as they dont change
        env_Ffiles <- clim_fut_files

        names(env_Ffiles) <- names.ras[1:length(clim_fut_files)]

        if (projMod == "M-M") {
          VarsNames <- gsub(
            pattern = ".asc", replacement = "",
            list.files(paste0(foldersp, "/M_variables/Set_1"),
              pattern = ".asc", full.names = F, recursive = F
            )
          )
        }

        if (projMod == "M-G") {
          VarsNames <- gsub(
            pattern = ".asc", replacement = "",
            list.files(paste0(foldersp, "/G_variables/Set_1/G"),
              pattern = ".asc", full.names = F, recursive = F
            )
          )
        }

        complimentVars <- VarsNames[!VarsNames %in% names(env_Ffiles)]

        other_rootfiles <- list.files(paste0(envother, "current"),
          pattern = extension, full.names = T,
          recursive = T
        )

        other_rootnames <- gsub(
          pattern = ".tif", replacement = "",
          list.files(paste0(envother, "current"),
            pattern = extension, full.names = F,
            recursive = T
          )
        )

        other_rootuse <- other_rootfiles[other_rootnames %in% complimentVars]

        env_Ffiles <- c(env_Ffiles, other_rootuse)
      }

      # writing information of future scenarios
      write.csv(cbind(model, year, concentration), paste0(foldersp, "/G_variables/Set_1/", info_cc, "/data.csv"), row.names = F)

      env_Fras <- raster::stack(env_Ffiles)

      env_Fstack <- env_Fras %>%
        raster::crop(shapeF)

      # writing future layers
      for (d in 1:nlayers(env_Fstack)) {
        env_Fd <- env_Fstack[[d]]

        value <- na.omit(values(env_Fd)) %>% max()
        y <- ndec(x = value)

        if (y == 0) {
          datTyp <- "INT2S"
          decinum <- 0
        }
        if (y >= 1) {
          datTyp <- "FLT4S"
          decinum <- 1
        }

        names(env_Fd) <- names.ras[d]
        raster::writeRaster(
          x = round(env_Fd, digits = decinum),
          filename = paste0(
            foldersp, "/G_variables/Set_1/", info_cc, "/", names(env_Fd), ".asc"
          ),
          overwrite = T,
          NAflag = -9999,
          datatype = datTyp
        )
      }
    }
  }

  return("ok")
}

#-----------------
ndec <- function(x) {
  dec <- nchar(strsplit(as.character(x), "\\.")[[1]][2])
  if (is.na(dec)) dec <- 0
  return(as.numeric(dec))
}

