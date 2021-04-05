# environmental variables

process_env.current <- function(clim.dataset, clim.dir, extension, crs.proyect, area.M,
                                env.other, folder.sp, dofuture, area.G, proj.models,
                                compute.G) {
  # extent of M or G to cut the variables
  if (proj.models == "M-M" | compute.G == FALSE) {
    cropArea <- area.M
  } else {
    # read G extent
    cropArea <- raster::raster(area.G)
  }

  # climatic folder paths

  clim_files <- list.files(
    path = paste0(clim.dir, clim.dataset, "/current/"),
    pattern = extension,
    recursive = F,
    full.names = T
  )

  # read climatic raster
  clim_merge <- raster::stack(clim_files)

  # reduce climatic extent to crop area dimensions
  clim_crop <- raster::crop(clim_merge, cropArea)

  # other variables apart from the climatic ones
  other_files <- list.files(
    path = paste0(env.other, "/current/"),
    pattern = extension,
    recursive = F,
    full.names = T
  )

  # Merging the other variables with climatic
  if (!identical(other_files, character(0))) {
    other_merge <- raster::stack(other_files)
    crs(other_merge) <- sp::CRS(crs.proyect)
    other_crop <- raster::crop(other_merge, cropArea)
    env_crop <- raster::stack(clim_crop, other_crop)
    names_ras <- names(env_crop)
  } else {
    env_crop <- clim_crop
    names_ras <- names(env_crop)
  }

  raster::res(env_crop) <- 1/120
  
  # masking environmental data to accesible area or M

  env_M <- raster::crop(env_crop, area.M)
  env_M <- raster::mask(env_M, area.M)
  
  #---------------------
    # writing environmental layers

    if (proj.models == "M-G") {
      if (compute.G == TRUE) {

        # area G

        # set G folder

        dir.create(paste0(folder.sp, "/G_variables"), showWarnings = F)
        dir.create(paste0(folder.sp, "/G_variables/Set_1"), showWarnings = F)

        # write G area, Maxent needs ".asc" files

        for (i in 1:nlayers(env_crop)) {
          raster::writeRaster(
            x = env_crop[[i]],
            filename = paste0(
              folder.sp, "/G_variables/Set_1/", names(env_crop[[i]]), ".asc"
            ),
            overwrite = T, NAflag = -9999
          )
        }
      }
    }

  # area M

  # set M folder

  dir.create(paste0(folder.sp, "/M_variables"), showWarnings = F)
  dir.create(paste0(folder.sp, "/M_variables/Set_1"), showWarnings = F)

  # write M area, Maxent needs ".asc" files

  for (i in 1:nlayers(env_M)) {
    raster::writeRaster(
      x = env_M[[i]],
      filename = paste0(
        folder.sp, "/M_variables/Set_1/", names(env_M[[i]]), ".asc"
      ),
      overwrite = T, NAflag = -9999
    )
  }

  if (proj.models == "M-M" | compute.G == FALSE) {
    env.Ras <- env_M
  } else {
    env.Ras <- env_crop
  }

  if (dofuture == TRUE) {
    env_F <- process_env.future(
      climdataset = clim.dataset,
      climdir = clim.dir,
      exten = extension,
      crsproyect = crs.proyect,
      envRas = env.Ras,
      projMod = proj.models,
      envother = env.other,
      foldersp = folder.sp,
      names.ras = names_ras
    )

    # results do.future == TRUE

    if (proj.models == "M-M" | compute.G == FALSE) {
      return(list(M = env_M, G = NULL, Future = env_F))
    } else {
      return(list(M = env_M, G = env_crop, Future = env_F))
    }
  }

  # results do.future == FALSE

  if (proj.models == "M-M" | compute.G == FALSE) {
    return(list(M = env_M, G = NULL))
  } else {
    return(list(M = env_M, G = env_crop))
  }
}

#-------------------------
process_env.future <- function(climdataset, climdir, extension, crsproyect, G, envother,
                               foldersp, M, projMod, names.ras, envRas = env.Ras) {

  # future climatic folders/ directories
  fut_dir <- list.dirs(
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

  # last level of nesting is where the variables are stores
  max_dir <- max(quan_dir_nested)
  index_vars <- which(quan_dir_nested == max_dir)

  # loop to crop to G and write the future variables
  for (i in 1:length(index_vars)) {
    idata <- index_vars[i]
    set_ <- i

    model <- strings_dir[[idata]][5]
    year <- strings_dir[[idata]][6]
    concentration <- strings_dir[[idata]][7]

    # climatic folder paths
    clim_fut_files <- list.files(
      path = fut_dir[[idata]],
      pattern = extension,
      recursive = T,
      full.names = T
    )

    clim_F_merge <- raster::stack(clim_fut_files)

    # reduce future climatic extent to current ennvironmental raster
    clim_F <- raster::crop(clim_F_merge, envRas)
    clim_F <- projectRaster(clim_F, envRas)
    clim_F <- raster::mask(clim_F, envRas)

    # Are there other files?

    other_F <- list.files(
      path = paste0(envother, "/future/"),
      pattern = extension,
      recursive = F,
      full.names = T
    )

    # Merging the other variables with climatic
    if (!identical(other_F, character(0))) {
      other_F_merge <- raster::stack(other_F)
      crs(other_F_merge) <- sp::CRS(crs.proyect)
      other_F <- raster::crop(other_F_merge, clim_F)
      other_F <- raster::projectRaster(other_F, clim_F)
      other_F <- raster::mask(other_F, clim_F)
      names(env_F) <- names.ras
    } else {
      env_F <- clim_F
      names(env_F) <- names.ras
    }

    # set folders
    dir.create(paste0(foldersp, "/F_variables"), showWarnings = F)
    dir.create(paste0(foldersp, "/F_variables/Set_", set_), showWarnings = F)

    write.csv(cbind(model, year, concentration), paste0(foldersp, "/F_variables/Set_", set_, "/data.csv"), row.names = F)
    for (a in 1:nlayers(env_F)) {
      raster::writeRaster(
        x = env_F[[a]],
        filename = paste0(
          foldersp, "/F_variables/Set_", set_, "/", names(env_F[[a]]), ".asc"
        ),
        overwrite = T, NAflag = -9999
      )
    }
  }

  return("ok")
}
