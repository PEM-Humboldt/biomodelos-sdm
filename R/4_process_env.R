# environmental variables

process_env.current <- function(clim.dataset, clim.dir, extension, crs.proyect, area.M,
                                env.other, folder.sp, dofuture, area.G, proj.models,
                                compute.G, compute.F, dir.G, dir.F) {
  
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

  raster::res(env_crop) <- res(clim_merge)

  # masking environmental data to accesible area or M

  env_M <- raster::crop(env_crop, area.M)
  env_M <- raster::mask(env_M, area.M)

  template.ras <- env_M[[1]]

  #---------------------
  # writing environmental layers

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
      overwrite = T,
      NAflag = -9999,
      datatype = "FLT4S",
      options = "COMPRESS=LZW"
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
      template.ras <- env_crop[[1]]

      # write G area if it was computed

      for (i in 1:nlayers(env_crop)) {
        raster::writeRaster(
          x = env_crop[[i]],
          filename = paste0(
            folder.sp, "/G_variables/Set_1/G/", names(env_crop[[i]]), ".asc"
          ),
          overwrite = T,
          NAflag = -9999,
          datatype = "FLT4S",
          options = "COMPRESS=LZW"
        )
      }
    } else {

      # as was not computed G vars, is necessary to copy them from directory used
      Gfiles <- list.files(dir.G, pattern = ".asc", full.names = T, recursive = F)
      template.ras <- raster::raster(Gfiles[1])

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

  if (proj.models == "M-M") env.Ras <- env_M
  if (proj.models == "M-G") env.Ras <- raster::stack(list.files(paste0(folder.sp, "/G_variables/Set_1/G/"), pattern = ".asc$", full.names = T, recursive = F))


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
      names.ras = names_ras,
      NclimCurrent = nlayers(clim_merge),
      computeF = compute.F,
      dirF = dir.F,
      templateRas = template.ras
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
                               foldersp, M, projMod, names.ras, envRas = env.Ras, NclimCurrent,
                               computeF, dirF, templateRas) {

  # creating directories for M-M projections in future, for M-G is not necessary as the routine
  # in a former step do it already

  if (computeF == FALSE) {

    # as was not computed F vars, is necessary to copy them from directory in which them are stored
    Fdirs <- list.dirs(dirF, recursive = F, full.names = T)

    for (a in 1:length(Fdirs)) {
      file.copy(
        from = Fdirs[a],
        to = paste0(foldersp, "/G_variables/Set_1/"),
        overwrite = T, recursive = T
      )
    }
  }

  if (computeF == TRUE) {
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


    # Start

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

      clim_F_merge <- raster::stack(clim_fut_files)

      # reduce future climatic extent to current/base ennvironmental raster
      clim_F <- raster::crop(clim_F_merge, envRas[[1]])
      clim_F <- raster::mask(clim_F, envRas[[1]])

      # Are there other files in future scenaries?

      other_F <- list.files(
        path = paste0(envother, "/future/"),
        pattern = extension,
        recursive = F,
        full.names = T
      )

      # Merging the other variables with climatic / truly # MISSING

      if (!identical(other_F, character(0))) {
        other_F_merge <- raster::stack(other_F)
        crs(other_F_merge) <- sp::CRS(crs.proyect)
        other_F <- raster::crop(other_F_merge, clim_F)
        other_F <- raster::projectRaster(other_F, clim_F)
        other_F <- raster::mask(other_F, clim_F)
        names(env_F) <- names.ras
      } else {

        # loading other variables from base scenario to use in the model as they dont change
        env_F <- clim_F
        names(env_F) <- names.ras[1:NclimCurrent]

        if (projMod == "M-M") complimentVars <- list.files(paste0(foldersp, "/M_variables/Set_1/"), pattern = ".asc", full.names = T, recursive = F)
        if (projMod == "M-G") complimentVars <- list.files(paste0(foldersp, "/G_variables/Set_1/G"), pattern = ".asc", full.names = T, recursive = F)


        complimentVars <- complimentVars[-c(1:NclimCurrent)]


        for (c in 1:length(complimentVars)) {
          file.copy(
            from = complimentVars[c],
            to = paste0(foldersp, "/G_variables/Set_1/", info_cc),
            overwrite = T, recursive = T
          )
        }
      }

      # writing information of future scenaries
      write.csv(cbind(model, year, concentration), paste0(foldersp, "/G_variables/Set_1/", info_cc, "/data.csv"), row.names = F)

      raster::extent(env_F) <- raster::extent(templateRas)
      raster::res(env_F) <- raster::res(templateRas)

      # writing future layers
      for (d in 1:nlayers(env_F)) {
        raster::writeRaster(
          x = env_F[[d]],
          filename = paste0(
            foldersp, "/G_variables/Set_1/", info_cc, "/", names(env_F[[d]]), ".asc"
          ),
          overwrite = T,
          NAflag = -9999,
          datatype = "FLT4S",
          options = "COMPRESS=LZW"
        )
      }
    }
  }



  return("ok")
}
