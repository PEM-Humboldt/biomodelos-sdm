
do.biomod <- function(data.splitted, sp.name, folder.sp, Biasfile, nrep.s, env.Mdir, env.Gdir,
                      env.Fdir, do.future, proj.models, crs.proyect) {

  #--------------------------- 
  # 1. Create Pseudoabcence table
  #---------------------------

  bckgBst <- dobackPAlist(dat = data.splitted[["occ_train"]], BiasfilePo = Biasfile)

  #---------------------------
  # 2. Calibration models
  #---------------------------

  # reading environmental M files
  env.Mfiles <- list.files(env.Mdir, pattern = "*.asc", full.names = T, recursive = T)
  env.M <- raster::stack(env.Mfiles)

  current_train <- biomod_fit(
    spat.data = bckgBst[["backgBst_spat"]], PA.data = bckgBst[["backgBst_logicDf"]],
    foldersp = folder.sp, algos. = c("ANN", "GBM"), envM = env.M, split. = 90,
    type.mod = "current_cal", clamp = FALSE, nreps = nrep.s
  )

  dir.create(paste0(folder.sp, "/eval_results_biomod"))

  #---------------------------
  # 3. Evaluation of calibrated models
  #---------------------------

  # E = 5: MISSING threshold election by user
  eval1 <- do.GIndeva(
    data.splitted = data.splitted, predict = current_train$predictions, mask_points = Biasfile,
    foldersp = folder.sp
  )

  write.csv(eval1, paste0(folder.sp, "/eval_results_biomod/eval_models.csv"), row.names = F)
  
  #---------------------------
  # 4. Selecting best models
  #---------------------------

  # selecting from the table the best biomod models
  # partial roc and significance
  best1 <- eval1[which(eval1$pval_rocp <= 0.05), ]
  best1 <- best1[which(best1$rocptest >= 1), ]
  
  # omission rate criterion
  if (nrow(best1) != 0) {
    # model with the OR10 less minimum value
    best2 <- best1[which(best1$pbinomial <= 0.05), ]
    best2 <- best2[which(best2$OR == min(best2$OR)), ]
  } else {
    stop("any model met the test criterion")
  }
  
  if(nrow(best2) == 0){
    message("any model met the test criterion")
    return(NULL)
  } 

  # write table of best models
  write.csv(best2, paste0(folder.sp, "/eval_results_biomod/best_models.csv"), row.names = F)

  # write current predictions
  # how many models to create an index to extract the predictions of the best models
  index <- as.numeric(1:nrow(best2))

  for (i in 1:nrow(best2)) {
    a <- which(names(current_train$predictions) == best2$model.name[i])
    index[i] <- a
  }
  
  selected <- current_train$predictions[[index]]
  selected <- selected/1000 # from scale to 0-1000 to more traditional 0-1
  
  # Biomod models have a long name, best to deal with shorter
  short_name <- gsub(paste0(sp.name, "._PA1_RUN"), "rep", x = best2$model.name)

  raster::projection(selected) <- sp::CRS(crs.proyect)

  # writing current predictions in M

  dir.create(paste0(folder.sp, "/final_models_biomod/"))
  dir.create(paste0(folder.sp, "/final_models_biomod/current"))

  for (i in 1:nlayers(selected)) {
    raster::writeRaster(
      x = selected[[i]],
      filename = paste0(
        folder.sp, "/Final_models_biomod/current/",
        short_name[[i]], ".tif"
      ),
      overwrite = T
    )
  }

  # projection in case of projection to G

  if (proj.models == "M-G") {
    env.Gfiles <- list.files(env.Gdir, pattern = "*.asc", full.names = T, recursive = T)
    env.G <- raster::stack(env.Gfiles)

    sp_modProjG <- BIOMOD_Projection(
      modeling.output = current_train$biomod_models, new.env = env.G,
      proj.name = "G_models", selected.models = best2$model.name, compress = "gzip",
      clamping.mask = F, output.format = ".img", do.stack = FALSE
    )

    # retrieve predictions

    predictionsG <- get_predictions(sp_modProjG)
    predictionsG <- predictionsG/1000 # from scale to 0-1000 to more traditional 0-1
    raster::projection(predictionsG) <- sp::CRS(crs.proyect)
    names(predictionsG) <- short_name

    dir.create(paste0(folder.sp, "/final_models_biomod/current_G"))

    for (i in 1:nlayers(predictionsG)) {
      raster::writeRaster(
        x = predictionsG[[i]],
        filename = paste0(
          folder.sp, "/Final_models_biomod/current_G/",
          short_name[[i]], ".tif"
        ),
        overwrite = T
      )
    }
  }

  #---------------------------
  # 3. Projection to future
  #---------------------------

  if (do.future == TRUE) {
    env.Ffolder <- list.dirs(env.Fdir, full.names = T, recursive = F)
    env.FlistRas <- lapply(env.Ffolder, function(x) {
      list.files(x, pattern = "*.asc", all.files = T, full.names = T)
    })
    env.Fdata <- lapply(env.Ffolder, function(x) {
      a <- list.files(x, pattern = "*.csv", all.files = T, full.names = T)
      b <- read.csv(a)
    })

    envFdata <- dplyr::bind_rows(env.Fdata)
    envFRas <- lapply(env.FlistRas, raster::stack)
    names(envFRas) <- apply(envFdata[, c(1:3)], 1, paste0, collapse = "_")

    fut_bio_proj <- lapply(envFRas, function(x) {
      sp_modProjFut <- BIOMOD_Projection(
        modeling.output = current_train$biomod_models, new.env = x,
        proj.name = "future_models", selected.models = best2$model.name, compress = "gzip",
        clamping.mask = F, output.format = ".img", do.stack = FALSE
      )
    })
    
    dir.create(paste0(folder.sp, "/final_models_biomod/future"), showWarnings = FALSE)    
    
    predictionsF <- list()

    for (i in 1:length(fut_bio_proj)) {
      predx <- get_predictions(fut_bio_proj[[1]])
      names(predx) <- short_name
      predictionsF[[i]] <- predx
    }

    names(predictionsF) <- names(fut_bio_proj)

    for (i in 1:length(predictionsF)) {
      brick <- predictionsF[[i]]/1000 # from scale to 0-1000 to more traditional 0-1
      raster::projection(brick) <- sp::CRS(crs.proyect)
        for (a in 1:nlayers(brick)){
          layer <- brick[[a]]
          writeRaster(Ras, paste0(
          folder.sp, "/final_models_biomod/future/",
          names(predictionsF[i]), ".", names(brick[[a]]), ".tif"
          ),
        overwrite = T
        )
      }
    }
    
    #results in case of do.future = TRUE
      
    return(list(c_proj = selected, f_proj = fut_proj, best = best2))
  }
  
  # results in case of do.future = FALSE
  return(list(c_proj = selected, f_proj = NULL, best = best2))
}

#-------------------------

dobackPAlist <- function(dat, BiasfilePo) {

  ## Pseudoabscence-background selection using bias file, n bootstrapped samples

  BSpo <- BiasfilePo %>%
    data.frame() %>%
    dplyr::rename(longitude = lon, latitude = lat)
  BSpo <- BSpo[, c(1:2)]


  # 1.1 Coordinates from bias file (pseudoabscence-background) and training data sets are taken.

  backgPres_coord <- rbind(BSpo, dat)

  # 1.2 Give them category of pseudoabscence-background (NA) and presence (1) to each coordinate.

  backgPres_data <- c(rep(NA, nrow(BiasfilePo)), rep(1, nrow(dat))) %>% data.frame()

  # 1.3. Biomod accept better when we work in a sp object. Use the coordinates and data.

  backgPres_spat <- SpatialPointsDataFrame(coords = backgPres_coord, data = backgPres_data)

  # 2. It is necessary to construct a Biomod formatted table. Each column represent a sample,
  # each row a coordinate of the all extent. Each cell stores a logic value, TRUE or FALSE. This
  # value means active or inactive coordinate to get into modelling process Biomod.

  # 2.1 logic template, it is composed with FALSE info (inactive coordinate). Those will be activate
  # in the next lines

  backgPres_logic <- c(rep(FALSE, nrow(BiasfilePo)), rep(TRUE, nrow(dat)))

  # 2.2 extract a sample of 10000 coordinates with replace and having in account the sample probability
  
  if(nrow(BiasfilePo) > 10000){
    sBias <- sample(x = seq(1:nrow(BiasfilePo)), size = 10000, replace = TRUE, prob = BiasfilePo[, 3])
  }else{
    sBias <- sample(x = seq(1:nrow(BiasfilePo)), size = ceiling(nrow(BiasfilePo)*0.7), replace = TRUE, prob = BiasfilePo[, 3])  
  }
  # 2.3 activate each coordinate of the sample taken from the bias file, active = TRUE

  backgPres_logic[sBias] <- TRUE


  backgPres_logicdf <- data.frame(backgPres_logic)

  return(list(backgBst_spat = backgPres_spat, backgBst_logicDf = backgPres_logicdf))
}


############################################

biomod_fit <- function(spat.data, PA.data, foldersp, algos., envM, split., type.mod, clamp, nreps) {

  # Formatting data for Biomod

  sp_data <- BIOMOD_FormatingData(spat.data,
    expl.var = envM,
    resp.name = foldersp,
    PA.strategy = "user.defined",
    PA.table = PA.data
  )
  # Biomod Modeling

  sp_models <- BIOMOD_Modeling(sp_data,
    models = algos.,
    resp_name = foldersp,
    DataSplit = split.,
    NbRunEval = nreps,
    do.full.models = FALSE,
    VarImport = FALSE,
    modeling.id = type.mod
  )


  # make predictions present
  # projecting models to present

  sp_modProjCurrent <- BIOMOD_Projection(
    modeling.output = sp_models,
    new.env = envM,
    proj.name = type.mod,
    selected.models = "all",
    compress = "gzip",
    clamping.mask = clamp,
    output.format = ".img",
    do.stack = FALSE
  )

  # retrieve predictions

  predictions <- get_predictions(sp_modProjCurrent)

  return(list(
    biomod_datafor = sp_data, biomod_models = sp_models, biomod_proj = sp_modProjCurrent,
    predictions = predictions
  ))
}
