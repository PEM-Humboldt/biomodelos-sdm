library(SDMtune)
do.sdmtunes <- function(occ., bias.file, beta.mult, f.class, env.Mdir, env.Gdir, env.Fdir, do.future,
                        folder.sp, col.lon, col.lat, proj.models, partitionMethod, crs.proyect, use.bias,
                        dist.Mov) {
  # background data

  if(use.bias == TRUE & !is.null(bias.file)){
    # bias sample to create the background for modeling
    if(nrow(bias.file) > 10000){
      Sbg <- bias.file[
        sample(
          x = seq(1:nrow(bias.file)),
          size = 10000,
          replace = F,
          prob = bias.file[, 3]
        ),
        1:2
      ]
    }else{
      Sbg <- bias.file[
        sample(
          x = seq(1:nrow(bias.file)),
          size = ceiling(nrow(bias.file)*0.3),
          replace = F,
          prob = bias.file[, 3]
        ),
        1:2
      ]
    }  
  }else{
    if(nrow(bias.file) > 10000){
      Sbg <- bias.file[
        sample(
          x = seq(1:nrow(bias.file)),
          size = 10000,
          replace = F
        ),
        1:2
      ]
    }else{
      Sbg <- bias.file[
        sample(
          x = seq(1:nrow(bias.file)),
          size = ceiling(nrow(bias.file)*0.3),
          replace = F
        ),
        1:2
      ]
    }
  }
  
  # reading environmental files if "M-M" it only reads M_variables from species folder or "M-G"
  # in which in will be read G_variables folder, they are in .asc extension 
  
  # M reading
  env.Mfiles <- list.files(env.Mdir, pattern = "*.asc", full.names = T, recursive = T)
  env.M <- raster::stack(env.Mfiles)
  
  # G reading
  if (proj.models == "M-G") {
    env.Gfiles <- list.files(env.Gdir, pattern = "*.asc", full.names = T, recursive = T)
    env.G <- raster::stack(env.Gfiles)
  }
  
  #---------------------------
  # 2. Formatting data
  #---------------------------
  
  data <- prepareSWD(
    species = folder.sp, p = occ.[,c(col.lon, col.lat)], a = Sbg,
    env = env.M
  )
  
  if(partitionMethod == "jackknife") bins <- get.jackknife(occ = data@coords[data@pa == 1, ], bg.coords = data@coords[data@pa == 0, ])
  if(partitionMethod == "block") bins <- get.block(occ = data@coords[data@pa == 1, ], bg.coords = data@coords[data@pa == 0, ])
  if(partitionMethod == "checkerboard1") bins <- get.checkerboard1(occ = data@coords[data@pa == 1, ], bg.coords = data@coords[data@pa == 0, ], env = env.M, aggregation.factor = dist.Mov/2)

  #---------------------------
  # 3. Calibrating models
  #---------------------------
  
  # default model
  
  model <- train(method = "Maxent", data = data, fc = "lqph", reg = 1, folds = bins)
  
  #hyerparameters
  
  h <- list(reg = beta.mult, fc = f.class)
  
  eval_1 <- gridSearch(model, hypers = h, metric = "auc", save_models = FALSE, test = bins)
  
  eval_1_tbl <- eval_1@results
  
  dir.create(paste0(folder.sp, "/eval_results_sdmtune"))
  
  write.csv(eval_1_tbl, paste0(folder.sp, "/eval_results_sdmtune/eval_models.csv"), row.names = F)
  
  # Best AUC model
  eval_1_best <- eval_1_tbl[which(eval_1_tbl$test_AUC == max(eval_1_tbl$test_AUC)), ][1, ]
  
  #--------------------
  # 4. current predictions
  #--------------------
  
  # predicting current raster layers of best models
  if (proj.models == "M-M") {
    # filter best models and predict them in M
    current_mod <- train(method = "Maxent", data = data, fc = eval_1_best$fc, reg = eval_1_best$reg)
    current_proj <- predict(current_mod, env.M)
    names(current_proj) <- paste0("model_","fc_", eval_1_best$fc,"_","reg_", eval_1_best$reg)
  } else if (proj.models == "M-G") {
    current_mod <- train(method = "Maxent", data = data, fc = eval_1_best$fc, reg = eval_1_best$reg)
    current_proj <- predict(current_mod, env.G)
    names(current_proj) <- paste0("model_","fc_", eval_1_best$fc,"_","reg_", eval_1_best$reg)
  }
  
  # write current M or G prediction layers
  
  dir.create(paste0(folder.sp, "/final_models_sdmtune"), showWarnings = F)
  dir.create(paste0(folder.sp, "/final_models_sdmtune/current"), showWarnings = F)
  
  for (i in 1:nlayers(current_proj)) {
    Ras <- current_proj[[i]]
    raster::crs(Ras) <- sp::CRS(crs.proyect)
    writeRaster(Ras, paste0(
      folder.sp, "/final_models_sdmtune/current/",
      names(current_proj[[i]]), ".tif"
    ),
    format = "GTiff",
    overwrite = T
    )
  }
  
  #---------------------------
  # 5. Projection to future
  #---------------------------
  # MISSING
  
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
      current_proj <- predict(current_mod, envFRas)
      names(current_proj) <- paste0("model_","fc_", eval_1_best$fc,"_","reg_", eval_1_best$reg)
      
    })
    
    dir.create(paste0(folder.sp, "/final_models_sdmtune/future"), showWarnings = FALSE)    
    
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
    
    return(list(c_proj = selected, f_proj = fut_proj))
  }
  
  # results in case of do.future = FALSE
  return(list(c_proj = current_proj, f_proj = NULL))
}

