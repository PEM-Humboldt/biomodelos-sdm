
do.sdmtunes <- function(data.splitted, sp.name, folder.sp, Biasfile, nrep.s, env.Mdir, env.Gdir,
                      env.Fdir, do.future, proj.models, crs.proyect) {
  
  #--------------------------- 
  # 1. Create Pseudoabcence table
  #---------------------------
  
  if(nrow(Biasfile) > 10000){
    Sbg <- Biasfile[
      sample(
        x = seq(1:nrow(Biasfile)),
        size = 10000,
        replace = F,
        prob = Biasfile[, 3]
      ),
      1:2
    ]
  }else{
    Sbg <- Biasfile[
      sample(
        x = seq(1:nrow(Biasfile)),
        size = ceiling(nrow(bias.file)*0.7),
        replace = F,
        prob = Biasfile[, 3]
      ),
      1:2
    ]
  }
  
  
  #---------------------------
  # 2. Calibration models
  #---------------------------
  
  # reading environmental M files
  env.Mfiles <- list.files(env.Mdir, pattern = "*.asc", full.names = T, recursive = T)
  env.M <- raster::stack(env.Mfiles)
  
  train.data <- SDMtune::prepareSWD(species = sp.name, p = data.splitted$occ_train, a = Sbg,
                     env = env.M)
  test.data <- SDMtune::prepareSWD(species = sp.name, p = data.splitted$occ_test, a = Sbg, 
                                   env = env.M)
  
  #---------------------------
  # 3. Evaluation of calibrated models
  #---------------------------
  
  # E = 5: MISSING threshold election by user
  eval1 <- do.GIndeva(
    data.splitted = data.splitted, predict = best_all, mask_points = Biasfile,
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
  # 5. Projection to future
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

####################
# tuning species ANN distribution model with package sdmtune
sdmtune_ANN <- function(){
  #ANN
  
  model.ann <- SDMtune::train(method = "ANN", data = train.data, size = 10)
  
  h_ann <- list(size = 2:10, decay = c(0.01, 0.1), maxit = c(100), 
                rang = c(0.01, 0.1, 0.5, 1:5))
  
  gs_ann <- gridSearch(model.ann, hypers = h_ann, metric = "auc", test = test.data, 
                       env = env.M, save_models = FALSE)
  
  best.ann <- gs_ann@results[ order(gs_ann@results$test_AUC, decreasing = T)[1:10], ]
  
  best_annmod <- list()
  best_annstack <- stack()
  for(i in 1:nrow(best20.ann)){
    final_modeli <- SDMtune::train("ANN", data = train.data, size = best.ann$size[i], 
                                   decay = best.ann$decay[i], maxit = best.ann$maxit[i] )
    best_annmod[[i]] <- final_modeli
    # predicting
    predi <- predict(final_modeli, data = env.M, type = "logistic")
    best_annstack <- raster::stack(best_annstack, predi)
  }
  
  plot(best_annstack)
  
  # GBM or BRT
  
  time1 <- Sys.time()
  
}

#####################3
# tuning species BRT distribution model with package sdmtune
sdmtune_BRT <- function(){
  model.brt <- SDMtune::train(method = "BRT", data = train.data)
  
  h_brt <- list(n.trees = c(100, 300, 500), 
                interaction.depth = c(1:5), 
                shrinkage = c(0.01, 0.05, 0.1, 0.5), 
                bag.fraction = c(0.1, 0.5, 1)
  )
  
  gs_brt <- gridSearch(model.brt, hypers = h_brt, metric = "auc", test = test.data, 
                       env = env.M, save_models = FALSE)
  
  best.brt <- gs_brt@results[ order(gs_brt@results$test_AUC, decreasing = T)[1:10], ]
  
  best_brtmod <- list()
  best_brtstack <- raster::stack()
  for(i in 1:nrow(best.brt)){
    final_modeli <- SDMtune::train(method = "BRT", 
                                   data = train.data, 
                                   n.trees = best.brt$n.trees[i], 
                                   interaction.depth = best.brt$interaction.depth[i],
                                   shrinkage = best.brt$shrinkage[i],
                                   bag.fraction = best.brt$bag.fraction[i]
    )
    best_brtmod[[i]] <- final_modeli
    # predicting
    predi <- predict(final_modeli, data = env.M, type = "logistic")
    best_brtstack <- raster::stack(best_brtstack, predi)
  }
  
  give.msg.time(time.1 = time1)
  
  best_all <- raster::stack(best_annstack, best_brtstack)
  
  dir.create(paste0(folder.sp, "/eval_results_biomod"))
}

################
# tuning species Maxent distribution model with package sdmtune
sdmtune_maxent <- function(){
  
}

#################
# tuning species Random Fores distribution model with package sdmtune
sdmtune_rf <- function(){
  
}