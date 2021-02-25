do.enmeval <- function(occ., bias.file, beta.mult, f.class, env.Mdir, env.Gdir, env.Fdir, do.future,
                       folder.sp, col.lon, col.lat, proj.models, partitionMethod, crs.proyect) {

  #--------------------
  # 1. Formatting background and occurrences to enmeval package
  #--------------------
  
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
        size = ceiling(nrow(bias.file)*0.7),
        replace = F,
        prob = bias.file[, 3]
      ),
      1:2
    ]
  }
  
  # enmeval needs a data frame with longitude and latitude coordinates with these names and this
  # order
  data. <- occ.[, c(col.lon, col.lat)]
  names(data.) <- c("longitude", "latitude")
  
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
  
  #--------------------
  # 2. calibrate and evaluate models
  #--------------------
  
  # competitor models
  eval1 <- ENMevaluate(
    occ = data., env = env.M, bg.coords = Sbg, method = partitionMethod,
    RMvalues = beta.mult, fc = toupper(f.class), algorithm = "maxent.jar"
  )
  
  dir.create(paste0(folder.sp, "/eval_results_enmeval"), showWarnings = FALSE)
  
  # table of evaluation results
  eval_results <- eval1@results
  write.csv(eval_results, paste0(folder.sp, "/eval_results_enmeval/eval_models.csv"), row.names = F)
  
  # writing all enmeval modelling objects
  save(eval1, file = paste0(folder.sp, "/eval_results_enmeval/eval_models.RData") )
  
  #--------------------
  # 3. select models made by enmeval: meet project evaluation criterion
  #--------------------
  
  # selecting from the table the best enmeval models
  # AUC greater than 0.7
  best1 <- eval_results[which(eval_results$avg.test.AUC >= 0.7), ] %>% na.omit()
  
  if (nrow(best1) != 0) {
    # model with the OR10 less minimun value
    best2 <- best1[which(best1$avg.test.or10pct == min(best1$avg.test.or10pct)), ]
  } else {
    stop("any model met the test criterion")
  }
  
  if (nrow(best2) != 0) {
    if (nrow(best2) > 1) {
      # delta aic criterion
      best2$delta.AICc <- best2$AICc - min(best2$AICc)
      best3 <- best2[which(best2$delta.AICc <= 2), ]
    } else {
      best3 <- best2
    }
  }
  
  # select best models
  index_select <- as.numeric()
  for(i in 1:nrow(best3)){
    indexi <- which(eval_results$settings == best3$settings[i])
    index_select <- c(index_select, indexi)
  }
  
  eval1_models <- eval1@models[index_select]
  
  #write best models data frame
  write.csv(best3, paste0(folder.sp, "/eval_results_enmeval/best_models.csv"), row.names = F)
  
  # writing best modelling objects
  save(eval1_models, file = paste0(folder.sp, "/eval_results_enmeval/best_models.RData"))  
  
  #--------------------
  # 4. current predictions
  #--------------------
  
  # predicting current raster layers of best models
  if (proj.models == "M-M") {
    # filter best models and predict them in M
    current_proj <- lapply(eval1_models, function(x) dismo::predict(x, env.M))
    names(current_proj) <- best3$settings
    current_proj <- stack(current_proj)
  } else if (proj.models == "M-G") {
    current_proj <- lapply(eval1_models, function(x) dismo::predict(x, env.M))
    names(current_proj) <- best3$settings
    current_proj <- stack(current_proj)
  }
  
  # write current M or G prediction layers
  
  dir.create(paste0(folder.sp, "/final_models_enmeval"), showWarnings = F)
  dir.create(paste0(folder.sp, "/final_models_enmeval/current"), showWarnings = F)
  
  for (i in 1:nlayers(current_proj)) {
    Ras <- current_proj[[i]]
    raster::crs(Ras) <- sp::CRS(crs.proyect)
    writeRaster(Ras, paste0(
      folder.sp, "/final_models_enmeval/current/",
      names(current_proj[[i]]), ".tif"
    ),
    format = "GTiff",
    overwrite = T
    )
  }
  
  #--------------------
  # 5. future predictions
  #--------------------
  
  # prediction future
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
    names(envFRas) <- apply( envFdata[ , c(1:3) ] , 1 , paste0 , collapse = "_" )
    
    fut_proj <- list()
    for(i in 1:length(envFRas)){
      fut_proj[[i]] <- lapply(eval1_models, function(x) dismo::predict(x, envFRas[[i]]))
      names(fut_proj[[i]]) <- best3$settings
    }
    names(fut_proj) <- names(envFRas)
    fut_proj2 <- unlist(fut_proj)  
    
    dir.create(paste0(folder.sp, "/final_models_enmeval/future"))
    
    for (i in 1:length(fut_proj2)) {
      writeRaster(fut_proj2[[i]], paste0(
        folder.sp, "/final_models_enmeval/future/",
        names(fut_proj2[i]), ".tif"
      ),
      format = "GTiff",
      overwrite = T
      )
    }
    
    # results in case of do.future = TRUE
    return(list(c_proj = current_proj, f_proj = fut_proj, best = best3))
  }
  
  # results in case of do.future = FALSE
  return(list(c_proj = current_proj, f_proj = NULL, best = best3))
}