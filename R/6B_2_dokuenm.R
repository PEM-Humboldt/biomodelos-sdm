
do.kuenm <- function(occ., beta.mult, fc.clas, maxent.path, sp.name, E,
                     folder.sp, biasfile, kept., proj.models, do.future, env.Mdir, env.Gdir,
                     crs.proyect, use.bias, extrap, write.intfiles, redo., 
                     redo.path ) {

  #--------------------
  # 1. Formatting occurrences to Kuenm package
  #--------------------

  kuenm.occ(occ.$occ_train, sp.name, folder.sp, "occ_train_kuenm")
  kuenm.occ(occ.$occ_test, sp.name, folder.sp, "occ_test_kuenm")
  kuenm.occ(occ.$occ_joint, sp.name, folder.sp, "occ_joint_kuenm")

  #--------------------
  # 2. calibrate models
  #--------------------

  # preparing arguments for calibration funcion Kuenm_cal()
  occjoint <- paste0(folder.sp, "/occurrences/occ_joint_kuenm.csv")
  occtra <- paste0(folder.sp, "/occurrences/occ_train_kuenm.csv")
  M_var_dir <- env.Mdir
  out_dir <- paste0(folder.sp, "/", "candidate_models")
  batch_cal <- paste0(folder.sp, "/", "candidate_models")

  if (use.bias == TRUE) {
    biasarg <- kuenm.path.bias(bias.file = biasfile, foldersp = folder.sp)
  } else {
    biasarg <- NULL
  }

  wait <- FALSE
  run <- TRUE
  
  if(redo. == F){
    kuenm::kuenm_cal(
      occ.joint = occjoint, occ.tra = occtra, M.var.dir = M_var_dir,
      batch = batch_cal, out.dir = out_dir, reg.mult = beta.mult,
      f.clas = fc.clas, args = biasarg, maxent.path = maxent.path,
      wait = wait, run = run
    )  
  }
  

  #--------------------
  # 3. evaluation of calibrated models
  #--------------------

  # preparing arguments for calibration evaluation funtion Kuenm_ceval()
  occtest <- paste0(folder.sp, "/occurrences/occ_test_kuenm.csv")
  out_eval <- paste0(folder.sp, "/", "eval_results_kuenm")
  rand_percent <- 50
  iterations. <- 500
  paral_proc <- FALSE
  threshold. <- E
  select <- "OR_AICc"
  
  # evaluate
  if(redo. == F){
    eval1 <- kuenm::kuenm_ceval(
      path = out_dir, occ.joint = occjoint, occ.tra = occtra,
      occ.test = occtest, batch = batch_cal, out.eval = out_eval,
      threshold = threshold., rand.percent = rand_percent,
      iterations = iterations., kept = kept.,
      selection = "OR_AICc", parallel.proc = paral_proc
    )
  }

  #--------------------
  # 4. selected models by kuenm met project evaluation criterion
  #--------------------

  # conditional statements to ensure that best kuenm gets fitted models
  if(redo. == F){
    
    best <- eval1$`All models`
    best <- na.omit(best)
  
    # proc greater than 1 and pvalue less than 0.1
    
    best1 <- best[which(best$Mean_AUC_ratio >= 1 & best$pval_pROC <= 0.1), ]
    
    if (nrow(best1) == 0){
      stop("any model met the test criterion")
    }else{
      # model with the OR10 less minimun value
      best2 <- best1[which(best1$`Omission_rate_at_10%` == min(best1$`Omission_rate_at_10%`)), ]
      if (nrow(best2) != 0) {
        best2$delta_AICc <- best2$delta_AICc - min(best2$delta_AICc)
        if (nrow(best2) > 1) {
          # delta aic criterion
          best3 <- best2[which(best2$delta_AICc <= 2), ]
        } else {
          best3 <- best2
        }
      }else{
        best3 <- best1
      }
    }

    write.csv(best3, paste0(out_eval, "/best_candidate_models_OR_AICc.csv"), row.names = F)
  }else{
    best3 <- read.csv(redo.path)
    dir.create(out_eval)
    write.csv(best3, paste0(out_eval, "/best_candidate_models_OR_AICc.csv"), row.names = F)
  }
  #--------------------
  # 5. current predictions
  #--------------------

  # preparing arguments for final models function Kuenm_ceval()
  batch_fin <- paste0(folder.sp, "/", "final_models")
  rep_n <- 1
  rep_type <- "Bootstrap"
  jackknife <- FALSE
  out_format <- "logistic"
  wait1 <- TRUE
  run1 <- TRUE


  dir.create(paste0(folder.sp, "/final_models_kuenm"), showWarnings = FALSE)

  mod_dir <- paste0(folder.sp, "/final_models_kuenm/")

  if (proj.models == "M-M") {
    if(do.future == FALSE){
      proj.mod <- FALSE
      G.var <- ""  
    }else{
      proj.mod <- T
      G.var <- paste0(env.Gdir, "/")  
    }
    
  } 
  
  if (proj.models == "M-G") {
    # only use with raster and shape of the same original extent (feb-2021)
    proj.mod <- T
    G.var <- paste0(env.Gdir, "/")
  }

  kuenm::kuenm_mod(
    occ.joint = occjoint, M.var.dir = M_var_dir, out.eval = out_eval, batch = batch_fin,
    rep.n = rep_n, rep.type = rep_type, jackknife = jackknife, out.dir = mod_dir,
    out.format = out_format, maxent.path = maxent.path, args = biasarg, wait = wait1,
    run = run1, project = proj.mod, G.var.dir = G.var, ext.type = extrap
  )

  
  if (proj.models == "M-M"){
    if(proj.mod == TRUE & do.future == F){
      current_proj_files <- list.files(path = paste0(folder.sp, "/final_models_kuenm"), pattern = paste0(sp.name, "_M.asc"), full.names = T, include.dirs = T, recursive = T)
    }else{
      current_proj_files <- list.files(path = paste0(folder.sp, "/final_models_kuenm"), pattern = paste0(sp.name, ".asc$"), full.names = T, include.dirs = T, recursive = T)
    }
  }
  
  if (proj.models == "M-G"){
    current_proj_files <- list.files(path = paste0(folder.sp, "/final_models_kuenm"), pattern = "G.asc$", full.names = T, include.dirs = T, recursive = T)
  }   
  
  current_proj <- raster::stack(current_proj_files)
  names(current_proj) <- best3$Model

    for (i in 1:nlayers(current_proj)) {
      fileNm <- unlist(strsplit(x = current_proj_files[i], split = "/"))

      Ras <- current_proj[[i]]
      raster::crs(Ras) <- sp::CRS(crs.proyect)
      if (write.intfiles == TRUE) {
        writeRaster(Ras, paste0(
          folder.sp, "/final_models_kuenm/", fileNm[3], "/", unlist(strsplit(fileNm[4], ".asc$")),
          ".tif"
        ),
        format = "GTiff",
        overwrite = T,
        NAflag = -9999,
        datatype = "FLT4S",
        options = "COMPRESS=LZW"
        )
      }
    }
  

  #--------------------
  # 6. future predictions
  #--------------------
  if (do.future == TRUE) {
    env.folder <- list.dirs(paste0(folder.sp, "/final_models_kuenm"), full.names = T, recursive = F)
    env.folderNm <- list.dirs(paste0(folder.sp, "/final_models_kuenm"), full.names = F, recursive = F)

    fut_proj_list <- list()

    for (i in 1:length(env.folder)) {

      # folder of layers of each model
      env.listFolder <- list.files(env.folder[i], pattern = ".asc$", full.names = T, recursive = T, include.dirs = F)
      # no future asc files
      noFRas <- c(grep("*_M.asc$", env.listFolder), grep("*_G.asc$", env.listFolder), grep(paste0(folder.sp, ".asc$"), env.listFolder))
      # removing no future files
      env.FlistRas <- env.listFolder[-noFRas]
      fut_proj <- raster::stack(env.FlistRas)

      # change asc files for tif
      # first search into folder model, read files and create stack, write the layers in tif

      for (a in 1:nlayers(fut_proj)) {

        # getting a vector with name of model and layer
        fileNm2 <- unlist(strsplit(x = env.FlistRas[a], split = "/"))

        Ras <- fut_proj[[a]]
        raster::crs(Ras) <- sp::CRS(crs.proyect)
        if (write.intfiles == TRUE) {
          writeRaster(Ras, paste0(
            folder.sp, "/final_models_kuenm/", fileNm2[3], "/", unlist(strsplit(fileNm2[4], ".asc$")),
            ".tif"
          ),
          format = "GTiff",
          overwrite = T,
          NAflag = -9999,
          datatype = "FLT4S",
          options = "COMPRESS=LZW"
          )
        }
      }

      fut_proj_list[[i]] <- fut_proj
    }
    
    names(fut_proj_list) <- env.folderNm

    # results in case of do.future = FALSE

    return(list(c_proj = current_proj, f_proj = fut_proj_list, best = best3))
  }

  # results in case of do.future = FALSE

  return(list(c_proj = current_proj, f_proj = NULL, best = best3))
}

#--------------------------
# kuenm accepts a fixed structure of data column 1 species, column 2 longitude, column 3 latitude.
# this function converts occcurrence data to this data.frame structure

kuenm.occ <- function(data., spname, foldersp, occname) {
  occ_kuenm <- data.
  occ_kuenm$species <- rep(spname, nrow(occ_kuenm))
  occ_kuenm <- occ_kuenm [, c(3, 1, 2)]

  write.csv(x = occ_kuenm, file = paste0(foldersp, "/occurrences/", occname, ".csv"), row.names = F)

  return(occ_kuenm)
}


#--------------------------

kuenm.path.bias <- function(bias.file = biasfile, foldersp = folder.sp) {
  workdir <- paste0(getwd(), "/", foldersp)

  if (.Platform$OS.type == "unix") {
    s1 <- "/"
    o <- "/"
  } else {
    s1 <- "\\"
    o <- "/"
  }

  strings <- strsplit(workdir, split = o)

  for (i in 1:length(strings)) {
    string_slashs <- paste0(strings[[i]], s1)
  }

  string_slashs <- paste0(string_slashs, collapse = "")

  start_str <- "biasfile="

  complete_bias_path <- paste0(start_str, string_slashs, bias.file, " biastype=3")

  return(complete_bias_path)
}
