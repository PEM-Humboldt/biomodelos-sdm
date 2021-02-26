
do.kuenm <- function(occ., beta.mult, fc.clas, maxent.path, selection., sp.name,
                     folder.sp, biasfile, kept., proj.models, do.future, env.Mdir, env.Gdir,
                     env.Fdir, crs.proyect) {
  
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
  batch_cal <- paste0(folder.sp, "candidate_models")
  biasarg <- kuenm.path.bias(bias.file = biasfile, foldersp = folder.sp)
  wait <- FALSE
  run <- TRUE
  
  kuenm::kuenm_cal(
    occ.joint = occjoint, occ.tra = occtra, M.var.dir = M_var_dir,
    batch = batch_cal, out.dir = out_dir, reg.mult = beta.mult,
    f.clas = fc.clas, args = biasarg, maxent.path = maxent.path,
    wait = wait, run = run
  )
  
  #--------------------
  # 3. evaluation of calibrated models
  #--------------------
  
  # preparing arguments for calibration evaluation funtion Kuenm_ceval()
  occtest <- paste0(folder.sp, "/occurrences/occ_test_kuenm.csv")
  out_eval <- paste0(folder.sp, "/", "eval_results_kuenm")
  rand_percent <- 50
  iterations. <- 500
  paral_proc <- FALSE
  threshold. <- 5 # MISSING option for user
  
  # evaluate
  eval1 <- kuenm::kuenm_ceval(
    path = out_dir, occ.joint = occjoint, occ.tra = occtra,
    occ.test = occtest, batch = batch_cal, out.eval = out_eval,
    threshold = threshold., rand.percent = rand_percent,
    iterations = iterations., kept = kept.,
    selection = selection., parallel.proc = paral_proc
  )
  
  #--------------------
  # 4. selected models by kuenm met project evaluation criterion
  #--------------------
  
  # conditional statements to ensure that best kuenm gets fitted models
  best <- eval1$`All models`
  best <- na.omit(best)
  
  # AUC greater than 0.7
  best1 <- best[which(best$Mean_AUC_ratio >= 1 & best$pval_pROC <= 0.05), ]
  
  if (nrow(best1) != 0) {
    # model with the OR10 less minimun value
    best2 <- best1[which(best1$`Omission_rate_at_5%` == min(best1$`Omission_rate_at_5%`)), ]
  } else {
    message("any model met the test criterion")
    return(NULL)
  }
  
  if (nrow(best2) != 0) {
    best2$delta_AICc <- best2$delta_AICc - min(best2$delta_AICc)
    if (nrow(best2) > 1) {
      # delta aic criterion
      best3 <- best2[which(best2$delta_AICc <= 2), ]
    } else {
      best3 <- best2
    }
  }
  
  write.csv(best3, paste0(out_eval, "/best_candidate_models_OR_AICc.csv"), row.names = F)
  
  #--------------------
  # 5. current predictions
  #--------------------
  
  # preparing arguments for final models function Kuenm_ceval()
  batch_fin <- paste0(folder.sp, "final_models")
  rep_n <- 1
  rep_type <- "Bootstrap"
  jackknife <- FALSE
  out_format <- "logistic"
  wait1 <- TRUE
  run1 <- TRUE
  extrap <- "no_ext"
  
  dir.create(paste0(folder.sp, "/final_models_kuenm"), showWarnings = FALSE)
  mod_dir <- paste0(folder.sp, "/final_models_kuenm/current")
  
  if (proj.models == "M-M") {
    proj.mod <- FALSE
    G.var <- ""
  } else if (proj.models == "M-G") {
    # only use with raster and shape of the same original extent, at this moment (feb-2021)
    # is impossible to use with IQR outlier dropping method, only with freq method.
    proj.mod <- T
    G.var <- paste0(env.Gdir, "/")
  }
  
  kuenm::kuenm_mod(
    occ.joint = occjoint, M.var.dir = M_var_dir, out.eval = out_eval, batch = batch_fin,
    rep.n = rep_n, rep.type = rep_type, jackknife = jackknife, out.dir = mod_dir,
    out.format = out_format, maxent.path = maxent.path, args = biasarg, wait = wait1,
    run = run1, project = proj.mod, G.var.dir = G.var, ext.type = extrap
  )
  
  current_proj <- list.files( path = paste0(mod_dir, "/"), pattern = ".asc",full.names = T, include.dirs = T, recursive = T)
  current_proj <- raster::stack(current_proj)
  names(current_proj) <- best3$Model
  
  # Erasing .asc files in order to make small the size of species folders
  
  #erase.asc <- list.dirs(paste0(folder.sp, "/final_models_kuenm/current/"), full.names = T, recursive = F)
  
  #for (i in 1:length(erase.asc)) {
  #  if (file.exists(erase.asc[i])) {
  #    unlink(erase.asc[i], recursive = T, force = T)
  #  }
  #}
  
  # writing final models in .tif extensions
  for (i in 1:nlayers(current_proj)) {
    raster::writeRaster(
      x = current_proj[[i]],
      filename = paste0(
        folder.sp, "/Final_models_kuenm/current/",
        best3$Model[i], ".tif"
      ),
      overwrite = T
    )
  }
  
  
  #--------------------
  # 6. future predictions
  #--------------------
  
  if (do.future == TRUE) {
    envF <- paste0(env.Fdir, "/")
    proj.mod <- TRUE
    mod_dirF <- paste0(folder.sp, "/final_models_kuenm/future")
    kuenm::kuenm_mod(
      occ.joint = occjoint, M.var.dir = M_var_dir, out.eval = out_eval, batch = batch_fin, 
      rep.n = rep_n, rep.type = rep_type, jackknife = jackknife, out.dir = mod_dirF, 
      out.format = out_format, maxent.path = maxent.path, args = biasarg, wait = wait1,
      run = run1, project = proj.mod, G.var.dir = envF, ext.type = extrap
    )
    
    # results in case of do.future = TRUE
    return(list(c_proj = current_proj, f_proj = NULL, best = best))
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
  
  workdir <- paste0(getwd(),"/", foldersp)
  
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
