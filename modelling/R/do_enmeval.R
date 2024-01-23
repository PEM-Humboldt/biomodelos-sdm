#' Calibrate, evaluate and select ecological niche models using Maxent with the ENMeval package
#' 
#' @description
#' The 'do_enmeval' function uses the Maxent algorithm with the ENMeval package to calibrate, evaluate, and select 
#' ecological niche models. It first trains a set of competing models based on the 'f.clas' and 'beta.mult' arguments, 
#' and then evaluates their performance using partial ROC, omission rate at the 10th percentile, and AICc. The 
#' function selects the best models using a hierarchical process that considers partial ROC, omission rate, and 
#' AICc in that order. Finally, the function projects the calibrated model onto projection areas and saves them 
#' in the species folder specified by the 'folder.sp' argument. 
#' 
#' @param occ. data frame containing occurrence data of the species of interest. Must have columns for longitude 
#' (col.lon) and latitude (col.lat).
#' @param bias.file data frame containing a set of background points to be used for modeling. Must have columns 
#' for longitude, latitude, and a probability weighting (in that order).
#' @param beta.mult numeric value representing the regularization multiplier to be used in Maxent model calibration.
#' @param f.clas character string indicating the feature class function to be used in Maxent model calibration (e.g. 'l', 'lq').
#' @param env.Mdir character string representing the directory where the environmental layers for the M projection (train area) 
#' are located.
#' @param env.Gdir character string representing the directory where the environmental layers for the G projection (current area) 
#' are located.
#' @param env.Fdir character string representing the directory where the environmental layers for the F projection (future area) 
#' are located.
#' @param do.future logical indicating whether to calibrate models for future projections(F).
#' @param folder.sp character string representing the output directory where calibrated models and evaluation results 
#' will be saved.
#' @param sp.name character string representing the species name to be used in file naming.
#' @param col.lon character string representing the name of the column in occ. that contains the longitude data.
#' @param col.lat character string representing the name of the column in occ. that contains the latitude data.
#' @param proj.models character string indicating which set of environmental layers to use for modeling. If 'M-M', 
#' only the M layers will be used. If 'M-G', both M and G layers will be used.
#' @param partitionMethod character string, indicating the type of partitioning method to use for cross-validation.
#' Given by ENMeval.
#' @param crs.proyect character string, indicating the coordinate reference system of the environmental layers. Must be 
#' a valid CRS string for the raster package.
#' @param use.bias logical, indicating whether to use the bias file for modeling. If FALSE, background points 
#' will be randomly sampled from the M projection area.
#' @param extrap logical, indicating whether to allow model extrapolation beyond the range of environmental predictors.
#' @param predic character string, indicating the type of prediction method to use. Must be one of 'response', 'threshold', 
#' or 'probability'.
#' @param redo. logical, indicating whether to re-run model calibration and evaluation from scratch, even if saved 
#' results already exist.
#' @param redo.path character string, representing the directory where saved model calibration and evaluation results are 
#' located, if any.
#' @param E numeric, threshold (numeric) the percentage of training data omission error allowed .
#' @param outf, (character) the model output format; it can be: "raw", "logistic", "cloglog", or "cumulative".
#'
#' @return A table of evaluation results is saved in a CSV file in the folder eval_results_enmeval within the folder.sp 
#' directory specified by the user. The table contains evaluation metrics for each model tested, including AUC, OR10, 
#' and other performance measures. A summary table of the best models is created based on the evaluation results. 
#' The table includes information such as the model name, algorithm, AUC, OR10, and other evaluation metrics.
#' The best models are selected based on various criteria, such as AUC, OR10, and delta AICc. The indices of the 
#' selected models are stored in a variable index_select.

do_enmeval <- function(occ., bias.file, beta.mult, f.clas, env.Mdir, env.Gdir, env.Fdir, do.future,
                       folder.sp, sp.name, col.lon, col.lat, proj.models, partitionMethod, crs.proyect, 
                       use.bias, extrap, predic = "kuenm", redo., redo.path, E = E, outf = outformat,
                       Max.Bg, sel., algo.enmeval, sbg.file) {

  # MISSING user choose function to predict

  # reading environmental files. If  project == "M-M" it only reads M_variables from species folder or project == "M-G"
  # in which will be read also the variables stored in G_variables folder. They are in .asc extension

  # M reading
  env.Mfiles <- list.files(env.Mdir, ".asc$", recursive = T, full.names = T)
  env.M <- raster::stack(env.Mfiles)

  # G reading
  if (proj.models == "M-G") {
    env.Gfiles <- list.files(paste0(env.Gdir, "/Set_1/G/"), pattern = "*.asc$", full.names = T, recursive = F)
    env.G <- raster::stack(env.Gfiles)
  }

  #--------------------
  # 1. Formatting background and occurrences to enmeval package
  #--------------------

  if(!is.null(sbg.file)){
    
    Sbg <- data.table::fread(sbg.file) %>% 
      data.frame()
    
    if(dim(Sbg)[2] != 2){
      error("Sample background must be a csv with longitude and latitude columns")
    } 
    
  }else{
    # bias sample to create the background for modeling
    if (use.bias == TRUE) {
      if (nrow(bias.file) > Max.Bg) {
        Sbg <- bias.file[
          sample(
            x = seq(1:nrow(bias.file)),
            size = Max.Bg,
            replace = F,
            prob = bias.file[, 3]
          ),
          1:2
        ]
      } else {
        Sbg <- bias.file[
          sample(
            x = seq(1:nrow(bias.file)),
            size = ceiling(nrow(bias.file) * 0.2),
            replace = F,
            prob = bias.file[, 3]
          ),
          1:2
        ]
      }
    } else {
      M.points <- rasterToPoints(env.M[[1]])
      if (nrow(M.points) > Max.Bg) {
        Sbg <- M.points[
          sample(
            x = seq(1:nrow(M.points)),
            size = Max.Bg,
            replace = F
          ),
          1:2
        ]
      } else {
        Sbg <- M.points[
          sample(
            x = seq(1:nrow(M.points)),
            size = ceiling(nrow(M.points) * 0.2),
            replace = F
          ),
          1:2
        ]
      }
    }  
  }
  
  if(nrow(Sbg) > Max.Bg){
    Sbg <- Sbg[
      sample(
        x = seq(1:nrow(Sbg)),
        size = Max.Bg,
        replace = F
      ),
      1:2
    ]
  }

  colnames(Sbg) <- c("longitude", "latitude")
  Sbg_env <- raster::extract(env.M, Sbg)

  Sbg <- cbind(Sbg, Sbg_env)

  # enmeval needs a data frame with longitude and latitude coordinates with these names and this
  # order

  data. <- occ.[, c(col.lon, col.lat)]
  names(data.) <- c("longitude", "latitude")

  data.env <- raster::extract(env.M, data.)
  data. <- cbind(data., data.env)
  
  tm <- rbind(data., Sbg)
  tm$pres <- c(rep(1, nrow(data.)), rep(0, nrow(Sbg)))
  
  write.csv(tm, paste0(folder.sp, "/occurrences/pbg.csv"), row.names = F)

  #----------+----------
  # 2. calibrate and evaluate models
  #--------------------

  # competitor models
  if (redo. == F) {
    eval1 <- ENMevaluate(
      occs = data., bg = Sbg, partitions = partitionMethod,
      tune.args = list(fc = toupper(f.clas), rm = beta.mult), algorithm = algo.enmeval,#"maxent.jar",
      doClamp = F, user.eval = proc
    )

    dir.create(paste0(folder.sp, "/eval_results_enmeval"), showWarnings = FALSE)

    # table of evaluation results
    eval_results <- eval1@results
    write.csv(eval_results, paste0(folder.sp, "/eval_results_enmeval/eval_models.csv"), row.names = F)
  }

  #--------------------
  # 3. select models created by enmeval: meet project evaluation criterion
  #--------------------

  if (redo. == F) {
    
    best <- model.selection(evaldata = eval_results, oc = occ., sel = sel.)
    best$fc <- as.character(best$fc)

    # select best models
    index_select <- as.numeric()
    for (i in 1:nrow(best)) {
      indexi <- which(eval_results$tune.args == best$tune.args[i])
      index_select <- c(index_select, indexi)
    }

    eval1_models <- eval1@models[index_select]

    # write best models data frame
    write.csv(best, paste0(folder.sp, "/eval_results_enmeval/best_models.csv"), row.names = F)

    # writing best modelling objects
    save(eval1_models, file = paste0(folder.sp, "/eval_results_enmeval/best_models.RData"))

    # best models table kuenm style
    if (predic == "kuenm") {
      if(sum(best$fc == "LQHP") >= 1){
        index.fc <- which(best$fc == "LQHP")
        best[index.fc,"fc"] <- "LQPH"
        
      } 
      best_kuenm_style <- data.frame(Model = as.character(paste0("M_", best$rm, "_F_", tolower(best$fc), "_Set_1")))
      write.csv(best_kuenm_style, paste0(folder.sp, "/eval_results_enmeval/selected_models.csv"), row.names = F)
    }
  } else {
    best <- read.csv(redo.path)
    dir.create(paste0(folder.sp, "/eval_results_enmeval/"))
    write.csv(best, paste0(folder.sp, "/eval_results_enmeval/best_models.csv"), row.names = F)
  }

  #--------------------
  # 4. Predictions
  #--------------------

  dir.create(paste0(folder.sp, "/final_models_enmeval"), showWarnings = F)

  if (use.bias == TRUE) {
    biasfile <- "BiasfileM.asc"
    biasarg <- kuenm.path.bias(bias.file = biasfile, foldersp = folder.sp)
  } else {
    biasarg <- NULL
  }

  # predicting current raster layers of best models

  if (proj.models == "M-M") {

    if (predic == "kuenm") {
      if (do.future == TRUE) {
        proj <- TRUE
      } else {
        proj <- FALSE
      }
      kuenm.occ(occ.[, -c(1, 2)], spname = folder.sp, foldersp = folder.sp, occname = "occ_joint_kuenm")
      kuenm::kuenm_mod(
        occ.joint = paste0(folder.sp, "/occurrences/occ_joint_kuenm.csv"),
        M.var.dir = env.Mdir, out.eval = paste0(folder.sp, "/eval_results_enmeval"),
        batch = paste0(folder.sp, "/final_models"), rep.n = 1, rep.type = "Bootstrap",
        jackknife = FALSE, out.dir = paste0(folder.sp, "/final_models_enmeval"),
        max.memory = 2000, out.format = outf,
        project = proj, G.var.dir = env.Gdir, ext.type = extrap, write.mess = FALSE,
        write.clamp = FALSE, maxent.path = getwd(), 
        args = c(biasarg, paste0("maximumbackground=", Max.Bg)), wait = TRUE, run = TRUE
      )
    }
  }

  if (proj.models == "M-G") {
    proj <- TRUE

    if (predic == "kuenm") {
      kuenm.occ(occ.[, -c(1, 2)], spname = folder.sp, foldersp = folder.sp, occname = "occ_joint_kuenm")
      kuenm::kuenm_mod(
        occ.joint = paste0(folder.sp, "/occurrences/occ_joint_kuenm.csv"),
        M.var.dir = env.Mdir, out.eval = paste0(folder.sp, "/eval_results_enmeval"),
        batch = paste0(folder.sp, "/final_models"), rep.n = 1, rep.type = "Bootstrap",
        jackknife = FALSE, out.dir = paste0(folder.sp, "/final_models_enmeval"),
        max.memory = 2000, out.format = outf,
        project = proj, G.var.dir = env.Gdir, ext.type = extrap, write.mess = FALSE,
        write.clamp = FALSE, maxent.path = getwd(), 
        args = c(biasarg, paste0("maximumbackground=", Max.Bg)), wait = TRUE, run = TRUE
      )
    }
  }

  # searching raster layers

  if (predic == "kuenm") {
    if (proj.models == "M-M") {
      if (proj == TRUE) {
        current_M_files <- list.files(
          path = paste0(folder.sp, "/final_models_enmeval"), pattern = paste0(folder.sp, "_M.asc"), 
          full.names = T, include.dirs = T, recursive = T
          )
      } else {
        current_M_files <- list.files(
          path = paste0(folder.sp, "/final_models_enmeval"), pattern = paste0(".asc$"), 
          full.names = T, include.dirs = T, recursive = T
          )
      }
      current_M_proj <- terra::rast(current_M_files)
      names(current_M_proj) <- paste0("M_", best$rm, "_F_", tolower(best$fc), "_Set_1")
      
      # Create G object to complete the returned set of data
      current_G_proj <- NULL
      
    }

    if (proj.models == "M-G") {
      current_M_files <- list.files(
        path = paste0(folder.sp, "/final_models_enmeval"), pattern = paste0(folder.sp, "_M.asc"),
        full.names = T, include.dirs = T, recursive = T
      )

      current_M_proj <- terra::rast(current_M_files)
      names(current_M_proj) <- paste0("M_", best$rm, "_F_", tolower(best$fc), "_Set_1")

      current_G_files <- list.files(
        path = paste0(folder.sp, "/final_models_enmeval"),
        pattern = "*_G.asc$", full.names = T, include.dirs = T, recursive = T
      )

      current_G_proj <- terra::rast(current_G_files)
      names(current_G_proj) <- paste0("G_", best$rm, "_F_", tolower(best$fc), "_Set_1")
    }
  }

  #--------------------
  # 5. future predictions
  #--------------------

  # prediction future
  if (do.future == TRUE) {
    env.folder <- list.dirs(paste0(folder.sp, "/final_models_enmeval"), full.names = T, recursive = F)
    env.folderNm <- list.dirs(paste0(folder.sp, "/final_models_enmeval"), full.names = F, recursive = F)

    if (predic == "kuenm") {
      fut_proj_list <- list()
      for (i in 1:length(env.folder)) {

        # folder of layers of each model
        env.listFolder <- list.files(env.folder[i], pattern = ".asc$", full.names = T, recursive = T, include.dirs = F)
        env.listnms <- list.files(env.folder[i], pattern = ".asc$", full.names = F, recursive = T, include.dirs = F)
        # no future asc files
        noFRas <- c(grep("*_M.asc$", env.listFolder), grep("*_G.asc$", env.listFolder), 
                    grep(paste0(folder.sp, ".asc$"), env.listFolder))
        # removing no future files
        env.FlistRas <- env.listFolder[-noFRas]
        fut_proj <- terra::rast(env.FlistRas)
        names(fut_proj) <- env.listnms[-noFRas]

        fut_proj_list[[i]] <- fut_proj
      }
      names(fut_proj_list) <- env.folderNm
    }


    # results in case of do.future = TRUE
    return(list(M_proj = current_M_proj, G_proj = current_G_proj, f_proj = fut_proj_list, best = best, 
                algorithm = "MAXENT"))
  }

  # results in case of do.future = FALSE
  return(list(M_proj = current_M_proj, G_proj = current_G_proj, f_proj = NULL, best = best, algorithm = "MAXENT"))
}


#--------------------------------------------------------------------------------
proc <- function(vars) {
  proc <- kuenm::kuenm_proc(vars$occs.val.pred, c(vars$bg.train.pred, vars$bg.val.pred))
  out <- data.frame(
    proc_auc_ratio = proc$pROC_summary[1],
    proc_pval = proc$pROC_summary[2], row.names = NULL
  )
  return(out)
}

#--------------------------------------------------------------------------------

# AUC greater than 0.7
# PROC-AUC selection

proc.selection <- function(evaldata, oc){
  if (nrow(oc) > 20){
    evalresult <- evaldata[which(evaldata$proc_auc_ratio.avg >= 1), ]
  }
  if (nrow(oc) <= 20){
    evalresult <- evaldata[which(evaldata$auc.train >= 0.7), ]
  }
  
  if(nrow(evalresult) == 0){
    message("any model met the ROC test criterion")
  }
  
  return(evalresult)
}

#---------------------------------------------------------

# or selection

or.selection <- function(evaldata){
  
  evalresult <- evaldata[which(evaldata$or.10p.avg == min(evaldata$or.10p.avg)), ]
  
  if(nrow(evalresult) == 0){
    message("any model met the OR 10 test criterion")
  }
  
  return(evalresult)
}

#------------------------------------------------------------

# aic selection
aic.selection <- function(evaldata = selected_models){
  if(nrow(evaldata) != 0){
    if (nrow(evaldata) > 1) {
      # delta aic criterion
      evalresult <- evaldata[which(evaldata$AICc == min(evaldata$AICc, na.rm = T))[1], ]
    } else {
      evalresult <- evaldata
    }
    return(evalresult)
  }else{
    message("any model met the last test criterion (aic)")
  }
}

#-------------------------------------------------------------

# delta aic selection
delta.aic.selection <- function(evaldata){
  if(nrow(evaldata) != 0){
    if (nrow(evaldata) > 1) {
      # delta aic criterion
      evaldata$delta.AICc <- evaldata$AICc - min(evaldata$AICc, na.rm = T)
      evalresult <- evaldata[which(evaldata$delta.AICc <= 2), ]
    } else {
      evalresult <- evaldata
    }
    return(evalresult)
  }else{
    message("any model met the last test criterion (delta aic)")
  }
}

#---------------------------------------------------------

model.selection <- function(evaldata, oc, sel) {
  selected_models <- evaldata  # Inicializar con los datos originales
  for (method in sel) {
    if (method == "d.aic") {
      selected_models <- delta.aic.selection(selected_models)
    } else if (method == "aic") {
      selected_models <- aic.selection(selected_models)
    } else if (method == "or") {
      selected_models <- or.selection(selected_models)
    } else if (method == "proc") {
      selected_models <- proc.selection(selected_models, oc)
    }
  }
  return(selected_models)
}
