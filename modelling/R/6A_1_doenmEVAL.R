do.enmeval <- function(occ., bias.file, beta.mult, f.clas, env.Mdir, env.Gdir, env.Fdir, do.future,
                       folder.sp, sp.name, col.lon, col.lat, proj.models, partitionMethod, crs.proyect, use.bias,
                       extrap, predic, write.intfiles, redo., redo.path, E = E) {

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

  # bias sample to create the background for modeling
  if (use.bias == TRUE) {
    if (nrow(bias.file) > 10000) {
      Sbg <- bias.file[
        sample(
          x = seq(1:nrow(bias.file)),
          size = 10000,
          replace = F,
          prob = bias.file[, 3]
        ),
        1:2
      ]
    } else {
      Sbg <- bias.file[
        sample(
          x = seq(1:nrow(bias.file)),
          size = ceiling(nrow(bias.file) * 0.3),
          replace = F,
          prob = bias.file[, 3]
        ),
        1:2
      ]
    }
  } else {
    M.points <- rasterToPoints(env.M[[1]])
    if (nrow(M.points) > 10000) {
      Sbg <- M.points[
        sample(
          x = seq(1:nrow(M.points)),
          size = 10000,
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

  colnames(Sbg) <- c("longitude", "latitude")
  Sbg_env <- raster::extract(env.M, Sbg)

  Sbg <- cbind(Sbg, Sbg_env)

  # enmeval needs a data frame with longitude and latitude coordinates with these names and this
  # order

  data. <- occ.[, c(col.lon, col.lat)]
  names(data.) <- c("longitude", "latitude")

  data.env <- raster::extract(env.M, data.)
  data. <- cbind(data., data.env)

  #----------+----------
  # 2. calibrate and evaluate models
  #--------------------

  # competitor models
  if (redo. == F) {
    eval1 <- ENMevaluate(
      occs = data., bg = Sbg, partitions = partitionMethod,
      tune.args = list(fc = toupper(f.clas), rm = beta.mult), algorithm = "maxent.jar",
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
    # selecting from the table the best enmeval models
    # AUC greater than 0.7

    if (nrow(occ.) > 25) best1 <- eval_results[which(eval_results$proc_auc_ratio.avg >= 1), ]
    if (nrow(occ.) <= 25) best1 <- eval_results[which(eval_results$auc.train >= 0.7), ]

    if (nrow(best1) == 0) {
      stop("any model met the test criterion")
    } else {
      # model with the OR10 less minimun value
      best2 <- best1[which(best1$or.10p.avg == min(best1$or.10p.avg)), ]

      if (nrow(best2) != 0) {
        if (nrow(best2) > 1) {
          # delta aic criterion
          best2$delta.AICc <- best2$AICc - min(best2$AICc, na.rm = T)
          best3 <- best2[which(best2$delta.AICc <= 2), ]
        } else {
          best3 <- best2
        }
      } else {
        best3 <- best1
      }
    }

    # select best models
    index_select <- as.numeric()
    for (i in 1:nrow(best3)) {
      indexi <- which(eval_results$tune.args == best3$tune.args[i])
      index_select <- c(index_select, indexi)
    }

    eval1_models <- eval1@models[index_select]

    # write best models data frame
    write.csv(best3, paste0(folder.sp, "/eval_results_enmeval/best_models.csv"), row.names = F)

    # writing best modelling objects
    save(eval1_models, file = paste0(folder.sp, "/eval_results_enmeval/best_models.RData"))

    # best models table kuenm style
    if (predic == "kuenm") {
      best_kuenm_style <- data.frame(Model = as.character(paste0("M_", best3$rm, "_F_", tolower(best3$fc), "_Set_1")))
      write.csv(best_kuenm_style, paste0(folder.sp, "/eval_results_enmeval/selected_models.csv"), row.names = F)
    }
  } else {
    best3 <- read.csv(redo.path)
    dir.create(paste0(folder.sp, "/eval_results_enmeval/"))
    write.csv(best3, paste0(folder.sp, "/eval_results_enmeval/best_models.csv"), row.names = F)
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
    # if (predic == "dismo") {
    #   # filter best models and predict them in M
    #   current_proj <- lapply(eval1_models, function(x) dismo::predict(x, env.M))
    #   names(current_proj) <- best3$settings
    #   current_proj <- stack(current_proj)
    # }
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
        max.memory = 2000, out.format = "cloglog",
        project = proj, G.var.dir = env.Gdir, ext.type = extrap, write.mess = FALSE,
        write.clamp = FALSE, maxent.path = getwd(), args = biasarg, wait = TRUE, run = TRUE
      )
    }
  }

  if (proj.models == "M-G") {
    proj <- TRUE
    # if (predic == "dismo" & extrap == "ext_clam") {
    #   current_proj <- lapply(eval1_models, function(x) dismo::predict(x, env.G))
    #   names(current_proj) <- best3$settings
    #   current_proj <- stack(current_proj)
    # }
    if (predic == "kuenm") {
      kuenm.occ(occ.[, -c(1, 2)], spname = folder.sp, foldersp = folder.sp, occname = "occ_joint_kuenm")
      kuenm::kuenm_mod(
        occ.joint = paste0(folder.sp, "/occurrences/occ_joint_kuenm.csv"),
        M.var.dir = env.Mdir, out.eval = paste0(folder.sp, "/eval_results_enmeval"),
        batch = paste0(folder.sp, "/final_models"), rep.n = 1, rep.type = "Bootstrap",
        jackknife = FALSE, out.dir = paste0(folder.sp, "/final_models_enmeval"),
        max.memory = 2000, out.format = "cloglog",
        project = proj, G.var.dir = env.Gdir, ext.type = extrap, write.mess = FALSE,
        write.clamp = FALSE, maxent.path = getwd(), args = biasarg, wait = TRUE, run = TRUE
      )
    }
  }

  # write current M or G prediction layers using dismo or maxnet
  # if (predic == "dismo") {
  #   if (proj.models == "M-G") folwrite <- "G"
  #   if (proj.models == "M-M") folwrite <- "M"
  #
  #   if (write.intfiles == TRUE) {
  #     for (i in 1:nlayers(current_proj)) {
  #       dir.create(paste0(folder.sp, "/final_models_enmeval/", names(current_proj[[i]])))
  #       Ras <- current_proj[[i]]
  #       raster::crs(Ras) <- sp::CRS(crs.proyect)
  #
  #       writeRaster(Ras, paste0(
  #         folder.sp, "/final_models_enmeval/", names(current_proj[[i]]), "/",
  #         folder.sp, folwrite, ".tif"
  #       ),
  #       format = "GTiff",
  #       overwrite = T,
  #       NAflag = -9999,
  #       datatype = "FLT4S",
  #       options = "COMPRESS=LZW"
  #       )
  #     }
  #   }
  # }

  # searching raster layers

  if (predic == "kuenm") {
    if (proj.models == "M-M") {
      if (proj == TRUE) {
        current_M_files <- list.files(
          path = paste0(folder.sp, "/final_models_enmeval"), pattern = paste0(sp.name, "_M.asc"), 
          full.names = T, include.dirs = T, recursive = T
          )
      } else {
        current_M_files <- list.files(
          path = paste0(folder.sp, "/final_models_enmeval"), pattern = paste0(".asc$"), 
          full.names = T, include.dirs = T, recursive = T
          )
      }
      current_M_proj <- terra::rast(current_M_files)
      names(current_M_proj) <- paste0("M_", best3$rm, "_F_", tolower(best3$fc), "_Set_1")
      
      # Create G object to complete the returned set of data
      current_G_proj <- NULL
      
    }

    if (proj.models == "M-G") {
      current_M_files <- list.files(
        path = paste0(folder.sp, "/final_models_enmeval"), pattern = paste0(sp.name, "_M.asc"),
        full.names = T, include.dirs = T, recursive = T
      )

      current_M_proj <- terra::rast(current_M_files)
      names(current_M_proj) <- paste0("M_", best3$rm, "_F_", tolower(best3$fc), "_Set_1")

      current_G_files <- list.files(
        path = paste0(folder.sp, "/final_models_enmeval"),
        pattern = "G.asc$", full.names = T, include.dirs = T, recursive = T
      )

      current_G_proj <- terra::rast(current_G_files)
      names(current_G_proj) <- paste0("G_", best3$rm, "_F_", tolower(best3$fc), "_Set_1")
    }
  }

  #--------------------
  # 5. future predictions
  #--------------------

  # prediction future
  if (do.future == TRUE) {
    env.folder <- list.dirs(paste0(folder.sp, "/final_models_enmeval"), full.names = T, recursive = F)
    env.folderNm <- list.dirs(paste0(folder.sp, "/final_models_enmeval"), full.names = F, recursive = F)

    # if (predic == "dismo" & extrap == "ext_clam") {
    #   env.FlistRas <- lapply(env.Ffolder, function(x) {
    #     list.files(x, pattern = "*.asc", all.files = T, full.names = T)
    #   })
    #   env.Fdata <- lapply(env.Ffolder, function(x) {
    #     a <- list.files(x, pattern = "*.csv", all.files = T, full.names = T)
    #     b <- read.csv(a)
    #   })
    #   envFdata <- dplyr::bind_rows(env.Fdata)
    #   envFRas <- lapply(env.FlistRas, raster::stack)
    #   names(envFRas) <- apply(envFdata[, c(1:3)], 1, paste0, collapse = "_")
    # 
    #   fut_proj <- list()
    #   for (i in 1:length(envFRas)) {
    #     fut_proj[[i]] <- lapply(eval1_models, function(x) dismo::predict(x, envFRas[[i]]))
    #     names(fut_proj[[i]]) <- best3$settings
    #   }
    #   names(fut_proj) <- names(envFRas)
    #   fut_proj2 <- unlist(fut_proj)
    # 
    #   dir.create(paste0(folder.sp, "/final_models_enmeval/future"))
    # 
    #   if (write.intfiles == TRUE) {
    #     for (i in 1:length(fut_proj2)) {
    #       writeRaster(fut_proj2[[i]], paste0(
    #         folder.sp, "/final_models_enmeval/future/",
    #         names(fut_proj2[i]), ".tif"
    #       ),
    #       format = "GTiff",
    #       overwrite = T,
    #       NAflag = -9999,
    #       datatype = "FLT4S",
    #       options = "COMPRESS=LZW"
    #       )
    #     }
    #   }
    # }

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

        # change asc files for tif
        # first search into folder model, read files and create stack, write the layers in tif
        # if (write.intfiles == TRUE) {
        #   for (a in 1:nlayers(fut_proj)) {
        # 
        #     # getting a vector with name of model and layer
        #     fileNm2 <- unlist(strsplit(x = env.FlistRas[a], split = "/"))
        # 
        #     Ras <- fut_proj[[a]]
        #     raster::crs(Ras) <- sp::CRS(crs.proyect)
        #     writeRaster(Ras, paste0(
        #       folder.sp, "/final_models_enmeval/", fileNm2[3], "/", unlist(strsplit(fileNm2[4], ".asc$")),
        #       ".tif"
        #     ),
        #     format = "GTiff",
        #     overwrite = T,
        #     NAflag = -9999,
        #     datatype = "FLT4S",
        #     options = "COMPRESS=LZW"
        #     )
        #   }
        # }

        fut_proj_list[[i]] <- fut_proj
      }
      names(fut_proj_list) <- env.folderNm
    }


    # results in case of do.future = TRUE
    return(list(M_proj = current_M_proj, G_proj = current_G_proj, f_proj = fut_proj_list, best = best3))
  }

  # results in case of do.future = FALSE
  return(list(M_proj = current_M_proj, G_proj = current_G_proj, f_proj = NULL, best = best3))
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
