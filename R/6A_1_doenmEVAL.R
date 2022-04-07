do.enmeval <- function(occ., bias.file, beta.mult, f.class, env.Mdir, env.Gdir, env.Fdir, do.future,
                       folder.sp, sp.name, col.lon, col.lat, proj.models, partitionMethod, crs.proyect, use.bias,
                       extrap, predic, write.intfiles) {

  # MISSING user choose function to predict

  # reading environmental files if "M-M" it only reads M_variables from species folder or "M-G"
  # in which in will be read G_variables folder, they are in .asc extension

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


  # enmeval needs a data frame with longitude and latitude coordinates with these names and this
  # order
  data. <- occ.[, c(col.lon, col.lat)]
  names(data.) <- c("longitude", "latitude")

  #----------+----------
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
  save(eval1, file = paste0(folder.sp, "/eval_results_enmeval/eval_models.RData"))

  #--------------------
  # 3. select models made by enmeval: meet project evaluation criterion
  #--------------------

  # selecting from the table the best enmeval models
  # AUC greater than 0.7
  best1 <- eval_results[which(eval_results$train.AUC >= 0.7), ] %>% na.omit()
  
  if (nrow(best1) == 0) stop("any model met the test criterion")

  # model with the OR10 less minimun value
  if (nrow(best1) != 0)  best2 <- best1[which(best1$avg.test.or10pct == min(best1$avg.test.or10pct)), ]
  
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
  for (i in 1:nrow(best3)) {
    indexi <- which(eval_results$settings == best3$settings[i])
    index_select <- c(index_select, indexi)
  }

  eval1_models <- eval1@models[index_select]

  # write best models data frame
  write.csv(best3, paste0(folder.sp, "/eval_results_enmeval/selected_models.csv"), row.names = F)

  # writing best modelling objects
  save(eval1_models, file = paste0(folder.sp, "/eval_results_enmeval/selected_models.RData"))

  # best models table kuenm style
  if (predic == "kuenm") {
    best_kuenm_style <- data.frame(Model = paste0("M_", best3$rm, "_F_", tolower(best3$features), "_Set_1"))
    write.csv(best_kuenm_style, paste0(folder.sp, "/eval_results_enmeval/best_models.csv"), row.names = F)
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
    if (predic == "dismo") {
      # filter best models and predict them in M
      current_proj <- lapply(eval1_models, function(x) dismo::predict(x, env.M))
      names(current_proj) <- best3$settings
      current_proj <- stack(current_proj)
    }
    if (predic == "kuenm") {
      if(do.future == TRUE){
        proj <- TRUE
      }else{
          proj <- FALSE
          }
      kuenm.occ(occ.[, -c(1, 2)], spname = folder.sp, foldersp = folder.sp, occname = "occ_joint_kuenm")
      kuenm::kuenm_mod(
        occ.joint = paste0(folder.sp, "/occurrences/occ_joint_kuenm.csv"),
        M.var.dir = env.Mdir, out.eval = paste0(folder.sp, "/eval_results_enmeval"),
        batch = paste0(folder.sp, "/final_models"), rep.n = 1, rep.type = "Bootstrap",
        jackknife = FALSE, out.dir = paste0(folder.sp, "/final_models_enmeval"),
        max.memory = 2000, out.format = "logistic",
        project = proj, G.var.dir = env.Gdir, ext.type = extrap, write.mess = FALSE,
        write.clamp = FALSE, maxent.path = getwd(), args = biasarg, wait = TRUE, run = TRUE
      )
    }
  }

  if (proj.models == "M-G") {
    proj <- TRUE
    if (predic == "dismo" & extrap == "ext_clam") {
      current_proj <- lapply(eval1_models, function(x) dismo::predict(x, env.G))
      names(current_proj) <- best3$settings
      current_proj <- stack(current_proj)
    }
    if (predic == "kuenm") {
      kuenm.occ(occ.[, -c(1, 2)], spname = folder.sp, foldersp = folder.sp, occname = "occ_joint_kuenm")
      kuenm::kuenm_mod(
        occ.joint = paste0(folder.sp, "/occurrences/occ_joint_kuenm.csv"),
        M.var.dir = env.Mdir, out.eval = paste0(folder.sp, "/eval_results_enmeval"),
        batch = paste0(folder.sp, "/final_models"), rep.n = 1, rep.type = "Bootstrap",
        jackknife = FALSE, out.dir = paste0(folder.sp, "/final_models_enmeval"),
        max.memory = 2000, out.format = "logistic",
        project = proj, G.var.dir = env.Gdir, ext.type = extrap, write.mess = FALSE,
        write.clamp = FALSE, maxent.path = getwd(), args = biasarg, wait = TRUE, run = TRUE
      )
    }
  }

  # write current M or G prediction layers using dismo or maxnet
  if (predic == "dismo") {
    if (proj.models == "M-G") folwrite <- "G"
    if (proj.models == "M-M") folwrite <- "M"

    if (write.intfiles == TRUE) {
      for (i in 1:nlayers(current_proj)) {
        dir.create(paste0(folder.sp, "/final_models_enmeval/", names(current_proj[[i]])))
        Ras <- current_proj[[i]]
        raster::crs(Ras) <- sp::CRS(crs.proyect)

        writeRaster(Ras, paste0(
          folder.sp, "/final_models_enmeval/", names(current_proj[[i]]), "/",
          folder.sp, folwrite, ".tif"
        ),
        format = "GTiff",
        overwrite = T,
        NAflag = -9999,
        datatype = "FLT4S",
        options = "COMPRESS=LZW"
        )
      }
    }
  }

  # searching and changing extension of raster layers from asc to tif

  if (predic == "kuenm") {
    if (proj.models == "M-M"){
      if(proj == TRUE){
        current_proj_files <- list.files(path = paste0(folder.sp, "/final_models_enmeval"), pattern = paste0(sp.name, "_M.asc"), full.names = T, include.dirs = T, recursive = T)
      }else{
        current_proj_files <- list.files(path = paste0(folder.sp, "/final_models_enmeval"), pattern = paste0(".asc$"), full.names = T, include.dirs = T, recursive = T)
      }
    } 
      
  if (proj.models == "M-G") current_proj_files <- list.files(path = paste0(folder.sp, "/final_models_enmeval"), pattern = "G.asc$", full.names = T, include.dirs = T, recursive = T)

    current_proj <- raster::stack(current_proj_files)
    names(current_proj) <- best3$settings

    if (write.intfiles == TRUE) {
      for (i in 1:nlayers(current_proj)) {
        fileNm <- unlist(strsplit(x = current_proj_files[i], split = "/"))

        Ras <- current_proj[[i]]
        raster::crs(Ras) <- sp::CRS(crs.proyect)

        writeRaster(Ras, paste0(
          folder.sp, "/final_models_enmeval/", fileNm[3], "/", unlist(strsplit(fileNm[4], ".asc$")),
          ".tif"
        ),
        format = "GTiff",
        NAflag = -9999,
        datatype = "FLT4S",
        options = "COMPRESS=LZW"
        )
      }
    }
  }

  #--------------------
  # 5. future predictions
  #--------------------

  # prediction future
  if (do.future == TRUE) {
    env.folder <- list.dirs(paste0(folder.sp, "/final_models_enmeval"), full.names = T, recursive = F)
    env.folderNm <- list.dirs(paste0(folder.sp, "/final_models_enmeval"), full.names = F, recursive = F)

    if (predic == "dismo" & extrap == "ext_clam") {
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

      fut_proj <- list()
      for (i in 1:length(envFRas)) {
        fut_proj[[i]] <- lapply(eval1_models, function(x) dismo::predict(x, envFRas[[i]]))
        names(fut_proj[[i]]) <- best3$settings
      }
      names(fut_proj) <- names(envFRas)
      fut_proj2 <- unlist(fut_proj)

      dir.create(paste0(folder.sp, "/final_models_enmeval/future"))

      if (write.intfiles == TRUE) {
        for (i in 1:length(fut_proj2)) {
          writeRaster(fut_proj2[[i]], paste0(
            folder.sp, "/final_models_enmeval/future/",
            names(fut_proj2[i]), ".tif"
          ),
          format = "GTiff",
          overwrite = T,
          NAflag = -9999,
          datatype = "FLT4S",
          options = "COMPRESS=LZW"
          )
        }
      }
    }

    if (predic == "kuenm") {
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
        if (write.intfiles == TRUE) {
          for (a in 1:nlayers(fut_proj)) {

            # getting a vector with name of model and layer
            fileNm2 <- unlist(strsplit(x = env.FlistRas[a], split = "/"))

            Ras <- fut_proj[[a]]
            raster::crs(Ras) <- sp::CRS(crs.proyect)
            writeRaster(Ras, paste0(
              folder.sp, "/final_models_enmeval/", fileNm2[3], "/", unlist(strsplit(fileNm2[4], ".asc$")),
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
    }


    # results in case of do.future = TRUE
    return(list(c_proj = current_proj, f_proj = fut_proj_list, best = best3))
  }

  # results in case of do.future = FALSE
  return(list(c_proj = current_proj, f_proj = NULL, best = best3))
}
