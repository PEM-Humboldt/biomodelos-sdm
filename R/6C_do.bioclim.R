do.bioclim <- function(occ. = M_$occurrences, env.Mdir = paste0(folder_sp, "/M_variables"),
                       env.Gdir = paste0(folder_sp, "/G_variables"), folder.sp = folder_sp,
                       col.lon = col_lon, col.lat = col_lat, proj.models = proj_models,
                       crs.proyect = crs_proyect, extrap = extrapo, predic = predic) {
  if (proj.models == "M-M") {
    # M reading
    env.Mfiles <- list.files(env.Mdir, pattern = "*.asc", full.names = T, recursive = T)
    env.Data <- raster::stack(env.Mfiles)
  }

  if (proj.models == "M-G") {
    # G reading
    env.Gfiles <- list.files(paste0(env.Gdir, "/Set_1/G/"), pattern = "*.asc$", full.names = T, recursive = F)
    env.Data <- raster::stack(env.Gfiles)
  }

  numlayers <- 1:nlayers(env.Data)

  lscom <- list()
  for (i in numlayers) {
    if (numlayers[i] >= 2) {
      combisi <- combn(x = 1:nlayers(env.Data), m = numlayers[i])
      lscom[[i]] <- combisi
    }
  }

  lscom <- lscom[lengths(lscom) != 0]

  lscomAll <- list()
  predictStack <- stack()

  for (c in 1:length(lscom)) {
    lsindeva <- list()

    vector <- 1:ncol(lscom[[c]])

    for (a in 1:length(vector)) {
      vectora <- lscom[[c]][, vector[a]]
      v <- extract(env.Data[[vectora]], occ.[, c(col.lon, col.lat)])
      bc <- bioclim(v)
      vars <- paste0(colnames(v), collapse = "_")
      p1 <- predict(env.Data, bc)
      names(p1) <- paste0(vars, "_BIOCLIM")
      predictStack <- stack(predictStack, p1)

      mask_points <- rasterToPoints(p1)

      Sbg <- mask_points[
        sample(
          x = seq(1:nrow(mask_points)),
          size = 10000,
          replace = F
        ),
        1:2
      ]

      # extract background data

      Sbg_projvalue <- raster::extract(p1, Sbg) %>% na.omit()

      # extract data

      train_projvalue <- raster::extract(p1, occ.[, c(col.lon, col.lat)])

      # merging background and test data

      dataTestBg <- data.frame(data = c(rep(1, length(train_projvalue)), rep(0, length(Sbg_projvalue))))

      roc_data <- data.frame(
        id = 1:(length(train_projvalue) + length(Sbg_projvalue)),
        data = dataTestBg,
        values = c(train_projvalue, Sbg_projvalue)
      )

      roctest <- PresenceAbsence::auc(roc_data, st.dev = TRUE, which.model = 1, na.rm = T) %>% round(digits = 3)

      #########################
      # binomial
      ########################

      # extract predicted values of all data

      train_projvalue <- train_projvalue %>%
        sort()

      # data bin_threshold %

      databin_threshold <- floor((length(train_projvalue) * 10) / 100)
      if (databin_threshold < 1) {
        databin_threshold <- 1
      }

      ## Converting continuos prediction to binary according to bin_threshold

      bin <- p1 > train_projvalue[databin_threshold]

      # writing binary prediction

      textbin_threshold <- as.character(10)

      ## Number of pixels in each class
      clases <- freq(bin)

      # Number of total cells (different to NA)

      if (length(clases[which(clases[, 1] == 0), 2]) >= 1) {
        npixels <- clases[which(clases[, 1] == 1), 2] + clases[which(clases[, 1] == 0), 2]
      } else {
        npixels <- unname(clases[which(clases[, 1] == 1), 2])
      }

      # Proportion of predicted area

      Parea <- (unname(clases[which(clases[, 1] == 1), 2]) / npixels) %>% round(digits = 2)

      # bin data

      bin_prediction <- extract(bin, occ.[, c(col.lon, col.lat)])

      # Presences predicted positively

      success <- length(which(bin_prediction == 1))

      # False negatives

      fail <- length(which(bin_prediction == 0))

      # Binomial test

      pbin <- 1 - pbinom(success, size = success + fail, prob = Parea) %>% round(digits = 5)

      # Omission rate

      OR <- ((fail) / (fail + success)) %>% round(digits = 2)

      res <- cbind(Parea, pbin, OR)

      indEVA.df <- data.frame(
        model.name = names(p1),
        auc.mean = roctest[, 1],
        auc.sd = roctest[, 2],
        PArea = res[1],
        pbinomial = res[2],
        OR = res[3],
        stringsAsFactors = FALSE
      )

      lsindeva[[a]] <- indEVA.df
    }

    lscomAll[[c]] <- do.call(rbind.data.frame, lsindeva)
  }

  eval_results <- do.call(rbind.data.frame, lscomAll)

  dir.create(paste0(folder.sp, "/", "eval_results_bioclim"), showWarnings = F)
  write.csv(eval_results, paste0(folder.sp, "/", "eval_results_bioclim", "/", "eval_models.csv"), row.names = F)

  best1 <- eval_results[which(eval_results$auc.mean >= 0.7), ] %>% na.omit()

  if (nrow(best1) != 0) {
    # model with the OR10 less minimun value
    best2 <- best1[which(best1$OR == min(best1$OR)), ]
  } else {
    stop("any model met the test criterion")
  }

  index_select <- as.numeric()
  for (i in 1:nrow(best2)) {
    indexi <- which(eval_results$model.name == best2$model.name[i])
    index_select <- c(index_select, indexi)
  }

  eval1_models <- predictStack[[index_select]]

  # write best models data frame
  write.csv(best2, paste0(folder.sp, "/eval_results_bioclim/best_models.csv"), row.names = F)

  dir.create(paste0(folder.sp, "/", "final_models_bioclim"), showWarnings = F)

  for (i in 1:nlayers(eval1_models)) {
    writeRaster(eval1_models[[i]],
      filename = paste0(
        folder.sp, "/",
        "final_models_bioclim", "/",
        names(eval1_models[[i]]), ".tif"
      ), format = "GTiff",
      overwrite = T,
      NAflag = -9999,
      datatype = "FLT4S",
      options = "COMPRESS=LZW"
    )
  }


  return(list(c_proj = eval1_models))
}
