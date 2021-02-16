# data.= occurrence splitted list
# predict = raster or raster stack
# mask_points = raster of environmental extent in points
# folder_Sp = path of species folder


do.GIndeva <- function(data.splitted, predict, mask_points, foldersp) {

  # sample background weighting probability as bias

  Sbg <- mask_points[
    sample(
      x = seq(1:nrow(mask_points)),
      size = 10000,
      replace = F,
      prob = mask_points[, 3]
    ),
    1:2
  ]

  indeva_list <- list()

  for (i in 1:nlayers(predict)) {
    eval_i <- do.Indeva(predictionx = predict[[i]], data. = data.splitted, SBground = Sbg)
    indeva_list[[i]] <- eval_i
  }

  indeva_df <- do.call(rbind.data.frame, indeva_list)

  write.csv(indeva_df, paste0(foldersp, "/", "indEVA.csv"), row.names = F)

  return(indeva_df)
}

#--------------------------

# Independent Evaluation

do.Indeva <- function(predictionx, data., SBground, folder.sp = foldersp) {

  ######################
  # ROC test
  #####################

  # extract test data

  test_projvalue <- raster::extract(predictionx, data.[["occ_test"]])

  # extract background data

  Sbg_projvalue <- raster::extract(predictionx, SBground) %>% na.omit()

  # merging background and test data

  dataTestBg <- data.frame(data = c(rep(1, length(test_projvalue)), rep(0, length(Sbg_projvalue))))

  # data formatted to get into roc

  roc_data <- data.frame(
    id = 1:(length(test_projvalue) + length(Sbg_projvalue)),
    data = dataTestBg,
    values = c(test_projvalue, Sbg_projvalue)
  )

  roctest <- PresenceAbsence::auc(roc_data, st.dev = TRUE, which.model = 1, na.rm = T) %>% round(digits = 3)

  #########################
  # partial ROC
  ########################

  rocp <- kuenm::kuenm_proc(occ.test = data.[["occ_test"]], model = predictionx)

  #########################
  # binomial
  ########################

  # extract predicted values of all data

  joint_projvalue <- raster::extract(predictionx, data.[["occ_joint"]]) %>%
    sort()

  # data bin_threshold %

  databin_threshold <- round((length(joint_projvalue) * 10) / 100)

  ## Converting continuos prediction to binary according to bin_threshold

  bin <- predictionx > joint_projvalue[databin_threshold]

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

  bin_prediction <- extract(bin, data.[["occ_joint"]])

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
    model.name = names(predictionx),
    roc.mean = roctest[, 1],
    roc.sd = roctest[, 2],
    rocptest = rocp[[1]][1] %>% round(digits = 3),
    pval_rocp = rocp[[1]][2],
    PArea = res[1],
    pbinomial = res[2],
    OR = res[3],
    stringsAsFactors = FALSE
  )

  return(indEVA.df)
}

#--------------------------
