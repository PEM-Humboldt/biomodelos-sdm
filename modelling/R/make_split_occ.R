
# The occurrences were divide using the package dismo following the method chess. The size of each
# cell was calculated through the calculated range of a semivariogram. The range specifies from
# what distance a spatial data base reduces its autocorrelation.

make_split_occ <- function(occ., bias.file, folder.sp, col.lon, col.lat, use.bias, env.M, Max.Bg,
                           sbg.file) {
  
  if(!is.null(sbg.file)){
    
    Sbg <- data.table::fread(sbg.file) %>% 
      data.frame()
    
    if(dim(Sbg)[2] != 2){
      error("Sample background must be a csv with longitude and latitude columns")
    } 
    
  }else{
    
    # bias sample to create the background for modeling
    if(use.bias == TRUE){
      if(nrow(bias.file) > Max.Bg){
        Sbg <- bias.file[
          sample(
            x = seq(1:nrow(bias.file)),
            size = Max.Bg,
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
      M.points <- rasterToPoints(env.M[[1]])
      if(nrow(M.points) > Max.Bg){
        Sbg <- M.points[
          sample(
            x = seq(1:nrow(M.points)),
            size = Max.Bg,
            replace = F
          ),
          1:2
        ]
      }else{
        Sbg <- M.points[
          sample(
            x = seq(1:nrow(M.points)),
            size = ceiling(nrow(M.points)*0.3),
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
  
  tm.Sbg <- raster::extract(env.M, Sbg)
  tm.Sbg <- cbind(Sbg, tm.Sbg)
  
  # enmeval way to split needs a data frame with longitude and latitude coordinates with these names and this
  # order
  data. <- occ.[, c(col.lon, col.lat)]
  names(data.) <- c("longitude", "latitude")
  
  tm.env <- raster::extract(env.M, data.)
  tm.env <- cbind(data., tm.env)
  
  tm <- rbind(tm.env, tm.Sbg)
  tm$pres <- c(rep(1, nrow(data.)), rep(0, nrow(Sbg)))
  
  write.csv(tm, paste0(folder.sp, "/occurrences/pbg.csv"), row.names = F)
  
  # creating blocks

  bins <- ENMeval::get.block(data., Sbg)
  bins.data <- cbind(data., bins = bins$occs.grp)
  
  # Assigning occurrences to train and test by groups
  
  occ_train <- dplyr::filter(bins.data, bins == 1 |bins == 3| bins == 2)[, c("longitude","latitude")]

  occ_test <- dplyr::filter(bins.data, bins == 4)[, c("longitude","latitude")]

  occ_joint <- rbind(occ_train, occ_test)[, c("longitude","latitude")]

  # writing occurrence data

  write.csv(x = occ_train, file = paste0(folder.sp, "/occurrences/occ_train_biomod.csv"), row.names = F)

  write.csv(x = occ_test, file = paste0(folder.sp, "/occurrences/occ_test_biomod.csv"), row.names = F)

  write.csv(x = occ_joint, file = paste0(folder.sp, "/occurrences/occ_joint_biomod.csv"), row.names = F)
  
  #------------------------------------
  # tracking occurrences with id
  
  lonlatTrain <- paste0(occ_train$longitude, occ_train$latitude)
  lonlatTest <- paste0(occ_test$longitude, occ_test$latitude)
  lonlatocc <- paste0(occ.[ , col.lon], occ.[ , col.lat])
  
  indexTrain <- as.numeric()
  for(i in 1:length(lonlatTrain)){
    val1 <- which(lonlatTrain[i] == lonlatocc)
    indexTrain[i] <- val1
  }
  occ_train_id <- occ.[indexTrain, ]
  
  indexTest <- as.numeric()
  for(i in 1:length(lonlatTest)){
    val1 <- which(lonlatTest[i] == lonlatocc)
    indexTest[i] <- val1
  } 
  occ_test_id <- occ.[indexTest, ]
  
  # writing tracking files with id
  write.csv(x = occ_train_id, file = paste0(folder.sp, "/occurrences/occ_trainID.csv"), row.names = F)
  write.csv(x = occ_test_id, file = paste0(folder.sp, "/occurrences/occ_testID.csv"), row.names = F)
  
  # Results
  
  return(list(occ_train = occ_train, occ_test = occ_test, occ_joint = occ_joint))
}
