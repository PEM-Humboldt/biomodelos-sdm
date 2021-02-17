
# The occurrences were divide using the package dismo following the method chess. The size of each
# cell was calculated through the calculated range of a semivariogram. The range specifies from
# what distance a spatial data base reduces its autocorrelation.

dosplit <- function(occ., bias.file, folder.sp, col.lon, col.lat) {
  
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
        size = nrow(bias.file)*0.7,
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
  
  # creating blocks

  bins <- ENMeval::get.block(data., Sbg)
  bins.data <- cbind(data., bins = bins$occ.grp)
  
  # Assigning occurrences to train and test by groups
  
  occ_train <- dplyr::filter(bins.data, bins == 2 |bins == 4| bins == 1)[, c(1,2)]

  occ_test <- dplyr::filter(bins.data, bins == 3)[, c(1,2)]

  occ_joint <- rbind(occ_train, occ_test)[, c(1,2)]

  # writing occurrence data

  write.csv(x = occ_train, file = paste0(folder.sp, "/occurrences/occ_train.csv"), row.names = F)

  write.csv(x = occ_test, file = paste0(folder.sp, "/occurrences/occ_test.csv"), row.names = F)

  write.csv(x = occ_joint, file = paste0(folder.sp, "/occurrences/occ_joint.csv"), row.names = F)

  return(list(occ_train = occ_train, occ_test = occ_test, occ_joint = occ_joint))
}
