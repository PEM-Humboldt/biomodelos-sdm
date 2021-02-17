clean_rawocc <- function(occ., col.lon, col.lat, spp.col, col.date, date, drop.out, 
                         IQR.mtpl) {

  # Rafael Moreno function
  # Function to clean conflicting, erroneous and duplicate characters
  occ.noChar.uniq <- clean_char_unique(
    data. = occ.,
    sppcol = spp.col,
    collon = col.lon,
    collat = col.lat,
    coldate = col.date
  )
  
  # flagging and removing occurrences failing in political centroids, biod institution coordinates,
  # and in the sea, 0-0 coordinates, and more than +180 or less tan 180 in lon as well +90 or -90
  # latitude

  flags <- CoordinateCleaner::clean_coordinates(
    x = occ.noChar.uniq,
    species = spp.col, 
    lon = col.lon,
    lat = col.lat,
    capitals_rad = 100,
    centroids_rad = 100,
    tests = c("capitals", "centroids", "equal", "gbif", "institutions", "seas", "zeros")
  )

  occ.flaged <- occ.noChar.uniq[flags$.summary, ]
  
  if(drop.out == "IQR"){
    if(nrow(occ.flaged) >= 7){
      # finding outliers and throwing them out
      occ.flaged <- CoordinateCleaner::cc_outl(
        x = occ.flaged,
        lon = col.lon,
        lat = col.lat,
        species = spp.col,
        method = "quantile",
        mltpl = IQR.mtpl # MISSING dejar al usuario la posibilidad de escoger el multiplicador del IQR
      )
    }
  }

  # fitting occurrences date with raster date

  #dates <- occ.flaged[, col.date]
  #index.Date <- which(is.na(dates) | dates < date)
  
  #if(length(index.Date) != 0){
  #  occurrences <- occ.flaged[-index.Date, ]
  #}else{
  #  occurrences <- occ.flaged
  #}
  
  return(occ.flaged)
}

# Rafael Moreno function
# Function to clean conflicting, erroneous and duplicate characters

clean_char_unique <- function(data., sppcol, collon, collat, coldate) {

  # removing conflicting characters in species, longitude, latitude and date columns
  
  data.small <- data.[, c(sppcol, collon, collat, coldate)]
  
  data.small <- completeFun(data.small, c(collon, collat))
  
  data.small[, sppcol] <- gsub("@[>!¿<#?&/\\]", "", data.small[, sppcol])
  data.small[, collon] <- gsub("@[>!¿<#?&/\\]", "", data.small[, collon])
  data.small[, collat] <- gsub("@[>!¿<#?&/\\]", "", data.small[, collat])
  data.small[, coldate] <- gsub("@[>!¿<#?&/\\]", "", data.small[, coldate])

  # making columns to their
  data.small[, collon] <- as.numeric(data.small[, collon])
  data.small[, collat] <- as.numeric(data.small[, collat])
  data.small[, coldate] <- as.Date(data.small[, coldate])
  
  data.small <- duplicatedFun(data = data.small, cols = c(collon, collat))

  return(data.small)
}

#----------------------

completeFun <- function(data, cols) {
  completeVec <- complete.cases(data[, cols])
  return(data[completeVec, ])
}

#----------------------
duplicatedFun <- function(data, cols) {
  dupVec <- duplicated(data[, cols])
 # dupIndex <- which(dupVec == TRUE)
  return(data[!dupVec, ] )
}
