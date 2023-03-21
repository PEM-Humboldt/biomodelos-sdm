#' The `format_occ_data` function takes the input occurrence data `occ.` and column names for longitude, latitude, 
#' and species. It calls the `clean_occ_chars` function to remove conflicting, erroneous, and duplicate characters 
#' from the data. The formatted data is returned.
#'
#' @param occ. data.frame of occurrence data with column names including at least longitude, latitude and species
#' name.
#' @param col.lon character string.
#' @param col.lat character string
#' @param spp.col character string
#' @return data.frame of occurrence with species, longitude and latitude columns only

format_occ_data <- function(occ. = occ, col.lon = col_lon, col.lat = col_lat,
                          spp.col = col_sp) {

  # Function to clean conflicting, erroneous and duplicate characters
  occ. <- clean_occ_chars(
    col.id = "occ.ID",
    data. = occ.,
    sppcol = spp.col,
    collon = col.lon,
    collat = col.lat
  )

  return(occ.)
}

# Rafael Moreno function / 2021
# Function to clean conflicting, erroneous and duplicate characters

clean_occ_chars <- function(col.id, data., sppcol, collon, collat) { # , coldate

  # removing conflicting characters in species, longitude, latitude
  # and date columns

  # Selecting the columns for the ID, species, longitude, and latitude
  data.small <- data.[, c(col.id, sppcol, collon, collat)] # , coldate
  
  # Removing rows with missing data for the longitude and latitude columns
  data.small <- complete_fun(data.small, c(collon, collat))

  data.small[, sppcol] <- gsub("@[>!¿<#?&/\\]", "", data.small[, sppcol])
  data.small[, collon] <- gsub("@[>!¿<#?&/\\]", "", data.small[, collon])
  data.small[, collat] <- gsub("@[>!¿<#?&/\\]", "", data.small[, collat])

  # Removing special characters from the species, longitude, and latitude columns
  data.small[, collon] <- as.numeric(data.small[, collon])
  data.small[, collat] <- as.numeric(data.small[, collat])

  # Removing duplicated rows based on longitude and latitude columns
  data.small <- duplicated_fun(data = data.small, cols = c(collon, collat))

  return(data.small)
}

#----------------------

complete_fun <- function(data, cols) {
  completeVec <- complete.cases(data[, cols])
  return(data[completeVec, ])
}

#----------------------
duplicated_fun <- function(data, cols) {
  dupVec <- duplicated(data[, cols])
  # dupIndex <- which(dupVec == TRUE)
  return(data[!dupVec, ])
}