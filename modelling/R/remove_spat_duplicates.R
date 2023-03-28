#' Function to remove spatial duplicates from occurrence data base
#' 
#' @description  The `remove_spat_duplicates` function is designed to remove duplicate occurrences of species from a 
#' data frame. The function offers two methods for removing duplicates: (1) a spatial thinning approach using the 
#' spThin package, and (2) a distance-based approach using a distance threshold. The function returns a data frame 
#' with unique occurrences, based on the chosen method and distance threshold.
#'
#' @param occ. data frame containing occurrence data with columns for longitude, latitude, and species identification
#' @param col.lon character string column name for the longitude variable
#' @param col.lat character string column name for the latitude variable
#' @param spp.col character string column name for the species identification variable
#' @param sp.name character string species name (default is sp_name)
#' @param remove.method character string indicating the method for removing duplicate occurrences. Two options:
#' "sqkm" uses clean_dup function from ntbox package by Luis Osorio 
#' https://github.com/luismurao/ntbox/blob/master/R/clean_dup.R or "spthin" uses thin function from package sp_thin
#' https://rdrr.io/cran/spThin/man/thin.html
#' @param remove.distance: a numeric value indicating the distance threshold in kilometers for considering 
#' occurrences as duplicates 
#' 
#' @return data frame with unique occurrences, based on the specified method and distance threshold.

remove_spat_duplicates <- function(occ., col.lon = col_lon, col.lat = col_lat, sp.col = col_sp,
                                   sp.name = sp_name, remove.method, remove.distance) {
  
  if (is.null(remove.method) == T) {
    dropUniq <- occ.
  } else {
    if (remove.method == "sqkm") {
      dropUniq <- clean_dup(
        data = occ., longitude = col.lon, latitude = col.lat, threshold = remove.distance / 120
      )
    }

    if (remove.method == "spthin") {
      thinned <- spThin::thin(
        loc.data = occ., lat.col = col.lat, long.col = col.lon, spec.col = sp.col, thin.par = remove.distance,
        reps = 20, verbose = TRUE, locs.thinned.list.return = TRUE, write.files = FALSE
        # thin.par parameter should be setting according to species natural history
      )

      thindf <- thinned[[1]]
      thindf[, sp.col] <- rep(sp.name, nrow(thindf))

      lonlatThinned <- paste0(thindf$Longitude, thindf$Latitude)

      lonlatnothinned <- paste0(occ.[, col.lon], occ.[, col.lat])

      indexV <- as.numeric()

      for (i in 1:length(lonlatThinned)) {
        val1 <- which(lonlatThinned[i] == lonlatnothinned)
        indexV[i] <- val1
      }

      dropUniq <- occ.[indexV, ]
    }
  }

  return(dropUniq)
}

#--------------------------
#' Function to clean duplicated longitude and latitude data
#' @description Clean duplicated longitude and latitude data using threshold distance
#'              which is a distance (in grades) between points to be considered
#'              duplicates.
#' @param data A data.frame with longitude and latitude data
#' @param longitude A character vector of the column name of longitude.
#' @param latitude A character vector of the column name of latitude.
#' @param threshold A numeric value representing the Euclidean distance between coordinates
#'            to be considered a duplicate.
#' @return Returns a data.frame with coordinate data from species
#' @export
#' @examples
#' # Species genus
#' genus <- "ambystoma"
#' # Species name
#' species <- "tigrinum"
#' \dontrun{
#' # GBIF search
#' ambystoma_tigrinum <- searh_gbif_data(genus, species,
#'   occlim = 100,
#'   writeFile = FALSE
#' )
#' ambystoma_tigrinum_clean <- clean_dup(ambystoma_tigrinum,
#'   longitude = "longitude",
#'   latitude = "latitude",
#'   threshold = 0.1666
#' )
#' # Check the dimensions of  data
#' dim(ambystoma_tigrinum)
#' dim(ambystoma_tigrinum_clean)
#' }
#'
#' acknowldgement: Luis Osorio NicheToolBox https://github.com/luismurao/ntbox/blob/master/R/clean_dup.R
clean_dup <- function(data, longitude, latitude, threshold = 0.0) {
  data <- data[!is.na(data[, longitude]), ]
  dat_sp <- sp::SpatialPointsDataFrame(data[, c(
    longitude,
    latitude
  )], data)
  dat_sp1 <- sp::remove.duplicates(dat_sp, zero = threshold)
  return(dat_sp1@data)
}
