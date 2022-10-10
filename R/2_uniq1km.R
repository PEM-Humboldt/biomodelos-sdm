do.uniq1km <- function(occ., col.lon = col_lon, col.lat = col_lat, sp.col = col_sp,
                       sp.name = sp_name, uniq1k.method, uniqDist) {
  if (is.null(uniq1k.method)) {
    dropUniq <- occ.
  } else {
    if (uniq1k.method == "sqkm") {
      dropUniq <- clean_dup(
        data = occ., longitude = col.lon, latitude = col.lat, threshold = uniqDist / 120 # missing let user choice
      )
    }

    if (uniq1k.method == "spthin") {
      thinned <- spThin::thin(
        loc.data = occ., lat.col = col.lat, long.col = col.lon, spec.col = sp.col, thin.par = uniqDist,
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

  write.csv(dropUniq, paste0(folder.sp, "/occurrences/occ_thin.csv"), row.names = F)
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
clean_dup <- function(data, longitude, latitude, threshold = 0.0) {
  data <- data[!is.na(data[, longitude]), ]
  dat_sp <- sp::SpatialPointsDataFrame(data[, c(
    longitude,
    latitude
  )], data)
  dat_sp1 <- sp::remove.duplicates(dat_sp, zero = threshold)
  return(dat_sp1@data)
}
