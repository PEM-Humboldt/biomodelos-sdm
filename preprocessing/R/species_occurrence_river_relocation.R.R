# Spatial Relocation of Species Occurrences Around Aquatic Basins Using Raster Analysis

# This R script facilitates the spatial relocation of species occurrence records around 
# aquatic basins using raster analysis. It integrates CSV data with raster templates to 
# adjust species locations based on defined buffer distances from water bodies, ensuring 
# accurate spatial representation for ecological studies and conservation efforts.

# Packages
library(data.table)  
library(sf)          
library(terra)      
library(dplyr)     

# Reading CSV files
# Requires a list of CSV files or tables

files <- list.files("temp/", ".csv", full.names = TRUE)
csvs <- lapply(files, function(file) read.csv(file))

# Raster template for rivers, can be any variable from the set of variables
raster_rivers <- rast("a.tif")

# `moving_occurrences` is a function that relocates species occurrence records
# situated around aquatic basins. It can move points from any distance to the river. 
# However, a record that is not in the adjacent pixel of the river might belong to 
# a very different basin, so it should be considered whether it is worth moving 
# pixels to a distance that is more than 1 km away.
#
# `csvData` = data.frame, with columns "acceptedNameUsage", "decimalLongitude", "decimalLatitude"
# `rastData` = raster, template of location of the aquatic basins
# `dirout` = character vector, directory where the CSV with the moved records will be saved
# `distBuf` = numeric vector, maximum distance in meters of the basin from which 
# records will be allowed to move
#
# Returns = data.frame, with records in their new location with the following columns:
# "acceptedNameUsage": character vector, species name
# "decimalLatitude": numeric vector, latitude of the record
# "decimalLongitude": numeric vector, longitude of the record
# "nodat": logical, was the record in a location with information from the 
# basin template?
# "indist": logical, was the record within the allowed distance for moving records?
# "moved": logical, was the record moved?
#
# Example:
#
# acceptedNameUsage         decimalLatitude    decimalLongitude   nodat   indist   moved
# Chelonoidis denticulatus   8.0875             -61.84583333      FALSE   NA       NA
# Chelonoidis denticulatus   8.0875             -61.84583333      FALSE   NA       NA
# Chelonoidis denticulatus   7.3706             -60.4911          FALSE   NA       NA

moving_occurrences <- function(csvData, rastData, dirout, distBuf) {
  csv_i <- csvData
  print(csv_i[1, "acceptedNameUsage"])
  csv_i$nodat <- NA
  csv_i$indist <- NA
  csv_i$moved <- NA
  
  for (a in 1:nrow(csv_i)) {
    print(a)
    dat_a <- csv_i[a, c("decimalLongitude", "decimalLatitude")]
    if (is.na(extract(rastData, dat_a)[, 2])) {
      dat_spat <- dat_a %>% st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), 
                                     crs = crs(rastData))
      dat_buffer <- dat_spat %>% st_buffer(dist = distBuf) %>% vect()
      tryCatch({
        coords <- rastData %>% crop(dat_buffer) %>% mask(dat_buffer) %>% crds(df = TRUE, na.rm = FALSE)
      }, error = function(e) {
        return(NA)
      })
      buffer_extract <- extract(rastData, coords)
      index_1 <- which(buffer_extract[, 2] == 1)
      if (length(index_1) != 0) {
        coords_1 <- coords[index_1, ]
        coords_1_spat <- coords_1 %>% st_as_sf(coords = c("x", "y"), 
                                               crs = crs(rastData))
        dist <- st_distance(dat_spat, coords_1_spat)
        new_coords <- coords_1[which.min(dist), ]
        csv_i[a, c("decimalLongitude", "decimalLatitude")] <- new_coords
        csv_i[a, "indist"] <- TRUE
        csv_i[a, "moved"] <- TRUE
      } else {
        csv_i[a, "indist"] <- FALSE
        csv_i[a, "moved"] <- FALSE
      }
      
      csv_i[a, "nodat"] <- TRUE
      
    } else {
      csv_i[a, "nodat"] <- FALSE
    }
  }
  dir.create(file.path(dirout, "moved"), showWarnings = FALSE)
  write.csv(csv_i, file.path(dirout, "moved", paste0(csv_i[1, "acceptedNameUsage"], "_", 
                                                    distBuf, "m_mov.csv")), row.names = FALSE)
  
  return(csv_i)
}

# Single species
moving_occurrences(csvData = csvs[[4]], rastData = raster_rivers, dirout = "temp/", distBuf = 2000)

# Multiple species
lapply(X = csvs[acuaticas], FUN = function(X) {
  moving_occurrences(csvData = X, rastData = raster_rivers, dirout = "temp/", distBuf = 1000)
})
