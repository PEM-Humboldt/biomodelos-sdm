M_area <- function(polygon.M, raster.M, occ., col.lon, col.lat, folder.sp, dist.Mov,
                   drop.out) {
  Polygon. <- sf::st_read(polygon.M)

  if (drop.out == "freq") {
    if (is.null(raster.M)) {
      stop("Raster is necesary to compute biogeohraphical frequencies")
    }
    # reading bio-geographic data
    Ras. <- raster::raster(raster.M)

    # extract bio region data and joint it with occurrence dat aset
    occr <- bior_extract(RasPolygon = Ras., data. = occ., collon = col.lon, collat = col.lat)

    # count and frequency for each bio geographic region

    occ.br <- plyr::ddply(occr, "bior", dplyr::mutate, biofreq = length(ID) / nrow(occr))
    occ.br <- dplyr::filter(.data = occ.br, biofreq > 0.05)

    # Create expression to filter the bio geographical with more than 5 % of data
    namreg <- as.expression(unique(occ.br$bior))

    # Filtering bioregions for M area
    M <- Polygon. %>%
      dplyr::filter(BIOME_NUM %in% c(namreg)) %>%
      dplyr::select(BIOREG)

    sf::write_sf(M, paste0(folder.sp, "/shape_M.shp"))

    resul <- (list(shape_M = M, occurrences = occ.br[, c(1:3)]))
  }

  if (drop.out == "IQR") {

    # A. Biogeographic units

    crs_polygon <- st_crs(Polygon.)

    # convert data to spatial object
    coord <- occ. %>%
      dplyr::select(col.lon, col.lat) %>%
      st_as_sf(coords = c(col.lon, col.lat), crs = crs_polygon) %>%
      st_transform(crs_polygon)

    # buffer to every ocurrence
    coord_buff <- st_buffer(coord, dist.Mov / 120)

    coord_buff <- st_crop(coord_buff, Polygon.)
    
    # intersecting coordinates with buffer and polgons from shapefile, the data is lost

    tryCatch(
    exp= {bgBio <- Polygon.$geometry[coord_buff] %>% as_Spatial()},
    error = function(error_message) {stop("points buffer outside polygon")}
    )
    # B. Minimun Convex Polygonum (MCP) with a buffer distance associated to distbuf

    bgMCPbuf <- do.MCP(
      dat = occ., collon = col.lon, collat = col.lat,
      distMov = dist.Mov
    )

    # C. intersecting biogeographic units and MCP to retrieve the composed M

    M <- raster::intersect(bgBio, bgMCPbuf)

    # write shapefile

    raster::shapefile(
      x = M,
      filename = paste0(folder.sp, "/", "shape_M"),
      overwrite = T
    )

    resul <- (list(shape_M = M, occurrences = occ.))
  }
  return(resul)
}

#--------------------------
### Function to extract bioregion data and joint it with occurrence dataset

bior_extract <- function(RasPolygon, data., collon, collat) {
  extc <- raster::extract(RasPolygon, data.[, c(collon,collat)], fun = "simple", df = TRUE)
  data.2 <- cbind(data., extc)
  data.2 <- na.omit(data.2)
  as.factor <- data.2[4]
  return(data.2)
}

# In case of malfunction of ddply, mutate and filter in line 12 and 13

# count and frequency for each bio geographic region
# solution with base R
# vector_freq <- c()

# for (i in 1:length(unique(data_br$bior))) {
#  index <- unique(data_br$bior)[i]
#  count_i <- length(which(data_br$bior == index))
#  count_all <- nrow(data_br)
#  freq_bior <- count_i / count_all
#  vector_freq[i] <- freq_bior
# }

# names(vector_freq) <- unique(data_br$bior)

# which regions have a frequency greater than 5%
# vector_select <- vector_freq[vector_freq > 0.05]

# filter occurrences in each selected region
# data_br2 <- data.frame()
# for (i in 1:length(names(vector_select))) {
#  select <- as.numeric(names(vector_select)[i])
#  index <- which(data_br$bior == select)
#  data_br2i <- data_br[index, c(1:3)]
#  data_br2 <- rbind(data_br2, data_br2i)
# }

# Create expression to filter the bio geographical with more than 5 % of data
# namreg <- as.expression(names(vector_select))
