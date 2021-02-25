M_area <- function(polygon.M, raster.M, occ., col.lon, col.lat, folder.sp, dist.Mov,
                   drop.out, do.clean) {
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

    if (do.clean == TRUE) {
      occ.br <- dplyr::filter(.data = occ.br, biofreq > 0.05) # MISSING let user choice
    }

    # Create expression to filter the bio geographical with more than 5 % of data
    namreg <- as.expression(unique(occ.br$bior))

    # Filtering bioregions for M area
    M <- Polygon. %>%
      dplyr::filter(BIOME_NUM %in% c(namreg)) %>%
      dplyr::select(BIOREG)

    sf::write_sf(M, paste0(folder.sp, "/shape_M.shp"))

    resul <- (list(shape_M = M, occurrences = subset(occ.br, select = -c(ID, bior, biofreq))))
  }

  if (drop.out == "IQR" | drop.out == "any") {

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
      exp = {
        bgBio <- Polygon.$geometry[coord_buff] %>% as_Spatial()
      },
      error = function(error_message) {
        stop("points buffer outside polygon")
      }
    )
    # B. Minimun Convex Polygonum (MCP) with a buffer distance associated to distbuf

    bgMCPbuf <- do.MCP(
      dat = occ., collon = col.lon, collat = col.lat,
      distMov = dist.Mov
    )

    # C. intersecting biogeographic units and MCP to retrieve the composed M

    M <- intersectsp(bgBio, bgMCPbuf)

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
  extc <- raster::extract(RasPolygon, data.[, c(collon, collat)], fun = "simple", df = TRUE)
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

#-------------------------
# force intersect of raster to check and try to buffer by zero distance to repair not valid
# auto intersection

intersectsp <- function(x, y) {
  requireNamespace("rgeos")

  prj <- x@proj4string
  if (is.na(prj)) prj <- y@proj4string
  x@proj4string <- CRS(as.character(NA))
  y@proj4string <- CRS(as.character(NA))

  # 	threshold <- get_RGEOS_polyThreshold()
  # 	on.exit(set_RGEOS_polyThreshold(threshold))
  # 	minarea <- min(apply(bbox(union(extent(x), extent(y))), 1, diff) / 1000000, 0.00001)
  # 	set_RGEOS_polyThreshold(minarea)
  # 	slivers <- get_RGEOS_dropSlivers()
  # 	on.exit(set_RGEOS_dropSlivers(slivers))
  # 	set_RGEOS_dropSlivers(TRUE)


  x <- spChFIDs(x, as.character(1:length(x)))
  y <- spChFIDs(y, as.character(1:length(y)))

  subs <- rgeos::gIntersects(x, y, byid = TRUE)
  if (sum(subs) == 0) {
    warning("polygons do not intersect")
    return(NULL)
  }

  xdata <- .hasSlot(x, "data")
  ydata <- .hasSlot(y, "data")
  dat <- NULL
  if (xdata & ydata) {
    nms <- .goodNames(c(colnames(x@data), colnames(y@data)))
    colnames(x@data) <- xnames <- nms[1:ncol(x@data)]
    colnames(y@data) <- ynames <- nms[(ncol(x@data) + 1):length(nms)]
    dat <- cbind(x@data[NULL, , drop = FALSE], y@data[NULL, , drop = FALSE])
  } else if (xdata) {
    dat <- x@data[NULL, , drop = FALSE]
    xnames <- colnames(dat)
  } else if (ydata) {
    dat <- y@data[NULL, , drop = FALSE]
    ynames <- colnames(dat)
  }

  subsx <- apply(subs, 2, any)
  subsy <- apply(subs, 1, any)

  int <- rgeos::gIntersection(x[subsx, ], y[subsy, ], byid = TRUE, drop_lower_td = TRUE, 
                              checkValidity = 2)
  # 	if (inherits(int, "SpatialCollections")) {
  # 		if (is.null(int@polyobj)) { # merely touching, no intersection
  # 			#warning('polygons do not intersect')
  # 			return(NULL)
  # 		}
  # 		int <- int@polyobj
  # 	}
  if (!inherits(int, "SpatialPolygons")) {
    # warning('polygons do not intersect')
    return(NULL)
  }

  if (!is.null(dat)) {
    ids <- do.call(rbind, strsplit(row.names(int), " "))
    rows <- 1:length(ids[, 1])
    if (xdata) {
      idsx <- match(ids[, 1], rownames(x@data))
      dat[rows, xnames] <- x@data[idsx, ]
    }
    if (ydata) {
      idsy <- match(ids[, 2], rownames(y@data))
      dat[rows, ynames] <- y@data[idsy, ]
    }
    rownames(dat) <- 1:nrow(dat)
    int <- spChFIDs(int, as.character(1:nrow(dat)))
    int <- SpatialPolygonsDataFrame(int, dat)
  }

  if (length(int) > 0) {
    w <- getOption("warn")
    on.exit(options("warn" = w))
    options("warn" = -1)

    j <- rgeos::gIsValid(int, byid = TRUE, reason = FALSE)
    if (!all(j)) {
      bad <- which(!j)
      for (i in bad) {
        # it could be that a part of a polygon is a sliver, but that other parts are OK
        a <- disaggregate(int[i, ])
        if (length(a) > 1) {
          jj <- which(rgeos::gIsValid(a, byid = TRUE, reason = FALSE))
          a <- a[jj, ]
          if (length(a) > 0) {
            int@polygons[i] <- aggregate(a)@polygons
            j[i] <- TRUE
          }
        }
      }
      int <- int[j, ]
    }
  }
  int@proj4string <- prj
  int
}
