M_area <- function(polygon.M, raster.M, occ., col.lon, col.lat, folder.sp, dist.Mov,
                   drop.out, MCPbuffer, polygon.select, pointsBuffer) {

  # method 1: accessible area by buffer at points controlled by dist.Mov

  if (polygon.select == FALSE & MCPbuffer == FALSE & pointsBuffer == TRUE) {
    coord <- occ.

    coord_sf <- coord %>%
      dplyr::select(col.lon, col.lat) %>%
      st_as_sf(coords = c(col.lon, col.lat), crs = st_crs("EPSG:4326")) %>%
      st_transform(st_crs("EPSG:4326"))

    # in case of buffer to every occurrence with dist.Mov
    M <- st_buffer(coord_sf, dist.Mov / 120) %>%
      st_union() %>%
      as_Spatial()
    
    raster::shapefile(
      x = M,
      filename = paste0(folder.sp, "/", "shape_M"),
      overwrite = T
    )

    return(result <- (list(shape_M = M, occurrences = coord)))
  }

  # method 2: when you don't want to select your accesibe area trhough features from a bio geographical
  # region, nor dropping outliers by bio geographical frequency in those features. It makes a Minimum
  # convex polygon around the points with a buffer controlled by dist.Mov (in kilometers). It could use cleaned or not cleaned data.

  if (polygon.select == FALSE & MCPbuffer == TRUE) {
    M <- do.MCP(
      dat = occ., collon = col.lon, collat = col.lat,
      distMov = dist.Mov
    )

    coord <- occ.

    raster::shapefile(
      x = M,
      filename = paste0(folder.sp, "/", "shape_M"),
      overwrite = T
    )

    return(result <- (list(shape_M = M, occurrences = coord)))
  }

  # method 3: when you want to select your accessible area through features from a bio geographical
  # region. You need the directory of the file polygon.M and depending of the sub method, a raster layer
  # of it. Sub methods: A. selecting features by intersecting points with a buffer. This buffer
  #                        ensures a full layer without spaces between features.
  #                     B. selecting features by intersecting points without a buffer but
  #                        excluding features with points in a frequency count less than 0.05
  #                        (It is only possible if you use a raster layer of the bio geographical
  #                         shape file to compute frequencies)
  #

  # bio geographical polygon selection
  if (polygon.select == TRUE) {
    if (is.null(polygon.M)) {
      stop("You need a polygon shape file to select features")
    }
    Polygon. <- sf::st_read(polygon.M)

    # convert occurrence data to spatial object
    coord <- occ.

    if (drop.out != "freq") {
      # in case of buffer to every occurrence with dist.Mov

      coord_sf <- coord %>%
        dplyr::select(col.lon, col.lat) %>%
        st_as_sf(coords = c(col.lon, col.lat), crs = st_crs(Polygon.)) %>%
        st_transform(st_crs(Polygon.))
      
      if(pointsBuffer == TRUE){
        coord_buff <- st_buffer(coord_sf, dist.Mov / 120)
        
        # cropping buffer with polygon extension to ensure parsimony
        coord_buff <- st_crop(coord_buff, Polygon.)
        
        # intersecting coordinates with buffer and polygons from shape file, the data is lost
 
        tryCatch(
          exp = {
            M <- Polygon.$geometry[coord_buff] %>% as_Spatial()
            },
            error = function(error_message) {
              stop("points outside polygon")
            }
         )
      }else{
        tryCatch(
          exp = {
            M <- Polygon.$geometry[coord_sf] %>% as_Spatial()
          },
          error = function(error_message) {
            stop("points outside polygon")
          }
        )
      }
    }
    
    if (drop.out == "freq") {
      # sub method B (frequency activate)
      if (is.null(raster.M)) {
        stop("Raster is necesary to compute biogeohraphical frequencies")
      }

      # reading raster of biogeographical shape file to calculate frequencies
      Ras. <- raster::raster(raster.M)

      # extract bio region data and joint it with occurrence dat aset
      occr <- bior_extract(RasPolygon = Ras., data. = occ., collon = col.lon, collat = col.lat)

      # count and frequency for each bio geographic region

      occ.br <- plyr::ddply(occr, "bior", dplyr::mutate, biofreq = length(ID) / nrow(occr))

      # in case of cleaning from frequency table of biogeographical regions

      occ.br <- dplyr::filter(.data = occ.br, biofreq > 0.05) # MISSING let user choice

      # Create expression to filter the bio geographical with more than 5 % of data
      namreg <- as.expression(unique(occ.br$bior))

      coord <- subset(occ.br, select = -c(ID, bior, biofreq))

      # Filtering bio regions for M area
      M <- Polygon. %>%
        dplyr::filter(BIOME_NUM %in% c(namreg)) %>%
        dplyr::select(BIOREG) %>%
        as(Class = "Spatial")
    }
    
    if (MCPbuffer == TRUE) {
      MCPbuf <- do.MCP(
        dat = coord, collon = col.lon, collat = col.lat,
        distMov = dist.Mov
      )
  
      M <- tryCatch(
        exp = {
          intersectsp(x = M, y = MCPbuf, valid = 2)
        },
        error = function(error_message) {
          stop("imposible fixing self intersection")
        }
      )
    }
    # write shape file either bio geographical regions selected (cleaning or not by frequencies) or
    # biogeographical regions selected cutted with MCP
    raster::shapefile(
      x = M,
      filename = paste0(folder.sp, "/", "shape_M"),
      overwrite = T
    )

    return(result <- (list(shape_M = M, occurrences = coord)))
  }

  # in case of want a minimum convex polygon cutting the bio geographical features selected
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

#-------------------------
# force intersect of raster to check and try to buffer by zero distance to repair not valid
# auto intersection

intersectsp <- function(x, y, valid) {
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

  int <- rgeos::gIntersection(x[subsx, ], y[subsy, ],
    byid = TRUE, drop_lower_td = TRUE,
    checkValidity = valid
  )
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
