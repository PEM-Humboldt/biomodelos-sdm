inte_areas <- function(polygon.data = polygon_data, raster.data = raster_data, occ. = occ_thin,
                       col.lon = col_lon, col.lat = col_lat, folder.sp = folder_sp, dist.Mov = dist_MOV,
                       drop.out = drop_out, method.M = method_M, method.G = method_G,
                       method.F = method_F, area.G = area_G, area.F = area.F, freq.percent = freq_percent,
                       proj.models = proj_models, do.future = do_future) {

  # freq layer doesn't work to cut as it hast a lot of self intersection errors

  if (method.M == "points_buffer") M <- gen.st.buffer(gen.st.points(dat = occ., collon = col.lon, collat = col.lat), distMov = dist.Mov) %>% as_Spatial()
  if (method.M == "points_MCP") M <- gen.MCP(gen.st.points(dat = occ., collon = col.lon, collat = col.lat)) %>% as_Spatial()
  if (method.M == "points_MCP_buffer") M <- gen.st.buffer(gen.MCP(gen.st.points(dat = occ., collon = col.lon, collat = col.lat)), distMov = dist.Mov) %>% as_Spatial()

  if (grepl(method.M, pattern = "polygon")) {
    if (grepl(method.M, pattern = "polygon_points")) M <- gen.Polygon(gen.st.points(dat = occ., collon = col.lon, collat = col.lat), polygondata = polygon.data, dropout = drop.out, freqperc = freq.percent, rasterdata = raster.data, occurrences = occ) %>% as_Spatial()
    if (grepl(method.M, pattern = "polygon_buffer")) M <- gen.st.buffer(gen.Polygon(gen.st.points(dat = occ., collon = col.lon, collat = col.lat), polygondata = polygon.data, dropout = drop.out, freqperc = freq.percent, rasterdata = raster.data, occurrences = occ), distMov = dist.Mov) %>% as_Spatial()
    if (grepl(method.M, pattern = "polygon_points_buffer")) M <- gen.Polygon(gen.st.buffer(gen_st_points(dat = occ., collon = col.lon, collat = col.lat), distMov = dist.Mov), polygondata = polygon.data, dropout = drop.out, freqperc = freq.percent, rasterdata = raster.data, occurrences = occ) %>% as_Spatial()
    if (grepl(method.M, pattern = "polygon_MCP")) M <- gen.Polygon(gen.MCP(gen.st.points(dat = occ., collon = col.lon, collat = col.lat)), polygondata = polygon.data, dropout = drop.out, freqperc = freq.percent, rasterdata = raster.data, occurrences = occ) %>% as_Spatial()
    if (grepl(method.M, pattern = "cut_buffer")) M <- cut.polygon(M, gen.st.buffer(gen.st.points(dat = occ., collon = col.lon, collat = col.lat)), distMov = dist.Mov) %>% as_Spatial()
    if (grepl(x = method.M, pattern = "cut_MCP")) M <- cut.polygon(M, gen.MCP(gen.st.points(dat = occ., collon = col.lon, collat = col.lat))) %>% as_Spatial()
  }

  if (proj.models == "M-G") {
    if (is.null(area.G)) {
      if (!is.null(method.G)) {
        if (method.G == "points_buffer") G <- gen.st.buffer(gen.st.points(dat = occ., collon = col.lon, collat = col.lat), distMov = dist.Mov)  %>% as_Spatial()
        if (method.G == "points_MCP") G <- gen.MCP(gen.st.points(dat = occ., collon = col.lon, collat = col.lat))  %>% as_Spatial()
        if (method.G == "points_MCP_buffer") G <- gen.st.buffer(gen.MCP(gen.st.points(dat = occ., collon = col.lon, collat = col.lat)), distMov = dist.Mov)  %>% as_Spatial()

        if (grepl(method.G, pattern = "polygon")) {
          if (grepl(method.G, pattern = "polygon_points")) G <- gen.Polygon(gen.st.points(dat = occ., collon = col.lon, collat = col.lat), polygondata = polygon.data, dropout = drop.out, freqperc = freq.percent, rasterdata = raster.data, occurrences = occ)  %>% as_Spatial()
          if (grepl(method.G, pattern = "polygon_buffer")) G <- gen.st.buffer(gen.Polygon(gen.st.points(dat = occ., collon = col.lon, collat = col.lat), polygondata = polygon.data, dropout = drop.out, freqperc = freq.percent, rasterdata = raster.data, occurrences = occ), distMov = dist.Mov)  %>% as_Spatial()
          if (grepl(method.G, pattern = "polygon_points_buffer")) G <- gen.Polygon(gen.st.buffer(gen_st_points(dat = occ., collon = col.lon, collat = col.lat)), polygondata = polygon.data, dropout = drop.out, freqperc = freq.percent, rasterdata = raster.data, occurrences = occ)  %>% as_Spatial()
          if (grepl(method.G, pattern = "polygon_MCP")) G <- gen.Polygon(gen.MCP(gen.st.points(dat = occ., collon = col.lon, collat = col.lat)), polygondata = polygon.data, dropout = drop.out, freqperc = freq.percent, rasterdata = raster.data, occurrences = occ) %>% as_Spatial()
          if (grepl(method.G, pattern = "cut_buffer")) G <- cut.polygon(M, gen.st.buffer(gen.st.points(dat = occ., collon = col.lon, collat = col.lat), distMov = dist.Mov)) %>% as_Spatial()
          if (grepl(x = method.G, pattern = "cut_MCP")) G <- cut.polygon(M, gen.MCP(gen.st.points(dat = occ., collon = col.lon, collat = col.lat)))  %>% as_Spatial()
        }
      } else {
        stop("Provide a method for G area")
      }
    } else {
      try(
        exp = {
          finalstr <- tail(unlist(strsplit(area.G, "\\.")), n = 1)
          if (finalstr == "shp") {
            G <- raster::shapefile(area.G)
          } else {
            G <- raster::raster(area.G)
          }
        }
      )
      if (!exists("F.")) stop("if you do not provide a method for F at least provide a valid raster/shape path file ")
    }
  }


  if (do.future == TRUE) {
    if (is.null(area.F)) {
      if (!is.null(method.F)) {
        if (method.F == "points_buffer") F. <- gen.st.buffer(gen.st.points(dat = occ., collon = col.lon, collat = col.lat), distMov = dist.Mov) %>% as_Spatial()
        if (method.F == "points_MCP") F. <- gen.MCP(gen.st.points(dat = occ., collon = col.lon, collat = col.lat)) %>% as_Spatial()
        if (method.F == "points_MCP_buffer") F. <- gen.st.buffer(gen.MCP(gen.st.points(dat = occ., collon = col.lon, collat = col.lat)), distMov = dist.Mov) %>% as_Spatial()

        if (grepl(method.F, pattern = "polygon")) {
          if (grepl(method.F, pattern = "polygon_points")) F. <- gen.Polygon(gen.st.points(dat = occ., collon = col.lon, collat = col.lat), polygondata = polygon.data, dropout = drop.out, freqperc = freq.percent, rasterdata = raster.data, occurrences = occ) %>% as_Spatial()
          if (grepl(method.F, pattern = "polygon_buffer")) F. <- gen.st.buffer(gen.Polygon(gen.st.points(dat = occ., collon = col.lon, collat = col.lat), polygondata = polygon.data, dropout = drop.out, freqperc = freq.percent, rasterdata = raster.data, occurrences = occ), distMov = dist.Mov) %>% as_Spatial()
          if (grepl(method.F, pattern = "polygon_points_buffer")) F. <- gen.Polygon(gen.st.buffer(gen_st_points(dat = occ., collon = col.lon, collat = col.lat), distMov = dist.Mov), polygondata = polygon.data, dropout = drop.out, freqperc = freq.percent, rasterdata = raster.data, occurrences = occ) %>% as_Spatial()
          if (grepl(method.F, pattern = "polygon_MCP")) F. <- gen.Polygon(gen.MCP(gen.st.points(dat = occ., collon = col.lon, collat = col.lat)), polygondata = polygon.data, dropout = drop.out, freqperc = freq.percent, rasterdata = raster.data, occurrences = occ) %>% as_Spatial()
          if (grepl(method.F, pattern = "cut_buffer")) F. <- cut.polygon(M, gen.st.buffer(gen.st.points(dat = occ., collon = col.lon, collat = col.lat), distMov = dist.Mov)) %>% as_Spatial()
          if (grepl(x = method.F, pattern = "cut_MCP")) F. <- cut.polygon(M, gen.MCP(gen.st.points(dat = occ., collon = col.lon, collat = col.lat))) %>% as_Spatial()
        }
      } else {
        if (proj.models == "M-M") F. <- M
        if (proj.models == "M-G") F. <- G
      }
    } else {
      try(
        exp = {
          finalstr <- tail(unlist(strsplit(area.F, "\\.")), n = 1)
          if (finalstr == "shp") {
            F. <- raster::shapefile(area.F)
          } else {
            F. <- raster::raster(area.F)
          }
        }
      )
      if (!exists("F.")) stop("if you do not provide a method for F at least provide a valid raster/shape path file ")
    }
  }
  
  if (proj.models == "M-M" & do.future == F) res <- list(shape_M = M, shape_G = NULL, shape_F = NULL, occurrences = occ.)
  if (proj.models == "M-M" & do.future == T) res <- list(shape_M = M, shape_G = NULL, shape_F = F., occurrences = occ.)

  if (proj.models == "M-G" & do.future == F) res <- list(shape_M = M, shape_G = G, shape_F = NULL, occurrences = occ.)
  if (proj.models == "M-G" & do.future == T) res <- list(shape_M = M, shape_G = G, shape_F = F., occurrences = occ.)

  dir.create(paste0(folder.sp, "/", "interest_areas"), showWarnings = F)
  
  for (i in 1:length(res)) {
     resi <- res[[i]]
     namesresi <- names(res[i])
     if (!is.null(resi)) {
       if (!is.data.frame(resi)) {
         if(class(resi)[1] == "RasterLayer"){
           raster::writeRaster(resi, paste0(folder.sp, "/", "interest_areas", "/", namesresi, ".tif"), overwrite = T, NAflag = -9999, datatype = "INT2S", options = "COMPRESS=LZW")
         }else{
           raster::shapefile(x = resi, filename = paste0(folder.sp, "/", "interest_areas", "/", namesresi), overwrite = T)
         }
       }
     }
  }

  return(res)
}

#--------------------------------
# Generate points

gen.st.points <- function(dat, collon = col.lon, collat = col.lat) {
  st.points <- dat %>%
    dplyr::select(collon, collat) %>%
    st_as_sf(coords = c(collon, collat), crs = st_crs("EPSG:4326")) %>%
    st_transform(st_crs("EPSG:4326"))
}

#--------------------------------
# Generate buffer around sf object

gen.st.buffer <- function(stobject, distMov = dist.Mov) {
  st_buffer(stobject, distMov / 120) %>%
    st_union()
}

#--------------------------------
# Minimum convex polygon

gen.MCP <- function(stobject) {
  st_convex_hull(st_union(stobject))
}

#--------------------------------
# Generate Polygon

gen.Polygon <- function(stobject, polygondata = polygon.data, dropout = drop.out,
                        freqperc = freq.percent, rasterdata = raster.data,
                        occurrences = occ.) {
  Polygon. <- sf::st_read(polygondata)

  if (dropout != "freq") {
    if (unique(sf::st_geometry_type(stobject)) == "POINT") {
      pntpolygon <- Polygon.$geometry[stobject] %>%
        st_union()
      return(shapes = pntpolygon)
    }

    if (unique(sf::st_geometry_type(stobject)) == "MULTIPOLYGON" |
      unique(sf::st_geometry_type(stobject)) == "POLYGON") {

      # cropping buffer with polygon extension to ensure parsimony
      coord_buff <- sf::st_crop(stobject, Polygon.)

      tryCatch(
        exp = {
          bffrpolygon <- Polygon.$geometry[coord_buff] %>%
            st_union()
        },
        error = function(error_message) {
          stop("points outside polygon")
        }
      )
      return(shapes = bffrpolygon)
    }
  }

  if (dropout == "freq") {
    if (freq.percent > 0) {
      if (!is.null(raster.data)) {
        if (unique(sf::st_geometry_type(stobject)) == "POINT") {
          # reading raster of biogeographical shape file to calculate frequencies
          Ras. <- raster::raster(rasterdata)
          stobject_coord <- st_coordinates(stobject)
          # extract bio region data and joint it with occurrence dat aset
          occr <- bior.extract(RasPolygon = Ras., data. = stobject_coord, collon = "X", collat = "Y")
          # count and frequency for each bio geographic region
          occ.br <- plyr::ddply(occr, "bior", dplyr::mutate, biofreq = length(ID) / nrow(occr))
          # in case of cleaning from frequency table of biogeographical regions
          occ.br <- dplyr::filter(.data = occ.br, biofreq > (freqperc / 100)) # MISSING let user choice
          # Create expression to filter the bio geographical with more than freq.percent
          namreg <- as.expression(unique(occ.br$bior))

          new.occ. <- subset(occ.br, select = -c(ID, bior, biofreq))

          # Filtering bio regions
          new.regions <- Polygon. %>%
            dplyr::filter(BIOME_NUM %in% c(namreg)) %>%
            dplyr::select(BIOREG) %>%
            st_union()

          return(shapes = new.regions)
        }
      } else {
        stop("Provide a rasterized layer of the polygons to compute frequencies")
      }
    } else {
      stop("Provide a frequency percentege of occurrences to remove under-represented regions")
    }
  }
}

#--------------------------------
# Cut polygon

cut.polygon <- function(polygon.Gen, obj.withCut) {
  polygon.Gen <- polygon.Gen %>%
    st_union() %>%
    as_Spatial()
  obj.withCut <- obj.withCut %>%
    st_union() %>%
    as_Spatial()

  tryCatch(
    exp = {
      polygon.cutted <- intersect.sp(x = polygon.Gen, y = obj.withCut, valid = 2)
    },
    error = function(error_message) {
      stop("imposible fixing self intersection")
    }
  )
  return(polygon.cutted)
}

#--------------------------------
### Function to extract bioregion data and joint it with occurrence dataset

bior.extract <- function(RasPolygon, data., collon, collat) {
  extc <- raster::extract(RasPolygon, data.[, c(collon, collat)], fun = "simple", df = TRUE)
  data.2 <- cbind(data., extc)
  data.2 <- na.omit(data.2)
  as.factor <- data.2[4]
  return(data.2)
}

#--------------------------------
# force intersect of spatial objects to check and try to buffer by zero distance to repair not valid
# auto intersection

intersect.sp <- function(x, y, valid) {
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

