#' Generate areas of interest for ecological niche models, projections, and future scenarios. 
#' 
#' @description  The `define_interest_areas` function constructs areas of interest for ecological
#' niche modelling. It can define M area or accessible area in which algorithm is train, G area in which is
#' possible project the trained model in current time or same as M, and F area in which is project 
#' the model in different scenarios. The shape files of M, G and F are returned and written in the species
#' folder.
#'
#' @param occ. data frame with occurrence data, by default occ_thin.
#' @param col.lon character string, column name for longitude coordinates in `occ.`, by default col_lon.
#' @param col.lat character string, column name for latitude coordinates in `occ.`, by default col_lat.
#' @param folder.sp  character string, folder path to save polygon shapefiles to use as masks, by default folder_sp.
#' @param dist.Mov  numeric, maximum distance in kilometers to make buffer.
#' @param method.M  character string, method to define the area to train ecological niche models.
#' @param method.G  character string, method to define the area to project with current climate. It is used if
#' compute.G is set to TRUE and proj.models is "M-G". 
#' @param method.F  character string, method to define the area to project with future climate. It is used if
#' do.future and compute.F are set to TRUE
#' @param area.M  character string, file path to the raster or shapefile defining the M area, in case of not
#' using any optional methods
#' @param area.G  character string, file path to the raster or shapefile defining the G area, in case of not
#' using any optional methods. It is used if compute.G is set to TRUE and proj.models is "M-G".
#' @param area.F  character string, file path to the raster or shapefile defining the F area, in case of not
#' using any default methods. It is used if do.future and compute.F are set to TRUE
#' @param proj.models  character string, set to "M-G" to generate G area. By default, "M-M" no area is generated in G.
#' @param do.future logical, set to `TRUE` to generate F area. By default, set to `FALSE`.
#' @param compute.F logical, set to `TRUE` to compute F area.
#' @param polygon.data character string, file path to shapefile in order to extract areas of interest
#' 
#' @details 
#' The method.M, method.G and method.F are parameters defined by character string that specifies the method to 
#' define each interest area.  The method parameter can take the following values: 
#' "points_buffer": This method generates a buffer around the occurrence points (given by dist.Mov). 
#' "points_MCP": This method generates a minimum convex polygon around the occurrence points. 
#' "points_MCP_buffer": This method generates a minimum convex polygon around the occurrence points, applies a buffer
#' of a specified distance (given by dist.Mov) around the polygon, and then clips the polygon to the study area boundary.
#' "polygon_points": This method intersects a biogeographic area multi-polygon (given by polygon.data) with the 
#' occurrence points to create a new polygon that covers the study area and contains all occurrence points.
#' "polygon_buffer": This method intersects a biogeographic area multi-polygon (given by polygon.data) with the 
#' occurrence points and then create a buffer of a specified distance (given by dist.Mov) around the selected polygon
#' to create a new polygon that covers the study area and contains all occurrence points and a buffer.
#' "polygon_points_buffer": This method intersects a biogeographic area multi-polygon (given by polygon.data) with 
#' a buffer of a specified distance (given by dist.Mov) around the occurrence points, then clips the resulting polygon 
#' to the study area boundary. 
#' "polygon_MCP": This method intersects a study area polygon (given by polygon.data) with a minimum convex polygon 
#' around the occurrence points to create a new polygon that covers the study area and contains all occurrence points.
#' 
#' return 
define_interest_areas <- function(occ. = occ_thin,
                       col.lon = col_lon, col.lat = col_lat, folder.sp = folder_sp, dist.Mov = dist_MOV,
                       method.M = method_M, method.G = method_G, method.F = method_F, area.M = area_M,
                       area.G = area_G, area.F = area.F, proj.models = proj_models,
                       do.future = do_future, compute.F = compute_F, polygon.data = polygon_data
                       ) {
  # setup simple features
  sf::sf_use_s2(FALSE)
  
  # freq layer doesn't work to cut as it hast a lot of self intersection errors
  if (is.null(area.M)) {
    if (!is.null(method.M)) {
      if (method.M == "points_buffer") M <- gen.points.buffer(occ., col.lon, col.lat, dist.Mov)
      if (method.M == "points_MCP") M <- gen.points.MCP(occ., col.lon, col.lat)
      if (method.M == "points_MCP_buffer") M <- gen.points.MCP.buffer(occ., col.lon, col.lat, dist.Mov)

      if (grepl(method.M, pattern = "polygon")) {
        if (grepl(method.M, pattern = "polygon_points")) M <- intersect.polygon.points(occ., col.lon, col.lat, polygon.data)
        if (grepl(method.M, pattern = "polygon_buffer")) M <- intersect.polygonBuffer(occ., col.lon, col.lat, polygon.data, dist.Mov)
        if (grepl(method.M, pattern = "polygon_points_buffer")) M <- intersect.polygon.pointsBuffer(occ., col.lon, col.lat, polygon.data, dist.Mov)
        if (grepl(method.M, pattern = "polygon_MCP")) M <- intersect.polygon.MCP(occ., col.lon, col.lat, polygon.data)
      }
      M <- vect(M)
    } else {
      stop("Provide a method for M area")
    }
  } else {
    try(
      exp = {
        finalstr <- tail(unlist(strsplit(area.M, "\\.")), n = 1)
        if (finalstr == "shp") {
          M <- terra::vect(area.M)
        } else {
          M <- terra::rast(area.M)
        }
      }
    )
    if (!exists("M")) stop("if you do not provide a method for M at least provide a valid raster/shape path file ")
  }

  if (proj.models == "M-G") {
    if (is.null(area.G)) {
      if (!is.null(method.G)) {
        if (method.G == "points_buffer") G <- gen.points.buffer(occ., col.lon, col.lat, dist.Mov)
        if (method.G == "points_MCP") G <- gen.points.MCP(occ., col.lon, col.lat)
        if (method.G == "points_MCP_buffer") G <- gen.points.MCP.buffer(occ., col.lon, col.lat, dist.Mov)

        if (grepl(method.G, pattern = "polygon")) {
          if (grepl(method.G, pattern = "polygon_points")) G <- intersect.polygon.points(occ., col.lon, col.lat, polygon.data)
          if (grepl(method.G, pattern = "polygon_buffer")) G <- intersect.polygonBuffer(occ., col.lon, col.lat, polygon.data, dist.Mov)
          if (grepl(method.G, pattern = "polygon_points_buffer")) G <- intersect.polygon.pointsBuffer(occ., col.lon, col.lat, polygon.data, dist.Mov)
          if (grepl(method.G, pattern = "polygon_MCP")) G <- intersect.polygon.MCP(occ., col.lon, col.lat, polygon.data)
        }
        G <- vect(G)
      } else {
        stop("Provide a method for G area")
      }
    } else {
      try(
        exp = {
          finalstr <- tail(unlist(strsplit(area.G, "\\.")), n = 1)
          if (finalstr == "shp") {
            G <- terra::vect(area.G)
          } else {
            G <- terra::rast(area.G)
          }
        }
      )
      if (!exists("G")) stop("if you do not provide a method for G at least provide a valid raster/shape path file ")
    }
  }


  if (do.future == TRUE) {
    if (compute.F == TRUE) {
      if (is.null(area.F)) {
        if (!is.null(method.F)) {
          if (method.F == "points_buffer") F. <- gen.points.buffer(occ., col.lon, col.lat, dist.Mov)
          if (method.F == "points_MCP") F. <- gen.points.MCP(occ., col.lon, col.lat)
          if (method.F == "points_MCP_buffer") F. <- gen.points.MCP.buffer(occ., col.lon, col.lat, dist.Mov)

          if (grepl(method.F, pattern = "polygon")) {
            if (grepl(method.F, pattern = "polygon_points")) F. <- intersect.polygon.points(occ., col.lon, col.lat, polygon.data)
            if (grepl(method.F, pattern = "polygon_buffer")) F. <- intersect.polygonBuffer(occ., col.lon, col.lat, polygon.data, dist.Mov)
            if (grepl(method.F, pattern = "polygon_points_buffer")) F. <- intersect.polygon.pointsBuffer(occ., col.lon, col.lat, polygon.data, dist.Mov)
            if (grepl(method.F, pattern = "polygon_MCP")) F. <- intersect.polygon.MCP(occ., col.lon, col.lat, polygon.data)
          }
          F <- vect(F)
        } else {
          if (proj.models == "M-M") F. <- M
          if (proj.models == "M-G") F. <- G
        }
      } else {
        finalstr <- tail(unlist(strsplit(area.F, "\\.")), n = 1)
        if (finalstr == "shp") F. <- terra::vect(area.F)
        if (finalstr == "tif") F. <- terra::rast(area.F)
        if (!exists("F.")) stop("if you do not provide a method for F at least provide a valid raster/shape path file ")
      }
    } else {
      F. <- NULL
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
        if (class(resi)[1] == "RasterLayer") {
          terra::writeRaster(resi, paste0(folder.sp, "/", "interest_areas", "/", namesresi, ".tif"), overwrite = T, NAflag = -9999, datatype = "INT2S")
        } else {
          terra::writeVector(x = resi, filename = paste0(folder.sp, "/", "interest_areas", "/", namesresi, ".shp"), overwrite = T)
        }
      }
    }
  }
  
  return(res)
  
}

#--------------------------------
intersect.polygon.MCP <- function(dat = occ., collon = col.lon, collat = col.lat, polygondata = polygon.data, 
                                  distMov = dist.Mov){
  st.polygon.MCP <- gen.st.points(dat = dat, collon = collon, collat = collat) %>% 
    gen.MCP() %>% terra::vect()
  polygon <- terra::vect(polygondata)
  st.polygon.MCP <- polygon[st.polygon.MCP] %>% 
    aggregate() %>% 
    st_as_sf() %>% 
    st_cast("POLYGON") 
  st.polygon.MCP <- st.polygon.MCP[gen.st.points(dat = dat, collon = collon, collat = collat), ] %>% 
    as_Spatial()  
  
  return(st.polygon.MCP)
}

#--------------------------------
intersect.polygon.pointsBuffer <- function(dat = occ., collon = col.lon, collat = col.lat, polygondata = polygon.data, 
                                                   distMov = dist.Mov){
  st.polygon.pointsBuffer <- gen.st.points(dat = dat, collon = collon, collat = collat) %>% 
    gen.st.buffer(distMov = distMov) %>% 
    gen.Polygon(polygondata = polygondata, occurrences = dat) %>% 
    as_Spatial()
  return(st.polygon.pointsBuffer)
}

#--------------------------------
intersect.polygonBuffer <- function(dat = occ., collon = col.lon, collat = col.lat, polygondata = polygon.data, 
                                     distMov = dist.Mov){
  st.polygonBuffer <- gen.st.points(dat = dat, collon = collon, collat = collat) %>% 
    gen.Polygon(polygondata = polygondata, occurrences = dat) %>% 
    gen.st.buffer(distMov = distMov) %>% 
    as_Spatial()
  return(st.polygonBuffer)
}

#--------------------------------
intersect.polygon.points <- function(dat = occ., collon = col.lon, collat = col.lat, polygondata = polygon.data,
                                     distMov = dist.Mov){
  st.polygon.points <- gen.st.points(dat = dat, collon = collon, collat = collat) %>% 
    gen.Polygon(polygondata = polygondata, occurrences = dat) %>% 
    as_Spatial()  
  return(st.polygon.points)
}

#--------------------------------
gen.points.MCP.buffer <- function(dat = occ., collon = col.lon, collat = col.lat, distMov = dist.Mov){
  st.points.MCP.buffer <- gen.st.points(dat, collon = collon, collat = collat) %>% 
    gen.MCP() %>% 
    gen.st.buffer(distMov = distMov) %>% 
    as_Spatial()
  return(st.points.MCP.buffer)
}

#--------------------------------
gen.points.MCP <- function(dat = occ., collon = col.lon, collat = col.lat){
  st.points.MCP <- gen.st.points(dat = dat, collon = collon, collat = collat) %>% 
    gen.MCP() %>% 
    as_Spatial()
  return(st.points.MCP)
}

#--------------------------------
gen.points.buffer <- function(dat = occ., collon = col.lon, collat = col.lat, dist.Mov){
  st.points.buffer <- gen.st.points(dat = dat, collon = collon, collat = collat) %>% 
    gen.st.buffer(., distMov = dist.Mov) %>% 
    as_Spatial()
  return(st.points.buffer)
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
  stobject_transform <- st_transform(stobject, crs = 7801)
  stbuffer_transform <- st_buffer(stobject_transform, units::set_units(distMov, km)) %>%
    st_union()
  stbuffer <- st_transform(stbuffer_transform, crs = st_crs(stobject))
}

#--------------------------------
# Minimum convex polygon

gen.MCP <- function(stobject) {
  st_convex_hull(st_union(stobject))
}

#--------------------------------
# Generate Polygon

gen.Polygon <- function(stobject, polygondata = polygon.data, occurrences = occ.) {
  Polygon. <- sf::st_read(polygondata)

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
