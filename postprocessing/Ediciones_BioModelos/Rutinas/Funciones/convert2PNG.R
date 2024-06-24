#'convert2PNG.R
#'
#' Convert TIFF to PNG and create KMZ and thumbnail images.
#'
#' This function converts a TIFF file to a PNG file, creates a KMZ file,
#' and generates a thumbnail image. The projection, extent, and color scheme have been
#' optimized for BioModelos.
#
#' @param sp.raster Character string with the TIFF filename (including extension).
#' @param name Name for the output files (KMZ, PNG, and thumbnail). If NULL, the function
#'             extracts the name from the TIFF file.
#' @param in.folder Folder that contains the TIFF file to convert.
#' @param col.pal Color palette to be used in resulting maps.
#' @param add.trans Logical, indicating whether to add a transparent color to the palette.
#'                  Use TRUE when the TIFF file contains NA, 0, and 1 values; use FALSE when
#'                  the TIFF only has NA and 1 values.
#' @param params List with elements dem, a raster with elevation for the background of thumbnails,
#'               and shape, a SpatialPolygonsDataFrame with administrative boundaries for thumbnails.
#' @param w Width of the thumbnail image.
#' @param h Height of the thumbnail image.
#'
#' @examples
#' in.folder <- "~/Modelos/librorojo2"
#' col.pal <- rgb(193, 140, 40, maxColorValue = 255)
#' sp.raster <- "Anas_bahamensis_0.tif"
#' convert2PNG(sp.raster, NULL, in.folder, col.pal, TRUE, params = params, w = 800, h = 600)
#'
#' @export
#' 
#' @author Jorge Velasquez
#' @date 05-09-2014
#' 
#' Updated
#' @date 24-01-2024

convert2PNG <- function(sp.raster, name, in.folder, col.pal, add.trans, params, w, h) {
  
  # Load necessary libraries
  require(raster)
  require(sp)
  require(rgdal)
  
  # Create directories to store output files
  dir.create(file.path(in.folder, "PNG"), recursive = TRUE, showWarnings = F)
  dir.create(file.path(in.folder, "KMZ"), recursive = TRUE, showWarnings = F)
  dir.create(file.path(in.folder, "thumb"), recursive = TRUE, showWarnings = F)
  
  #-----------
  # Check if the input raster is already a RasterLayer or needs to be loaded
  if (class(sp.raster) == "RasterLayer") {
    in.raster <- sp.raster
  } else {
    in.raster <- raster(file.path(in.folder, sp.raster))
    
    # Set a default projection if not available
    if (is.na(projection(in.raster))) {
      projection(in.raster) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
    }
  }
  
  # Add transparency to the color palette if specified
  tr <- rgb(255, 255, 255, 0, maxColorValue = 255)
  if (add.trans) {
    col.pal <- c(tr, col.pal)
  }
  
  #-----------
  # Create a KML file from the raster
  if (is.null(name)) {
    name <- strsplit(sp.raster, "[.]")[[1]][1]
  }
  KML(in.raster, filename = file.path(in.folder, "KMZ", paste0(name, ".kmz")),
      maxpixels = ncell(in.raster), col = col.pal, overwrite = TRUE, blur = 1)
  
  # Unzip the KMZ file to extract PNG files
  unzip(file.path(in.folder, "KMZ", paste0(name, ".kmz")), exdir = file.path(in.folder, "PNG"))
  # Remove the KML file
  file.remove(file.path(in.folder, "PNG", paste0(name, ".kml")))
  
  #-----------
  # Generate thumbnails
  in.raster.co <- in.raster
  png(file.path(in.folder, "thumb", paste0(name, "_thumb.png")),
      width = w, height = h, units = "px", type = "cairo")
  op <- par(mar = rep(0, 4), bg = NA)
  
  # Plot the background elevation
  image(params$dem, axes = FALSE, xlab = "", ylab = "", col = c(tr, "grey90"))
  
  # Overlay the raster with the specified color palette
  image(in.raster.co, col = col.pal, axes = FALSE, add = TRUE)
  
  # Plot administrative boundaries
  plot(params$shape, add = TRUE, lwd = 1, border = "grey40")
  dev.off()
  
  #-----------
  # Remove temporary files
  unlink(list.files(tempdir()), recursive = TRUE)
}

