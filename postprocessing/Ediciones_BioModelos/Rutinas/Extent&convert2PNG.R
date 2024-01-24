# This script, named Extent&convert2PNG.R, is designed to prepare raster data for BioModelos by 
# converting TIFF  files to georeferenced PNG images and creating additional files for visualization. 
# The script involves several steps:
#
# 1. Prepare objects and load functions: Loads the convert2PNG function and necessary parameters for 
# the conversion process.
#
# 2. Convert Statistics or N0
# 2.1 prepare folders
# 2.2 Adjust the projection and extent
# 2.3. Color Palette and Reclassification:
# - Defines a color palette for continuous models.
# - Reclassifies the raster data based on predefined thresholds.#
# 2.3. Conversion to PNG:
# - Applies the convert2PNG function to each statistic raster# 
# 3. Convert N1 or Level 1 Models (From 2023 onwards, it is preferable to use the Geoserver to upload these models.)
# 4. Convert N2 or Level 2 Models (From 2023 onwards, it is preferable to use the Geoserver to upload these models.)   

#-----------
# Step 1: prepare objects and load functions

# Set the working directory and load necessary raster files.
setwd('path_to_working_directory')

# load reference map
ref.map <- raster("path_where_is_stored_Info_base/ref_map.tif")

# Load the convert2PNG function and necessary parameters for the conversion process.
ruta_funcion <- ("path_where_is_stored_function_convert2PNG.R_and_params.RData")
source(paste0(ruta_funcion, "/convert2PNG.R"))
load(paste0(ruta_funcion, "/params.RData"))

#-----------
# 2. Convert Statistics or N0
# Statistics models group two kinds of models, continuos and binaries (or thresholded). Binaries 
# by default are 0, 10, 20 and 30.

# 2.1 prepare folders
# path in where are stored tif models
in.folder <- 'path_to_statistics_models'
sp.raster <- list.files(in.folder, pattern = "*.tif$", full.names = F)
names <- gsub('*.tif$', '', sp.raster)
outputfile <- "path_to_save"

# 2.2 Adjust the projection and extent of each raster to WGS84 and BioModelos standards:
# Use if the TIFF files have a coordinate system different from WGS84 or if they have 
# an extent smaller than of BioModelos (xmin: -83, xmax: -60, ymin: -14, ymax: 13).
# In case of use biomodelos-sdm modelling tool, it is usual not to need.

for (i in 1:length(con.list)) {
  map <- raster(con.list[i])
  if (map@crs@projargs != ref.map@crs@projargs) {
    cat('Adjusting projection for', names[i], '\n')
    map <- projectRaster(map, ref.map)
    map2 <- extend(map, ref.map)
    extent(map2) <- extent(ref.map)
    writeRaster(map2, paste0(outputfile, "/", names[i]), format = "GTiff", datatype = 'INT2S', overwrite = TRUE)
  } else
    cat(names[i], "doesn't need adjustment to projection \n")
}

# 2.3 Color Palette and Reclassification
# Define a color palette for continuous models.
colpal <- c(rgb(255, 255, 255, 0, maxColorValue = 255),
            rgb(32, 131, 141, maxColorValue = 255),
            rgb(143, 201, 143, maxColorValue = 255),
            rgb(237, 188, 37, maxColorValue = 255),
            rgb(213, 120, 51, maxColorValue = 255),
            rgb(193, 140, 40, maxColorValue = 255))

# Reclassify the raster data based on predefined thresholds.
rclmat <- matrix(c(-Inf, 0, 1, 0, 0.2, 2, 0.2, 0.4, 3, 0.4, 0.6, 4, 0.6, 0.8, 5, 0.8, 1, 6), ncol = 3, 
                 byrow = TRUE)

# 2.4 Conversion to PNG
# Apply the convert2PNG function to each raster, generating PNG images and thumbnails.

w = 179
h = 220

for (i in 1:length(con.list)) {
  print(con.list[i])
  in.raster <- raster(con.list[i])
  rc <- reclassify(in.raster, rclmat, include.lowest = FALSE)
  vals <- unique(rc)
  
  # Logical, indicating whether to add a transparent color to the palette.
  # Use TRUE when the TIFF file contains NA, 0, and 1 values; use FALSE when
  # the TIFF only has NA and 1 values.
  
  convert2PNG(rc, names[i], outputfile, colpal[vals[vals > 0]], FALSE, params, w, h)
}

#-----------
# 3. Convert N1 or Level 1 Models (orange-ochre color) 

dir.create('Especies/N1')
col.pal <- rgb(193, 140, 40, maxColorValue = 255)

# path in where are stored tif models
in.folder <- 'path_to_tif_N1'
sp.raster <- list.files(in.folder, pattern = "*.tif$", full.names = FALSE)
name <- gsub('*_con.tif$', '', archivos)

# Apply the convert2PNG function to each Level 1 model.
for (i in 1:length(sp.raster)) {
  
  # Logical, indicating whether to add a transparent color to the palette.
  #                  Use TRUE when the TIFF file contains NA, 0, and 1 values; use FALSE when
  #                  the TIFF only has NA and 1 values.
  
  convert2PNG(sp.raster[i], name[i], in.folder, col.pal, FALSE, params, w, h)
}

#------------
# 4. Convert N2 or Level 2 Models (purple color) March 4, 2022   

dir.create('Especies/N2')
col.pal <- rgb(138, 47, 95, maxColorValue = 255)

# path in where are stored tif models
in.folder <- 'path_to_tif_N2'
sp.raster <- list.files(in.folder, pattern = "veg.tif$")
name <-  gsub('*_veg.tif$', '', sp.raster)

# Apply the convert2PNG function to each Level 2 model.
for (i in 1:length(sp.raster)) {
  
  # Logical, indicating whether to add a transparent color to the palette.
  # Use TRUE when the TIFF file contains NA, 0, and 1 values; use FALSE when
  # the TIFF only has NA and 1 values.
  
  convert2PNG(sp.raster[i], name[i], in.folder, col.pal, FALSE, params, w, h)
}