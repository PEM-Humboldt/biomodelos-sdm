# This script, named Extent&convert2PNG.R, is designed to prepare raster data for BioModelos by 
# converting TIFF  files to PNG images and creating additional files for visualization. 
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
# - Reclassifies the raster data based on predefined thresholds.
# 2.3. Conversion to PNG:
# - Applies the convert2PNG function to each statistic raster
#
# 3. Convert N1 or Level 1 Models (From 2023 onwards, it is preferable to use the Geoserver to upload these models.)
#
# 4. Convert N2 or Level 2 Models (From 2023 onwards, it is preferable to use the Geoserver to upload these models.)   

#-----------

library(rgdal)
library(raster)
library(maptools)
library(dplyr)

# 1: prepare objects and load functions

wd <- ".../biomodelos-sdm/postprocessing/Ediciones_BioModelos"
# Example: wd <- "D:/humboldt/bm_ediciones_upload/actualizacion_carnivoros"

# Set the working directory and load necessary raster files.
setwd(wd)

# load reference map
ref.map <- raster("Info_base/ref_map.tif")
# Example: ref.map <- raster("D:/humboldt/biomodelos-sdm/postprocessing/Ediciones_BioModelos/Info_base/ref_map.tif")

# Load the convert2PNG function and necessary parameters for the conversion process.
ruta_funcion <- ("Rutinas/Funciones/")
# Example: ruta_funcion <- ("D:/humboldt/biomodelos-sdm/postprocessing/Ediciones_BioModelos/Rutinas/Funciones/")

source(paste0(ruta_funcion, "/convert2PNG.R"))
load(paste0(ruta_funcion, "/params.RData"))

# set wide and hight for thumbnails
w <- 179
h <- 220

#-----------
# 2. Convert continuous and thresholded models
# Binaries by default are 0, 10, 20 and 30.

# 2.1 prepare folders
# path in where are stored tif models
in.folder <- "Especies/Statistics"

output.folder <- getwd()

sp.raster <- list.files(in.folder, pattern = "*.tif$", full.names = T)
names <- list.files(in.folder, pattern = "*.tif$", full.names = F) %>%  
  gsub('*.tif$', '', .)


# 2.2 Adjust the projection and extent of each raster to WGS84 and BioModelos standards:
# Use if the TIFF files have a coordinate system different from WGS84 or if they have 
# an extent smaller than of BioModelos (xmin: -83, xmax: -60, ymin: -14, ymax: 13).
# In case of use biomodelos-sdm modelling tool, it is usual not to need.

#for (i in 1:length(con.list)) {
 # map <- raster(con.list[i])
  #if (map@crs@projargs != ref.map@crs@projargs) {
   # cat('Adjusting projection for', names[i], '\n')
    #map <- projectRaster(map, ref.map)
    #map2 <- extend(map, ref.map)
    #extent(map2) <- extent(ref.map)
    #writeRaster(map2, paste0(output.folder, "/", names[i]), format = "GTiff", datatype = 'INT2S', overwrite = TRUE)
  #} else
   # cat(names[i], "doesn't need adjustment to projection \n")
#}
######################################################################################################
#Este es la función for de arriba modificada 
con.list<- sp.raster
# 1. Crear carpeta para los TIFF procesados
output_tif_folder <- file.path(output.folder, "tiff_procesados")
if (!dir.exists(output_tif_folder)) dir.create(output_tif_folder, recursive = TRUE)

# 2. Función para detectar si un raster es categórico / binario
es_categorico_o_binario <- function(r) {
  # Muestra aleatoria de valores, para no cargar todo en memoria
  n_muestra <- min(10000, ncell(r))
  vals <- tryCatch(
    sampleRandom(r, size = n_muestra, na.rm = TRUE, useGDAL = TRUE),
    error = function(e) {
      # Si falla sampleRandom, intentamos getValues (puede ser pesado)
      v <- getValues(r)
      v[!is.na(v)]
    }
  )
  
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(FALSE)
  
  # Si tiene pocos valores únicos y todos son enteros, lo tratamos como categórico/binario
  unicos <- unique(vals)
  es_entero <- all(unicos == round(unicos))
  pocos_valores <- length(unicos) <= 20  # puedes ajustar este umbral
  
  return(es_entero && pocos_valores)
}

for (i in seq_along(con.list)) {
  cat("Procesando:", names[i], "\n")
  
  r <- raster(con.list[i])
  
  # 3. Decidir método y tipo de dato según el tipo de raster
  if (es_categorico_o_binario(r)) {
    cat("  -> Detectado como CATEGÓRICO/BINARIO\n")
    metodo <- "ngb"
    dtype  <- "INT2S"
  } else {
    cat("  -> Detectado como CONTINUO\n")
    metodo <- "bilinear"
    dtype  <- "FLT4S"
  }
  
  # 4. Alinear al mapa de referencia
  if (!compareCRS(r, ref.map)) {
    cat("  -> Ajustando proyección (CRS distinto)\n")
    r2 <- projectRaster(from = r, to = ref.map, method = metodo)
  } else if (!compareRaster(r, ref.map,
                            extent = TRUE, rowcol = TRUE,
                            res = TRUE, crs = TRUE,
                            stopiffalse = FALSE)) {
    cat("  -> CRS igual pero tamaño/resolución/extensión diferentes: remuestreando\n")
    r2 <- resample(r, ref.map, method = metodo)
  } else {
    cat("  -> Ya está alineado con ref.map, solo se copia\n")
    r2 <- r
  }
  
  # 5. Forzar extent idéntico (por seguridad)
  extent(r2) <- extent(ref.map)
  
  # 6. Chequeos de seguridad
  mismo_dim <- (nrow(r2) == nrow(ref.map)) && (ncol(r2) == ncol(ref.map))
  misma_res <- all(res(r2) == res(ref.map))
  mismo_crs <- compareCRS(r2, ref.map)
  mismo_ext <- all(as.vector(extent(r2)) == as.vector(extent(ref.map)))
  
  if (!mismo_dim || !misma_res || !mismo_crs || !mismo_ext) {
    stop(paste("⚠️ El raster", names[i], "NO quedó alineado con ref.map. Revisa el archivo de origen."))
  }
  
  # 7. Guardar el TIFF alineado en la carpeta "tiff_procesados"
  writeRaster(
    r2,
    filename = file.path(output_tif_folder, paste0(names[i], ".tif")),
    format   = "GTiff",
    datatype = dtype,
    overwrite = TRUE
  )
  
  cat("✔ Guardado:", paste0(names[i], ".tif"), "en", output_tif_folder, "\n\n")
}

#se renombran en las demás funciones sp.raster y conlist por sp.raster2 y conlist2 para usar los nuevos archivos redimensionados 
sp.raster2 <- list.files("C:/biomodelos-sdm-master/postprocessing/Ediciones_BioModelos/Especies/anfibios_amazonicos/tiff_procesados", pattern = "*.tif$", full.names = T)

con.list2 <-sp.raster2
######################################################################################################
# map <- raster(con.list[4])
# map <- projectRaster(map, ref.map)
# map2 <- extend(map, ref.map)
# extent(map2) <- extent(ref.map)
# writeRaster(map2, paste0(output.folder, "/", names[4]), format = "GTiff", datatype = 'INT2S', overwrite = TRUE)

###for de la función de arriba 

for (i in seq_along(con.list2)) {
  
  cat("Procesando:", con.list2[i], "\n")
  
  # cargar raster
  map <- raster(con.list2[i])
  
  # reproyectar si es necesario
  if (!compareCRS(map, ref.map)) {
    map <- projectRaster(map, ref.map)
  }
  
  # extender al extent del mapa de referencia
  map2 <- extend(map, ref.map)
  
  # asignar extent exactamente igual al del mapa de referencia
  extent(map2) <- extent(ref.map)
  
  # escribir el nuevo raster
  writeRaster(
    map2,
    filename = paste0(output.folder, "/", names[i]),
    format = "GTiff",
    datatype = "INT2S",
    overwrite = TRUE
  )
}

###

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

for (i in 1:length(sp.raster2)) {
  print(sp.raster2[i])
  in.raster <- raster(sp.raster2[i])
  rc <- reclassify(in.raster, rclmat, include.lowest = FALSE)
  vals <- unique(rc)
  
  # Logical, indicating whether to add a transparent color to the palette.
  # Use TRUE when the TIFF file contains NA, 0, and 1 values; use FALSE when
  # the TIFF only has NA and 1 values.
  
  convert2PNG(rc, names[i], in.folder, colpal[vals[vals > 0]], FALSE, params, w, h)
}

#-----------
# 3. Convert consensus (named Level 1 Models - N1) (orange-ochre color) 

col.pal <- rgb(193, 140, 40, maxColorValue = 255)

# path in where are stored tif models
in.folder <- 'Especies/N1'
# Example: in.folder <- "flujo_imagenes/"

sp.raster <- list.files(in.folder, pattern = "*.tif$", full.names = F)
names <- list.files(in.folder, pattern = "*.tif$", full.names = F) %>%  
  gsub('*.tif$', '', .)

# Apply the convert2PNG function to each Level 1 model.
for (i in 1:length(sp.raster)) {
  
  # Logical, indicating whether to add a transparent color to the palette.
  #                  Use TRUE when the TIFF file contains NA, 0, and 1 values; use FALSE when
  #                  the TIFF only has NA and 1 values.
  
  convert2PNG(sp.raster = sp.raster[i], name = names[i], in.folder = in.folder, 
              col.pal = col.pal, add.trans = FALSE, params = params, w = w, h = h)
}

#------------
# 4. Convert N2 or Level 2 Models (purple color) March 4, 2022   

col.pal <- rgb(138, 47, 95, maxColorValue = 255)

# path in where are stored tif models
in.folder <- 'Especies/N2'

sp.raster <- list.files(in.folder, pattern = "*.tif$", full.names = F)
names <- list.files(in.folder, pattern = "*.tif$", full.names = F) %>%  
  gsub('*.tif$', '', .)

# Apply the convert2PNG function to each Level 2 model.
for (i in 1:length(sp.raster)) {
  
  # Logical, indicating whether to add a transparent color to the palette.
  # Use TRUE when the TIFF file contains NA, 0, and 1 values; use FALSE when
  # the TIFF only has NA and 1 values.
  
  convert2PNG(sp.raster = sp.raster[i], name = names[i], in.folder = in.folder, 
              col.pal = col.pal, add.trans = FALSE, params = params, w = w, h = h)
}
