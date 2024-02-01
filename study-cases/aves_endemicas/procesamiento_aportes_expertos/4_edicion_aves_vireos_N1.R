library(terra)
library(rgdal)
library(dplyr)
library(openxlsx)
library(sf)
library(wallace)

#Ediciones aves mediaevaluación
crs.new<- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'

########################
####  Informaci?n Base
#######################

# output directory
output_dir <- "modelos_N1/aves_vireo"

#### Capa Base
alt1<-raster::getData('alt', country = 'COL', path = 'C:/humboldt/_capas_SIG_NAL/politico/getData_raster')
alt2<-raster::getData('alt', country= 'VEN', path = 'C:/humboldt/_capas_SIG_NAL/politico/getData_raster')
alt3<-raster::getData('alt', country= 'ECU', path = 'C:/humboldt/_capas_SIG_NAL/politico/getData_raster')
alt4<-raster::getData('alt', country= 'PER', path = 'C:/humboldt/_capas_SIG_NAL/politico/getData_raster')
alt5<-raster::getData('alt', country= 'BRA', path = 'C:/humboldt/_capas_SIG_NAL/politico/getData_raster')
alt6<-raster::getData('alt', country= 'PAN', path = 'C:/humboldt/_capas_SIG_NAL/politico/getData_raster')
alt7<-raster::getData('alt', country= 'BOL', path = 'C:/humboldt/_capas_SIG_NAL/politico/getData_raster')

alt<- rast(mosaic(alt1,alt2,alt3,alt4,alt5,alt6,alt7,  fun = sum))
rm(alt1, alt2, alt3, alt4, alt5, alt6,alt7)
alt1<-crop(alt, y=c(xmin=-83, xmax=-60, ymin=-14, ymax=13))
alt2<-crop(alt, y=c(xmin=-83, xmax=-60, ymin=-14, ymax=14))


##political plots
col<- raster::getData('GADM', country='COL', path = 'C:/humboldt/_capas_SIG_NAL/politico/getData_raster',  level=1)
ecu<-raster::getData('GADM', country = 'ECU', path = 'C:/humboldt/_capas_SIG_NAL/politico/getData_raster',  level=0)
ven<-raster::getData('GADM', country = 'VEN', path = 'C:/humboldt/_capas_SIG_NAL/politico/getData_raster',  level=0)
per<-raster::getData('GADM', country = 'PER', path = 'C:/humboldt/_capas_SIG_NAL/politico/getData_raster',  level=0)
bra<-raster::getData('GADM', country = 'BRA', path = 'C:/humboldt/_capas_SIG_NAL/politico/getData_raster',  level=0)
pan<-raster::getData('GADM', country = 'PAN', path = 'C:/humboldt/_capas_SIG_NAL/politico/getData_raster',  level=0)
bol<-raster::getData('GADM', country = 'BOL', path = 'C:/humboldt/_capas_SIG_NAL/politico/getData_raster',  level=0)

sud<-raster::bind(col, ecu, ven,  bra, per, pan, bol)
sud_aoi<-crop(sud, y=c(xmin=-83, xmax=-60, ymin=-14, ymax=13))
rm(ecu,ven,per,bra,pan,bol, sud)
sud_aoi<- vect(sud_aoi)

#---------------

# Vireo approximans (Providencia) 

Vir_app <- vect("modelos_to_edit/aves_media_evaluacion/vireos/providencia_.shp",
              crs = crs(alt2)) |> rasterize(alt2)

names(Vir_app) <- "Vireo approximans"

writeRaster(Vir_app, paste0(output_dir, "/Vireo_approximans_con.tif"), datatype = "INT2S", overwrite = TRUE)

#---------------

# Vireo caribaeus (San Andrés)


Vir_car <- vect("modelos_to_edit/aves_media_evaluacion/vireos/san_andres_.shp",
                crs = crs(alt1)) |> rasterize(alt1)

names(Vir_car) <- "Vireo caribaeus"

writeRaster(Vir_car, paste0(output_dir, "/Vireo_caribaeus_con.tif"), datatype = "INT2S", overwrite = TRUE)

