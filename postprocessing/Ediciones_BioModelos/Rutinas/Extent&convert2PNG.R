
###Implementar la funcion convert2PNG.R

###Paquetes necesarios
library(rgdal)
library(raster)
library(maptools)

### PREPARANCION DE LOS RASTER
### SI LOS TIFF VIENEN CON SISTEMA DE COORDENADAS DIFERENTE A WGS84
### O SI VIENEN CON UN EXTENT DIFERENTE AL DE BIOMODELOS (xmin:-83, xmax:-60, ymin:-14, ymax:13)

#1. Definirdirectoiro de trabajo 
setwd('I:/BioModelos')
ref.map <- raster("Info_base/ref_map.tif")
con.list <- list.files('Especies/Statistics',pattern = "*.tif$", full.names = T)
names <- gsub('*.tif$','',list.files('Especies/Statistics',pattern = "*.tif$", full.names = F))
dir.create('ext_pub')
outputfile <- "Especies/Statistics/"

#2. Ajustar projecci[o]n
for (i in 1:length(con.list)){
    #leer cada modelo y darle el sistema de referencia WGS84
  map <- raster(con.list[i])#, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  if (map@crs@projargs != ref.map@crs@projargs){
  cat('ajust projection to', names[i], '\n')
  map <- projectRaster(map,ref.map) #usar si el mapa viene en otra proyeccion
  #dar el extent de BioModelos a los modelos publicados
  map2<-extend(map,ref.map)
  extent(map2)<-extent(ref.map)
  #exportar los .tif de los modelos con el sistema de referencia y extent de BioModelos
  writeRaster(map2,paste0(outputfile,"/",names[i]),format="GTiff",datatype='INT2S',overwrite=T)
  }else
   cat(names[i], "don't need ajust to projection \n")
}

#3 Establecer la ruta donde se encuentra guardada la funcion convert2PNG y params.RData
ruta_funcion <-("Rutinas/Funciones")
source(paste0(ruta_funcion, "/convert2PNG.R"))
load(paste0(ruta_funcion, "/params.RData"))

###Medidas (alto:h y ancho:w) de los thumbnails (en pixeles) para la version 2 de BioModelos
w=179
h=220

#####PALETA DE COLORES
###1. Modelos continuos

colpal<-c(rgb(255, 255, 255, 0, maxColorValue=255),
          rgb(32,131,141,maxColorValue = 255),
          rgb(143,201,143,maxColorValue = 255),
          rgb(237,188,37,maxColorValue = 255),
          rgb(213,120,51,maxColorValue = 255),
          rgb(193,140,40,maxColorValue = 255))

rclmat<-matrix(c(-Inf,0,1,0,0.2,2,0.2,0.4,3,0.4,0.6,4,0.6,0.8,5,0.8,1,6),ncol=3,byrow=T)

###Aplicar la funcion convert2PNG a cada .tif.
for(i in 1:length(con.list)){
  print(con.list[i])
  in.raster<-raster(con.list[i])
  rc <- reclassify(in.raster, rclmat,include.lowest=F)
  vals<-unique(rc)
    #Colocar FALSE cuando el background = NA (cuando solo hay valores de 1 en el raster y NA), de lo contrario poner TRUE
  convert2PNG(rc, names[i], outputfile, colpal[vals[vals>0]], FALSE, params,w,h)
}

### ELEGIR SI SON MODELOS NIVEL 1 O 2
#--------------------------------------------#
###2. Modelos Nivel 1 (color naranja-ocre)   #
#--------------------------------------------#

dir.create('Especies/N1')
###2. Modelos Nivel 1 (color naranja-ocre)
col.pal=rgb(193,140,40,maxColorValue=255)
in.folder <- 'Especies/N1/'

archivos <- list.files('Especies/N1/',pattern = "*.tif$", full.names =F)
archivos_name <- gsub('*_con.tif$','', archivos)

###Aplicar la funcion convert2PNG a cada .tif.
for (i in 1:length(archivos)){
  #Colocar FALSE cuando el background = NA (cuando solo hay valores de 1 en el raster y NA), de lo contrario poner TRUE
  convert2PNG(archivos[i], archivos_name[i], in.folder, col.pal, F, params,w,h)
}


#----------------------------------------------------------#
###3. Modelos Nivel 2 (color morado) 4 de marzo del 2022   #
#----------------------------------------------------------#

dir.create('Especies/N2')
col.pal=rgb(138,47,95,maxColorValue=255)
in.folder <- 'Especies/N2/'

sp.raster <- list.files(in.folder, pattern = "veg.tif$")
name = gsub('*_veg.tif$','', sp.raster)


for (i in 1:length(sp.raster)){
  #Colocar FALSE cuando el background = NA (cuando solo hay valores de 1 en el raster y NA), de lo contrario poner TRUE
  convert2PNG(sp.raster[i], name[i], in.folder, col.pal, F, params,w,h)
}
