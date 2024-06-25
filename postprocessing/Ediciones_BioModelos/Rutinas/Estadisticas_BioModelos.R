#Ajustado por: Cristian Cruz Rodríguez 01-08-2923


##  ----------------  CALCULO DE ESTADISTICAS PARA BIOMODELOS--------------------------------
## Este script genera 2 arhivos CSV. 
## - El primero corresponde a los valores de estadisticas de:
##     + Habitat (coberturas de la tierra nivel 3, 54 tipos de cobertura) versión 2018
##     + Porcentaje de ocurrencia en bosque para los annos 1990, 2000, 2005, 2010, 2012 
##     + Tres tipos de Escenarios para el 2030
##     + Porcentaje del area de distribucion en areas protegidas
##     + Extension de la ocurrencia
##     + Area estimada por el metodo de Poligono minimo convexo con base a los pixeles de ocurrencias   
##     + Area estimada por el metodo de Poligono minimo convexo con base a registros de ocurrencias
##- El segundo contiene informacion de Amenazas
##     + Human foot print
##     + Vias
##     + Titulos minero
## Para la primera especie evaluada se generan algunas salidas cartograficas basado en las capas usadas


# Cargar librerias
library(rgdal)
library(raster)
library(dismo)
library(rgeos)
library(red)

# Cargar capas y scripts
setwd('I:/BioModelos')

source('Rutinas/Funciones/stats_SDM_V2withCLC.R')

#capas disponibles en el siguiente enlace https://drive.google.com/drive/folders/1iExvwhIcQ9AWS7MML5l1LfJZDue2hTNl?usp=sharing
# Almacenar el resultado en la carpeta Info_base/CapasBioModelos

var<-'Info_base/CapasBioModelos/'
load(paste0(var, 'clc_n4.RData')) # Archivos localizados en la NAS, ajustar si estan en otro directorio
load(paste0(var, 'redSinInfo201709.RData'))
load(paste0(var, 'redBosqueAll201709.RData'))
load(paste0(var, 'protAreaTif.RData'))#stack de AP
load(paste0(var, 'Escenarios201708.RData'))
load(paste0(var, 'hfp&Min.RData'))#stack de hfp y titMin

header.met <- read.csv("Info_base/Indice_metadatosCLC.csv",header=T, sep=";")


fold<-"Especies"
setwd(fold)
files<-list.files('N2/', pattern = "veg.tif$",full.names=T)
names<-sub("_veg.tif$","", files)
names<-sub("N2/","", names)

sppPath<-data.frame(Species=names,File=files)
sppRecords <- list.files("Records/", full.names = T) # Directorio de los archivos csv con los valores de registro o puntos geograficos 'records'
sppPath$Rec_file <- paste0(sppRecords)

#REVISAR QUE SOLO QUEDE EL NOMBRE DE LA ESPECIE (QUITAR SUFIJOS)
sppName <- paste0(gsub('_veg.tif', '', basename(as.character(sppPath[, 1]))))

# Definir sistemas de coordenadas 
proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#proj <- "+proj=longlat +datum=WGS84 +no_defs"
Proj_col <- "+proj=tmerc +lat_0=4.596200417 +lon_0=-74.07750791700001 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +units=m +no_defs"

# Tablas de resultados
R_size <- data.frame()
metadata.all <- data.frame()
threats.all <- data.frame()

spresult<- 'Stats_Portal/'

s<-1
gc()
rasts =TRUE

for(s in 1:length(names)){
  #gc()
  #memory.limit(size=100000)
  #Generar capas solo para el primer registro
  if (s==1) {
    rasts = TRUE
    # Direccion de salida outputs
    sppFolder <- paste0(spresult, gsub('.tif', '', basename(as.character(sppPath[s, 2])))) # Establecer el directorio de salida
    dir.create(sppFolder, recursive = TRUE)
  } else {
    rasts = FALSE
  }
  
  
  testN2 <- raster(as.character(sppPath[s, 2]))
  cat(s, '-', names[s], '\n')
  
  ## Funciones calculos estadisticas
  # 1. Revisar que las proyecciones coinciden
  #CheckProjection(r = testN2, proj=proj)
  
  area.raster <- area(testN2)

  # 2. Calcular el tamanno de ocurrencia de la espcie km2
  RangeSize <- EstimateRangeSize( r = testN2, area.raster = area.raster)
  
  # 3. Minimo poligono convexo para Raster
  
  MCPRast <- PredictMcpRast( r = testN2, area.raster = area.raster, rast = rasts)
  #MCPRast<-list (ch.area = ch.area, ch = ch.pred)  

  # 4. Minimo poligono convexo para Registros de especies
  cat('procesing eoo and aoo \n') 
  eoo <- PredictMcpRec(r = testN2, Proj_col = Proj_col, proj = proj, rast = rasts, s = s,  sppPath = sppPath) # se reemplazó por el resultado del prquete ConR
  
  # 5. Area de ocupacion (necesario para atlas, no para estadisticas de la plataforma BioModelos)
  # aoo <- 0
  #AOO <- AOO.computing(testN2)
  
  # # 6. Habitat CLC
  #MODIFICAR PARA QUE SOLO TOME LAS COBERTURAS A PARTIR DE LA TABLA DE VARIABLES ECOLOGICAS
  Habitat <- HabitatAreaClc(r = testN2, area.raster = area.raster, clcBrick = clcBrick, clcTif = clcTif, rast = rasts)
  HabitatPercent <- Habitat*100/sum(Habitat)
  
  # # 7. Historico perdida de bosque
  ##REVISAR QUE LAS CAPAS DE DEFORESTACION ESTAN ACTUALIZADAS EN EL RDATA
  ForestArea <- ForestLoss( r = testN2, area.raster = area.raster, f90 = red1990, f00 = red2000, f05 = red2005, f10 = red2010, f12 = red2012, f13 = red2013, f14 = red2014, f16 = red2016, rast = TRUE)
  ForestPercentage <- (ForestArea / RangeSize * 100)
  
  # 8. Escenarios 2030
  EscenariosBosque <- Escenarios2030 (r = testN2, area.raster = area.raster, con2030_res = con2030_res, des2030_res = des2030_res, his2030_res = his2030_res, rast = TRUE, sppFolder = sppFolder)
  EscenariosBosquePorcentaje <- (EscenariosBosque / RangeSize * 100)
  
  # 9. Areas protegidas
  ##REVISAR QUE LAS CAPAS DE AP ESTAN ACTUALIZADAS EN EL RDATA
  ProtAreaRep <- AllProtectedFigures( r = testN2, area.raster = area.raster, ap = ap, pn = pn, sc = sc, ot = ot, rast = rasts)
  
  # # Amenazas
  ##MODIFICAR CON CAPAS ACTUALIZADAS A 2019
  # 10.Human foot print
  HFPrint <- HumanFootPrint (r = testN2, hfp = hfp, rast = rasts)
  
  
  # 11. Titulos mineros
  MinTitArea <- TitulosMineros (r = testN2, area.raster = area.raster, titMin = titMin, rast = rasts)
  
  sppModel <- gsub('.tif', '', basename(as.character(sppPath[s, 2]))) # Conserva el nombre de la especie y el tipo de modelo, sí es concenso o nivel 2
  
  #Out data frames
  metadata <- cbind.data.frame(sppModel, HabitatPercent, ForestPercentage, EscenariosBosquePorcentaje, MCPRast$ch.area, eoo$MCPrec_km2, aoo, RangeSize, ProtAreaRep)
  threats <- cbind.data.frame(sppModel, HFPrint$Alto, HFPrint$Bajo, HFPrint$Medio, HFPrint$Natural, MinTitArea)
  metadata.all <- rbind(metadata.all, metadata)
  threats.all <- rbind(threats.all, threats)
  
}

colnames(metadata.all) <- colnames(header.met)

#level 2
write.csv(metadata.all,  paste0(spresult, 'stats_level2.csv'), row.names = F)
write.csv(threats.all, paste0 (spresult, 'stats_threats_level2.csv'), row.names = F)

####_______________________________________________________________________________________________________________________________________###
## Nota                                                                                                                                    ###  
## El numeroal  5. Area de ocupacion Se calculó con el paquete ConR, debido a que las funciones usadas ya no funcionan correctamente.      ###
## El paquete conR ofrece cálculos que podrían intergrarse a esta rutina, en un mediano plazo                                              ###  
##_________________________________________________________________________________________________________________________________________###
