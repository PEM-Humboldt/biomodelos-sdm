library(raster)
library(rgdal)

dem<-raster("F:/IMac/IAVH/BioModelos/ProcesamientoEdiciones/alt.asc")
col<-raster("F:/IMac/IAVH/BioModelos/ProcesamientoEdiciones/maskColombia.tif")
raster.ref <- raster("F:/IMac/IAVH/BioModelos/Aves/Modelos consenso/results/Anisognathus melanogenys_con.tif")
outpath<-'F:/IMac/IAVH/BioModelos/Aves/Modelos consenso/results'
wd <-"F:/IMac/IAVH/BioModelos/Aves/Modelos consenso/ediciones"
setwd(wd)
folders <- list.dirs(full.names = T)


## 1. Anisognathus melanogenys ##### 

setwd(folders[2])
edits <- list.files(patter=".json$")
edits
sp.name <-'Anisognathus melanogenys'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)

#Los aportes 
sp.shp1@data

#El umbral sobre el cual se trabajo fue 20% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Anisognathus_melanogenys_20_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1[1,],add=T)
plot(sp.shp1[2,],add=T)
plot(sp.shp1[3,],add=T)
plot(sp.shp1[4,],add=T)

#Sustraer areas al modelo (Esteban Botero-Delgadillo)
polyRemove1<-rasterize(sp.shp1[1,],sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove1==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1[1,], add=T)

polyRemove2<-rasterize(sp.shp1[2,],sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove2==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1[2,], add=T)

polyRemove3<-rasterize(sp.shp1[3,],sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove3==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1[3,], add=T)

polyRemove4<-rasterize(sp.shp1[4,],sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove4==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1[4,], add=T)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"))


## 2. Anthocephala berlepschi #####
setwd(folders[3])
edits <- list.files(patter=".json$")
edits
sp.name <-'Anthocephala berlepschi'
#María Ángela Echeverry seleccionó el umbral del 0% pero no hizo acción
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)

#Los aportes 
sp.shp1@data

#El umbral sobre el cual se trabajo fue 20% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Anthocephala_berlepschi_0_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1[1,],add=T)
plot(sp.shp1[2,],add=T)
plot(sp.shp1[3,],add=T)

#Sustraer areas al modelo (Cesar Antonio Ros-Muñoz)
polyRemove1<-rasterize(sp.shp1[1,],sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove1==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1[1,], add=T)

#Sustraer areas al modelo (Cesar Antonio Ros-Muñoz)
polyRemove2<-rasterize(sp.shp1[2,],sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove2==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1[2,], add=T)

#Sustraer areas al modelo (Cesar Antonio Ros-Muñoz)
polyRemove3<-rasterize(sp.shp1[3,],sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove3==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1[3,], add=T)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"))

## 3. NO TIENE EDICIONES (SELECCIÓN UMBRAL) Anthocephala floriceps #####
setwd(folders[4])
edits <- list.files(patter=".json$")
edits
sp.name <-'Anthocephala floriceps'
#SELECCIÓN DE UMBRAL DEL 20%

## 4. Arremon basilicus #####
setwd(folders[5])
edits <- list.files(patter=".json$")
edits
sp.name <-'Arremon basilicus'

#Nicholas Bayly seleccionó el umbral del 10% y luego del 20% pero no hizo acción
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[3]),encoding = "GeoJSON",verbose=FALSE)
#Maria  Angela Echeverry-Galvis seleccionó el umbral del 0% pero no hizo acción

#Los aportes 
sp.shp1@data

#El umbral del 0%
sp.raster <- raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Arremon_basilicus_0_MAXENT.tif")

#Recortar por poligono (David Alexander Prieto Torres)
polyEoo <- rasterize(sp.shp1,sp.raster,field=1) 
sp.raster<-sp.raster*polyEoo
plot(sp.raster)
plot(sp.shp1, add=T)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 5. Atlapetes blancae #####
setwd(folders[6])
edits <- list.files(patter=".json$")
edits
sp.name <-'Atlapetes blancae'

sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[4]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data

#El umbral del 0%
sp.raster <- raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Atlapetes_blancae_0_MAXENT.tif")

#Recortar por poligono (Sergio Chaparro-Herrera)
polyEoo <- rasterize(sp.shp1,sp.raster,field=1) 
sp.raster<-sp.raster*polyEoo
plot(sp.raster)
plot(sp.shp1, add=T)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 6. Atlapetes flaviceps #####
setwd(paste0(wd,"/",folders[7]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Atlapetes flaviceps'

sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)
sp.shp2<-readOGR(paste0(wd,"/",sp.name,"/",edits[5]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
sp.shp2@data

#El umbral sobre el cual se trabajo fue 30% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Atlapetes_flaviceps_30_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1[1,],add=T)
plot(sp.shp1[2,],add=T)
plot(sp.shp1[3,],add=T)
plot(sp.shp1[4,],add=T)
plot(sp.shp1[5,],add=T)

#Sustraer areas al modelo (Esteban Botero-Delgadillo)
polyRemove1<-rasterize(sp.shp1[1,],sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove1==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1[1,], add=T)
#Sustraer areas al modelo (Esteban Botero-Delgadillo)
polyRemove2<-rasterize(sp.shp1[2,],sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove2==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1[2,], add=T)
#Sustraer areas al modelo (Esteban Botero-Delgadillo)
polyRemove3<-rasterize(sp.shp1[3,],sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove3==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1[3,], add=T)
#Sustraer areas al modelo (Esteban Botero-Delgadillo)
polyRemove4<-rasterize(sp.shp1[4,],sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove4==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1[4,], add=T)
#Agregar areas al modelo (Esteban Botero-Delgadillo)
polyAdd<-rasterize(sp.shp1[5,],sp.raster,field=1)
sp.raster[Which(polyAdd==1)]<-1
plot(sp.raster)
plot(sp.shp1[5,],add=T)

#La edicion de Carolina Diaz (recorte por poligono) se hizo sobre el modelo continuo, pero coincide con las ediciones de Esteban.

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 7. Atlapetes fuscoolivaceus #####
setwd(paste0(wd,"/",folders[8]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Atlapetes fuscoolivaceus'

sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[3]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data

#El umbral sobre el cual se trabajo fue 10% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Atlapetes_fuscoolivaceus_10_MAXENT.tif")
plot(sp.raster)

#Recortar por poligono (Juan Miguel Ruiz Ovalle)
polyEoo <- rasterize(sp.shp1,sp.raster,field=1) 
sp.raster<-sp.raster*polyEoo
plot(sp.raster)
plot(sp.shp1, add=T)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 8. Atlapetes melanocephalus #####
setwd(paste0(wd,"/",folders[9]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Atlapetes melanocephalus'

sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data

#El umbral sobre el cual se trabajo fue 20% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Atlapetes_melanocephalus_20_MAXENT.tif")
plot(sp.raster)

#Sustraer areas al modelo (Esteban Botero-Delgadillo)
polyRemove1<-rasterize(sp.shp1[1,],sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove1==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1[1,], add=T)
#Sustraer areas al modelo (Esteban Botero-Delgadillo)
polyRemove2<-rasterize(sp.shp1[2,],sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove2==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1[2,], add=T)
#Sustraer areas al modelo (Esteban Botero-Delgadillo)
polyRemove3<-rasterize(sp.shp1[3,],sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove3==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1[3,], add=T)
#Sustraer areas al modelo (Esteban Botero-Delgadillo)
polyRemove4<-rasterize(sp.shp1[4,],sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove4==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1[4,], add=T)
#Sustraer areas al modelo (Esteban Botero-Delgadillo)
polyRemove5<-rasterize(sp.shp1[5,],sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove5==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1[5,], add=T)
#Sustraer areas al modelo (Esteban Botero-Delgadillo)
polyRemove6<-rasterize(sp.shp1[6,],sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove6==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1[6,], add=T)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 9. NO TIENE EDICIONES (SELECCIÓN UMBRAL) Bangsia aureocincta #####
setwd(paste0(wd,"/",folders[10]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Bangsia aureocincta'

sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
#SELECCIÓN DE UMBRAL DEL 20%

## 10. Bangsia melanochlamys #####
setwd(paste0(wd,"/",folders[11]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Bangsia melanochlamys'

sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)

#Los aportes 
sp.shp1@data

#El umbral sobre el cual se trabajo fue 30% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Bangsia_melanochlamys_30_MAXENT.tif")
plot(sp.raster)

#Sustraer areas al modelo (Maria  Angela Echeverry-Galvis)
polyRemove<-rasterize(sp.shp1[1,],sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1[1,], add=T)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 11. Bolborhynchus ferrugineifrons #####
setwd(paste0(wd,"/",folders[12]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Bolborhynchus ferrugineifrons'

sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data

#El umbral sobre el cual se trabajo fue 10% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Bolborhynchus_ferrugineifrons_10_MAXENT.tif")
plot(sp.raster)
#Recortar por poligono (Jeyson Sanabria-Mejia)
polyEoo <- rasterize(sp.shp1,sp.raster,field=1) 
sp.raster<-sp.raster*polyEoo
plot(sp.raster)
plot(sp.shp1, add=T)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 12. Campylopterus phainopeplus #####
sp.name <-'Campylopterus phainopeplus'
#Expert0: Sergio Chaparro
sp.raster1 <- raster("F:/IMac/IAVH/BioModelos/Aves/Modelos consenso/ediciones_18042022/Campylopterus phainopeplus/Campylopterus_phainopeplus.tif")
raster.ref <- raster("F:/IMac/IAVH/BioModelos/Aves/Modelos consenso/results/Anisognathus melanogenys_con.tif")
sp.raster<-extend(sp.raster1,raster.ref)
extent(sp.raster)<-extent(raster.ref)
#Recortar por altura
#Cortar por 1000-4800 msnm
new.dem<-extend(dem,sp.raster)
extent(new.dem)<-extent(sp.raster)
sp.raster<-sp.raster*(new.dem>=1000)*(new.dem<=4800)
plot(sp.raster)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 13. Capito hypoleucus #####
setwd(paste0(wd,"/",folders[13]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Capito hypoleucus'

sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
#El umbral sobre el cual se trabajo fue 20% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Capito_hypoleucus_20_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)

#Recortar por poligono (Carolina Diaz Jaramillo)
polyEoo <- rasterize(sp.shp1,sp.raster,field=1) 
sp.raster<-sp.raster*polyEoo
plot(sp.raster)
plot(sp.shp1, add=T)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 14. NO TIENE EDICIONES (SELECCIÓN UMBRAL) Cercomacroides parkeri #####
setwd(paste0(wd,"/",folders[14]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Cercomacroides parkeri'

sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)
#SELECCIÓN DE UMBRAL DEL 10%

## 15. Chaetocercus astreans #####
setwd(paste0(wd,"/",folders[15]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Chaetocercus astreans'

sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
#El umbral sobre el cual se trabajo fue 10% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Chaetocercus_astreans_10_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)

#Recortar por poligono (Alejandra Gonzalez Moncada)
polyEoo <- rasterize(sp.shp1,sp.raster,field=1) 
sp.raster<-sp.raster*polyEoo
plot(sp.raster)
plot(sp.shp1, add=T)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 16. Chlorochrysa nitidissima #####
setwd(paste0(wd,"/",folders[16]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Chlorochrysa nitidissima'

sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
#El umbral sobre el cual se trabajo fue 10% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Chlorochrysa_nitidissima_20_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)

#Agregar areas al modelo (Ron A. Fernandez)
polyAdd<-rasterize(sp.shp1,sp.raster,field=1)
sp.raster[Which(polyAdd==1)]<-1
plot(sp.raster)
plot(sp.shp1,add=T)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 17. Chlorostilbon olivaresi #####
sp.name <- 'Chlorostilbon olivaresi'
#Expertos: Sergio Chaparro y David Ocampo
#Hacer polígono mínimo convexo incluyendo todos los registros de la especie
records <- read.csv("F:/IMac/IAVH/BioModelos/Aves/Modelos consenso/ediciones_18042022/Chlorostilbon olivaresi/Chlorostilbon.olivaresi_allRecords.csv")
library(sp)
coordinates(records) <- c("x", "y")
proj4string(records) <- CRS("+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs")

#fuente: https://stackoverflow.com/questions/58622586/how-to-construct-plot-convex-hulls-of-polygons-from-points-by-factor-using-sf
library(tidyverse)
library(sf)
library(mapview)
records.sf <- records %>%
  st_as_sf( coords = c( "x", "y" ), crs = 4326 )
# perform fast visual check using mapview-package
mapview::mapview(records.sf)
#group and summarise by species, and draw hulls
hulls <- records.sf %>%
  #group_by(id) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_convex_hull()
#result
mapview::mapview(list(records.sf, hulls))

setwd("F:/IMac/IAVH/BioModelos/Aves/Modelos consenso/ediciones_18042022/Chlorostilbon olivaresi")
st_write(hulls, "pmc_C_olivaresi.shp")

sp.shp1 <- readOGR("F:/IMac/IAVH/BioModelos/Aves/Modelos consenso/ediciones_18042022/Chlorostilbon olivaresi/pmc_C_olivaresi.shp",encoding = "ESRI",verbose=FALSE)
sp.raster<-rasterize(sp.shp1,raster.ref)
plot(sp.raster)
plot(sp.shp1,add=T)
#Recortar por altura
#Cortar por 300-600 msnm
new.dem<-extend(dem,sp.raster)
extent(new.dem)<-extent(sp.raster)
sp.raster<-sp.raster*(new.dem>=300)*(new.dem<=600)
plot(sp.raster)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)


## 18. Cistothorus apolinari #####
setwd(paste0(wd,"/",folders[17]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Cistothorus apolinari'

sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data

#El umbral sobre el cual se trabajo fue 30% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Cistothorus_apolinari_30_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1[1,],add=T)
plot(sp.shp1[2,],add=T)
plot(sp.shp1[3,],add=T)

#Agregar areas al modelo (Maria  Angela Echeverry-Galvis)
polyAdd<-rasterize(sp.shp1[1,],sp.raster,field=1)
sp.raster[Which(polyAdd==1)]<-1
plot(sp.raster)
plot(sp.shp1[1,],add=T)

#Sustraer areas al modelo (Maria  Angela Echeverry-Galvis)
polyRemove1<-rasterize(sp.shp1[2,],sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove1==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1[2,], add=T)

#Sustraer areas al modelo (Maria  Angela Echeverry-Galvis)
polyRemove2<-rasterize(sp.shp1[3,],sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove2==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1[3,], add=T)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 19. Clibanornis rufipectus #####
setwd(paste0(wd,"/",folders[18]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Clibanornis rufipectus'

sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
sp.shp2<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
sp.shp2@data

#El umbral sobre el cual se trabajo fue 20% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Clibanornis_rufipectus_20_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1[1,],add=T)
plot(sp.shp1[2,],add=T)
plot(sp.shp1[3,],add=T)
plot(sp.shp1[4,],add=T)

#Sustraer areas al modelo (Esteban Botero-Delgadillo)
polyRemove1<-rasterize(sp.shp1[1,],sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove1==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1[1,], add=T)

#Sustraer areas al modelo (Esteban Botero-Delgadillo)
polyRemove2<-rasterize(sp.shp1[2,],sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove2==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1[2,], add=T)

#Sustraer areas al modelo (Esteban Botero-Delgadillo)
polyRemove3<-rasterize(sp.shp1[3,],sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove3==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1[3,], add=T)

#Sustraer areas al modelo (Nicholas Bayly)
polyRemove4<-rasterize(sp.shp2,sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove4==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp2, add=T)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 20. Coeligena orina #####
setwd(paste0(wd,"/",folders[19]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Coeligena orina'

sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
#Ignore los aportes de Cesar Rios por que no hay registros de la especie en el poligono que dibujó
#sp.shp2<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
#sp.shp2@data

#El umbral sobre el cual se trabajo fue 20% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Coeligena_orina_20_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)
#plot(sp.shp2,add=T)
#edit<-sp.shp1+sp.shp2

#Recortar por poligono (Maria  Angela Echeverry-Galvis)
#polyEoo <- rasterize(edit,sp.raster,field=1) 
polyEoo <- rasterize(sp.shp1,sp.raster,field=1)
sp.raster<-sp.raster*polyEoo
plot(sp.raster)
plot(sp.shp1, add=T)
#plot(sp.shp2,add=T)

#Recortar por altura
#la especie va por encima de los 3000 msnm
new.dem<-extend(dem,sp.raster)
extent(new.dem)<-extent(sp.raster)
sp.raster<-sp.raster*(new.dem>=3000)
plot(sp.raster)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 21. Coeligena phalerata #####
setwd(paste0(wd,"/",folders[20]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Coeligena phalerata'

sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
#El umbral sobre el cual se trabajo fue 10% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Coeligena_phalerata_10_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)

#Sustraer areas al modelo (Esteban Botero-Delgadillo)
polyRemove <- rasterize(sp.shp1,sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1, add=T)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 22. Coeligena prunellei #####
setwd(paste0(wd,"/",folders[21]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Coeligena prunellei'

sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)
sp.shp2<-readOGR(paste0(wd,"/",sp.name,"/",edits[4]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
sp.shp2@data

#El umbral sobre el cual se trabajo fue 20% 
sp.raster1<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Coeligena_prunellei_20_MAXENT.tif")
plot(sp.raster1)
plot(sp.shp1,add=T)
plot(sp.shp2,add=T)

#Recortar por poligono (Alejandra Gonzalez Moncada)
polyEoo1 <- rasterize(sp.shp1,sp.raster1,field=1) 
sp.raster1<-sp.raster1*polyEoo1
plot(sp.raster1)
plot(sp.shp1, add=T)

#El umbral sobre el cual se trabajo fue 20% 
sp.raster2<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Coeligena_prunellei_20_MAXENT.tif")
#Recortar por poligono (Juan Miguel Ruiz Ovalle)
polyEoo2 <- rasterize(sp.shp2,sp.raster2,field=1) 
sp.raster2<-sp.raster2*polyEoo2
plot(sp.raster2)
plot(sp.shp2, add=T)

sp.raster <- sp.raster1*sp.raster2
plot(sp.raster)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 23. Cranioleuca hellmayri #####
setwd(paste0(wd,"/",folders[22]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Cranioleuca hellmayri'

sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data

#El umbral sobre el cual se trabajo fue 20% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Cranioleuca_hellmayri_20_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)

#Sustraer areas al modelo (Esteban Botero-Delgadillo)
polyRemove <- rasterize(sp.shp1,sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1, add=T)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 24. Crax alberti #####
setwd(paste0(wd,"/",folders[23]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Crax alberti'

sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data

#El umbral sobre el cual se trabajo fue 30% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Crax_alberti_30_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)

#Sustraer areas al modelo (Hugo Vides)
polyRemove<-rasterize(sp.shp1[c(3:4,6:7),],sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1[c(3:4,6:7),],add=T)

#Agregar areas al modelo (NOMBRE EXPERTO)
polyAdd<-rasterize(sp.shp1[c(1:2,5),],sp.raster,field=1)
sp.raster[Which(polyAdd==1)]<-1
plot(sp.raster)
plot(sp.shp1[c(1:2,5),],add=T)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 25. NO TIENE EDICIONES (SELECCIÓN UMBRAL) Dacnis hartlaubi #####
setwd(paste0(wd,"/",folders[24]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Dacnis hartlaubi'

#No hay acciones
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[6]),encoding = "GeoJSON",verbose=FALSE)
#SELECCIÓN DE UMBRAL DEL 10%

## 26. Diglossa gloriosissima #####
setwd(paste0(wd,"/",folders[25]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Diglossa gloriosissima'

sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data

#El umbral sobre el cual se trabajo fue 20% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Diglossa_gloriosissima_20_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)

#Recortar por poligono (Maria  Angela Echeverry-Galvis)
polyEoo <- rasterize(sp.shp1,sp.raster,field=1) 
sp.raster<-sp.raster*polyEoo
plot(sp.raster)
plot(sp.shp1, add=T)
#Recortar por altura
#la especie va por encima de los 3000 msnm
new.dem<-extend(dem,sp.raster)
extent(new.dem)<-extent(sp.raster)
sp.raster<-sp.raster*(new.dem>=3000)
plot(sp.raster)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 27. Drymophila caudata #####
sp.name <- 'Drymophila caudata'
#Experto: Sergio Chaparro
sp.raster1 <- raster("F:/IMac/IAVH/BioModelos/Aves/Modelos consenso/ediciones_18042022/Drymophila caudata/Drymophil caudata_con.tif")
sp.raster<-extend(sp.raster1,raster.ref)
extent(sp.raster)<-extent(raster.ref)
#Recortar por altura
#Cortar por 1500-2500 msnm
new.dem<-extend(dem,sp.raster)
extent(new.dem)<-extent(sp.raster)
sp.raster<-sp.raster*(new.dem>=1500)*(new.dem<=2500)
plot(sp.raster)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 28. Drymophila hellmayri #####
sp.name <- 'Drymophila hellmayri'
#Experto: Sergio Chaparro
sp.raster1 <- raster("F:/IMac/IAVH/BioModelos/Aves/Modelos consenso/ediciones_18042022/Drymophila hellmayri/Drymophila hellmayri_con.tif")
sp.raster<-extend(sp.raster1,raster.ref)
extent(sp.raster)<-extent(raster.ref)
#Recortar por altura
#Cortar por 800-2000 msnm
new.dem<-extend(dem,sp.raster)
extent(new.dem)<-extent(sp.raster)
sp.raster<-sp.raster*(new.dem>=800)*(new.dem<=2000)
plot(sp.raster)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 29. Eriocnemis isabellae #####
setwd(paste0(wd,"/",folders[26]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Eriocnemis isabellae'

sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[3]),encoding = "GeoJSON",verbose=FALSE)
sp.shp2<-readOGR(paste0(wd,"/",sp.name,"/",edits[4]),encoding = "GeoJSON",verbose=FALSE)
sp.shp3<-readOGR(paste0(wd,"/",sp.name,"/",edits[5]),encoding = "GeoJSON",verbose=FALSE)
sp.shp4<-readOGR(paste0(wd,"/",sp.name,"/",edits[6]),encoding = "GeoJSON",verbose=FALSE)
sp.shp5<-readOGR(paste0(wd,"/",sp.name,"/",edits[7]),encoding = "GeoJSON",verbose=FALSE)
sp.shp6<-readOGR(paste0(wd,"/",sp.name,"/",edits[8]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
sp.shp2@data
sp.shp3@data
sp.shp4@data
sp.shp5@data
sp.shp6@data

#El umbral sobre el cual se trabajo fue 10% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Eriocnemis_isabellae_10_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)

#El poligono de Cesar Rios (recorte por poligono) resume todas las demás ediciones de sustraer.
#Recortar por poligono (Liliana Patricia Paz Betancourt, Cesar Antonio Rios-Muñoz)
polyEoo <- rasterize(sp.shp1,sp.raster,field=1) 
sp.raster<-sp.raster*polyEoo
plot(sp.raster)
plot(sp.shp1, add=T)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 30. Eriocnemis mirabilis #####
sp.name <- 'Eriocnemis mirabilis'
#Experto: Sergio Chaparro
sp.raster1 <- raster("F:/IMac/IAVH/BioModelos/Aves/Modelos consenso/ediciones_18042022/Eriocnemis mirabilis/Eriocnemis mirabilis_con.tif")
sp.raster<-extend(sp.raster1,raster.ref)
extent(sp.raster)<-extent(raster.ref)
#Recortar por altura
#Cortar por 2100-2800 msnm
new.dem<-extend(dem,sp.raster)
extent(new.dem)<-extent(sp.raster)
sp.raster<-sp.raster*(new.dem>=2100)*(new.dem<=2800)
plot(sp.raster)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 31. Euphonia concinna #####
setwd(paste0(wd,"/",folders[27]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Euphonia concinna'

sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
sp.shp2<-readOGR(paste0(wd,"/",sp.name,"/",edits[5]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
#El poligono de Carolina Diaz no tenia acción definida
#sp.shp2@data
#El umbral sobre el cual se trabajo fue 20% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Euphonia_concinna_20_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)
#Recortar por poligono (David Alexander Prieto Torres)
polyEoo <- rasterize(sp.shp1,sp.raster,field=1) 
sp.raster<-sp.raster*polyEoo
plot(sp.raster)
plot(sp.shp1, add=T)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 32. Grallaria bangsi #####
setwd(paste0(wd,"/",folders[28]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Grallaria bangsi'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
sp.shp2<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)
sp.shp3<-readOGR(paste0(wd,"/",sp.name,"/",edits[3]),encoding = "GeoJSON",verbose=FALSE)
sp.shp4<-readOGR(paste0(wd,"/",sp.name,"/",edits[4]),encoding = "GeoJSON",verbose=FALSE)
sp.shp5<-readOGR(paste0(wd,"/",sp.name,"/",edits[6]),encoding = "GeoJSON",verbose=FALSE)

#Los aportes 
sp.shp1@data
sp.shp2@data
sp.shp3@data
#sp.shp4@data = sp.shp3
#sp.shp5@data Este poligono sustrae el mismo poligono que el sp.shp1
#El umbral sobre el cual se trabajo fue 20% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Grallaria_bangsi_20_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)
plot(sp.shp2,add=T)
#Sustraer areas al modelo (Esteban Botero-Delgadillo, Nicholas Bayly)
polyRemove1<-rasterize(sp.shp1,sp.raster,field=1) #poligono que se elimina del modelo
polyRemove2<-rasterize(sp.shp2,sp.raster,field=1)
sp.raster[Which(polyRemove1==1&!(is.na(sp.raster)))]<-0
sp.raster[Which(polyRemove2==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1, add=T)
plot(sp.shp2, add=T)

#El umbral sobre el cual se trabajo fue 10% 
sp.raster2<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Grallaria_bangsi_10_MAXENT.tif")
plot(sp.raster2)
plot(sp.shp3,add=T)
#Agregar area (Dennys Plazas-Cardona)
polyAdd<-rasterize(sp.shp3[1,],sp.raster2,field=1)
sp.raster2[Which(polyAdd==1)]<-1
plot(sp.raster2)
plot(sp.shp3[1,],add=T)
#Sustraer area al modelo (Dennys Plazas-Cardona)
polyRemove<-rasterize(sp.shp3[2,],sp.raster2,field=1) #poligono que se elimina del modelo
sp.raster2[Which(polyRemove==1&!(is.na(sp.raster2)))]<-0
plot(sp.raster2)
plot(sp.shp3[2,], add=T)

#Cruzamos los modelos de los expertos
sp.raster1 <- sp.raster*sp.raster2
plot(sp.raster1)
sp.raster <- sp.raster1

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 33. Grallaria kaestneri #####
setwd(paste0(wd,"/",folders[29]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Grallaria kaestneri'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
sp.shp2<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
sp.shp2@data
#El umbral sobre el cual se trabajo fue 30% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Grallaria_kaestneri_20_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)
plot(sp.shp2,add=T)
#Recortar por poligono (Dennys Plazas-Cardona)
polyEoo1 <- rasterize(sp.shp1,sp.raster,field=1) 
sp.raster1<-sp.raster*polyEoo1
plot(sp.raster1)
plot(sp.shp1, add=T)
#Recortar por poligono (Juan Miguel Ruiz Ovalle)
polyEoo2 <- rasterize(sp.shp2,sp.raster,field=1) 
sp.raster2<-sp.raster*polyEoo2
plot(sp.raster2)
plot(sp.shp2, add=T)
#Cruzamos los modelos de los expertos
sp.raster <- sp.raster1*sp.raster2
plot(sp.raster)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 34. Grallaria milleri #####
setwd(paste0(wd,"/",folders[30]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Grallaria milleri'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
sp.shp2<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)
sp.shp3<-readOGR(paste0(wd,"/",sp.name,"/",edits[5]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
#sp.shp2@data = sp.shp1@data
sp.shp3@data
#El umbral sobre el cual se trabajo fue 10% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Grallaria_milleri_10_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)
#plot(sp.shp2,add=T) Es el mismo polígono del sp.shp1
#Recortar por poligono (Jeyson Sanabria-Mejia)
polyEoo <- rasterize(sp.shp1,sp.raster,field=1) 
sp.raster1<-sp.raster*polyEoo
plot(sp.raster1)
plot(sp.shp1, add=T)
#El umbral sobre el cual se trabajo fue 20% 
sp.raster2<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Grallaria_milleri_20_MAXENT.tif")
plot(sp.raster2)
plot(sp.shp3,add=T)
#Sustraer area al modelo (Dennys Plazas-Cardona)
polyRemove<-rasterize(sp.shp3,sp.raster2,field=1) #poligono que se elimina del modelo
sp.raster2[Which(polyRemove==1&!(is.na(sp.raster2)))]<-0
plot(sp.raster2)
plot(sp.shp3, add=T)

#Cruzamos los modelos de los expertos
sp.raster <- sp.raster1*sp.raster2
plot(sp.raster)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 35. Grallaria urraoensis #####
sp.name <-'Grallaria urraoensis'
#Expert0: Sergio Chaparro
sp.raster1 <- raster("F:/IMac/IAVH/BioModelos/Aves/Modelos consenso/ediciones_18042022/Grallaria urraoensis/Grallaria_urraoensis_con.tif")
sp.raster<-extend(sp.raster1,raster.ref)
extent(sp.raster)<-extent(raster.ref)
#Recortar por altura
#Cortar por 2500-3000 msnm
new.dem<-extend(dem,sp.raster)
extent(new.dem)<-extent(sp.raster)
sp.raster<-sp.raster*(new.dem>=2500)*(new.dem<=3000)
plot(sp.raster)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)
## 36. Habia cristata #####
setwd(paste0(wd,"/",folders[31]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Habia cristata'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[3]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
#El umbral sobre el cual se trabajo fue 20% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Habia_cristata_20_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)
#plot(sp.shp2,add=T) Es el mismo polígono del sp.shp1
#Recortar por poligono (David Alexander Prieto Torres)
polyEoo <- rasterize(sp.shp1,sp.raster,field=1) 
sp.raster<-sp.raster*polyEoo
plot(sp.raster)
plot(sp.shp1, add=T)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 37. Habia gutturalis #####
setwd(paste0(wd,"/",folders[32]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Habia gutturalis'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
#El umbral sobre el cual se trabajo fue 20% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Habia_gutturalis_20_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)
#Recortar por poligono (David Alexander Prieto Torres)
polyEoo <- rasterize(sp.shp1,sp.raster,field=1) 
sp.raster<-sp.raster*polyEoo
plot(sp.raster)
plot(sp.shp1, add=T)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 38. Hapalopsittaca fuertesi #####
setwd(paste0(wd,"/",folders[33]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Hapalopsittaca fuertesi'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
#El umbral sobre el cual se trabajo fue 30% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Hapalopsittaca_fuertesi_30_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)
#Recortar por poligono (David Alexander Prieto Torres)
polyEoo <- rasterize(sp.shp1,sp.raster,field=1) 
sp.raster<-sp.raster*polyEoo
plot(sp.raster)
plot(sp.shp1, add=T)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 39. Henicorhina negreti #####
setwd(paste0(wd,"/",folders[34]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Henicorhina negreti'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
#El umbral sobre el cual se trabajo fue 30% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Henicorhina_negreti_30_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)
#Sustraer areas al modelo (Angela Echeverry-Galvis)
polyRemove<-rasterize(sp.shp1,sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1, add=T)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 40. NO TIENE EDICIONES (SELECCIÓN UMBRAL) Hypopyrrhus pyrohypogaster #####
setwd(paste0(wd,"/",folders[35]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Hypopyrrhus pyrohypogaster'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[4]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
#SELECCIÓN DE UMBRAL DEL 20%

## 41. Lepidopyga lilliae #####
setwd(paste0(wd,"/",folders[36]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Lepidopyga lilliae'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)
sp.shp2<-readOGR(paste0(wd,"/",sp.name,"/",edits[3]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
#sp.shp2@data
#El umbral sobre el cual se trabajo fue 0% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Lepidopyga_lilliae_0_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)

#Recortar por poligono (David Alexander Prieto Torres)
polyEoo <- rasterize(sp.shp1,sp.raster,field=1) 
sp.raster<-sp.raster*polyEoo
plot(sp.raster)
plot(sp.shp1, add=T)
sp.raster1 <- sp.raster

#La edición anterior incluye esta edición
#El umbral sobre el cual se trabajo fue 20% 
# sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Lepidopyga_lilliae_20_MAXENT.tif")
# plot(sp.raster)
# plot(sp.shp2,add=T)

#Agregar areas al modelo (Hugo Vides)
# polyAdd<-rasterize(sp.shp2,sp.raster,field=1)
# sp.raster[Which(polyAdd==1)]<-1
# plot(sp.raster)
# plot(sp.shp2,add=T)
# sp.raster2 <- sp.raster

#Consenso
# sp.raster_final <- sp.raster1*sp.raster2
# plot(sp.raster_final)
# sp.raster <- sp.raster_final

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 42. Leptotila conoveri #####
setwd(paste0(wd,"/",folders[37]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Leptotila conoveri'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
sp.shp2<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)
sp.shp3<-readOGR(paste0(wd,"/",sp.name,"/",edits[3]),encoding = "GeoJSON",verbose=FALSE)
sp.shp4<-readOGR(paste0(wd,"/",sp.name,"/",edits[4]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
sp.shp2@data
sp.shp3@data
sp.shp4@data
#El umbral sobre el cual se trabajo fue 30% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Leptotila_conoveri_30_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)
#Recortar por poligono (Jeyson Sanabria-Mejia, Carolina Diaz Jaramillo)
polyEoo <- rasterize(sp.shp1,sp.raster,field=1) 
sp.raster<-sp.raster*polyEoo
plot(sp.raster)
plot(sp.shp1, add=T)
#Sustraer areas al modelo (Esteban Botero-Delgadillo, Nicholas Bayly)
polyRemove<-rasterize(sp.shp2,sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp2, add=T)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 43. Lipaugus weberi #####
sp.name <-'Lipaugus weberi'
#Expertos: Sergio Chaparro, David Ocampo
sp.raster1 <- raster("F:/IMac/IAVH/BioModelos/Aves/Modelos consenso/ediciones_18042022/Lipaugus weberi/Lipaugus weberi_con.tif")
sp.raster<-extend(sp.raster1,raster.ref)
extent(sp.raster)<-extent(raster.ref)
#Recortar por altura
#Cortar por 1400-2000 msnm
new.dem<-extend(dem,sp.raster)
extent(new.dem)<-extent(sp.raster)
sp.raster<-sp.raster*(new.dem>=1400)*(new.dem<=2000)
plot(sp.raster)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 44. Macroagelaius subalaris #####
setwd(paste0(wd,"/",folders[38]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Macroagelaius subalaris'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
sp.shp2<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
sp.shp2@data
#El umbral sobre el cual se trabajo fue 30% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Macroagelaius_subalaris_30_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)
plot(sp.shp2,add=T)
#Sustraer areas al modelo (Sergio AndrÃ©s Collazos-Gonzalez)
polyRemove<-rasterize(sp.shp1,sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1, add=T)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 45. NO TIENE EDICIONES (SELECCIÓN UMBRAL) Megascops gilesi #####
setwd(paste0(wd,"/",folders[39]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Megascops gilesi'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
#SELECCIÓN DE UMBRAL DEL 0%

## 46. Melanerpes pulcher #####
sp.name <-'Melanerpes pulcher'
#Expertos: Sergio Chaparro
sp.raster1 <- raster("F:/IMac/IAVH/BioModelos/Aves/Modelos consenso/ediciones_18042022/Melanerpes pulcher/Melanerpes pulcher_con.tif")
sp.raster<-extend(sp.raster1,raster.ref)
extent(sp.raster)<-extent(raster.ref)
#Recortar por altura
#Cortar por 200-1500 msnm
new.dem<-extend(dem,sp.raster)
extent(new.dem)<-extent(sp.raster)
sp.raster<-sp.raster*(new.dem>=200)*(new.dem<=1500)
plot(sp.raster)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 47. Myiarchus apicalis #####
setwd(paste0(wd,"/",folders[41]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Myiarchus apicalis'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)
sp.shp2<-readOGR(paste0(wd,"/",sp.name,"/",edits[5]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
sp.shp2@data
#El umbral sobre el cual se trabajo fue 20% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Myiarchus_apicalis_20_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)
#plot(sp.shp2,add=T): esta edición de incluye en la anterior
#Sustraer areas al modelo (David Alexander Prieto Torres)
polyRemove<-rasterize(sp.shp1,sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1, add=T)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 48. Myioborus flavivertex #####
setwd(paste0(wd,"/",folders[42]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Myioborus flavivertex'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
sp.shp2<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)
sp.shp3<-readOGR(paste0(wd,"/",sp.name,"/",edits[4]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes: el sp.shp2 incluye las demás ediciones 
#sp.shp1@data
sp.shp2@data
#sp.shp3@data
#El umbral sobre el cual se trabajo fue 20% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Myioborus_flavivertex_20_MAXENT.tif")
plot(sp.raster)
plot(sp.shp2,add=T)
#Sustraer areas al modelo (Esteban Botero-Delgadillo, Maria  Angela Echeverry-Galvis, Nicholas Bayly)
polyRemove<-rasterize(sp.shp2,sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp2, add=T)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 49. Myiotheretes pernix #####
setwd(paste0(wd,"/",folders[43]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Myiotheretes pernix'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
#El umbral sobre el cual se trabajo fue 20% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Myiotheretes_pernix_20_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)
#Sustraer areas al modelo (Esteban Botero-Delgadillo)
polyRemove<-rasterize(sp.shp1,sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1, add=T)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 50. Myiothlypis basilica #####
setwd(paste0(wd,"/",folders[44]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Myiothlypis basilica'
#sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)
sp.shp2<-readOGR(paste0(wd,"/",sp.name,"/",edits[3]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
#sp.shp1@data
sp.shp2@data
#El umbral sobre el cual se trabajo fue 20% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Myiothlypis_basilica_20_MAXENT.tif")
plot(sp.raster)
plot(sp.shp2,add=T)
#Sustraer areas al modelo (Esteban Botero-Delgadillo)
polyRemove<-rasterize(sp.shp2,sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp2, add=T)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 51. Myiothlypis conspicillata #####
setwd(paste0(wd,"/",folders[45]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Myiothlypis conspicillata'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)
sp.shp2<-readOGR(paste0(wd,"/",sp.name,"/",edits[3]),encoding = "GeoJSON",verbose=FALSE)
sp.shp3<-readOGR(paste0(wd,"/",sp.name,"/",edits[4]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
sp.shp2@data
sp.shp3@data
#El umbral sobre el cual se trabajo fue 20% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Myiothlypis_conspicillata_20_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)
plot(sp.shp2,add=T)
plot(sp.shp3,add=T)
#Sustraer areas al modelo (Esteban Botero-Delgadillo)
polyRemove<-rasterize(sp.shp2,sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp2, add=T)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 52. Odontophorus hyperythrus #####
setwd(paste0(wd,"/",folders[46]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Odontophorus hyperythrus'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)
sp.shp2<-readOGR(paste0(wd,"/",sp.name,"/",edits[5]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
sp.shp2@data
#El umbral sobre el cual se trabajo fue 10% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Odontophorus_hyperythrus_10_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)
plot(sp.shp2,add=T)
#Recortar por poligono (Marcia Carolina Munoz)
polyEoo <- rasterize(sp.shp2,sp.raster,field=1) 
sp.raster<-sp.raster*polyEoo
plot(sp.raster)
plot(sp.shp2, add=T)
#Sustraer areas al modelo (Maria  Angela Echeverry-Galvis)
polyRemove<-rasterize(sp.shp1[1,],sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1[1,], add=T)
#Agregar areas al modelo (Maria  Angela Echeverry-Galvis)
polyAdd<-rasterize(sp.shp1[2,],sp.raster,field=1)
sp.raster[Which(polyAdd==1)]<-1
plot(sp.raster)
plot(sp.shp1[2,],add=T)
#Recortar por altura
#la especie va desde 1300 hasta aproximadamente los 2600 msnm
new.dem<-extend(dem,sp.raster)
extent(new.dem)<-extent(sp.raster)
sp.raster<-sp.raster*(new.dem>=1300)*(new.dem<=2600)
plot(sp.raster)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 53. Odontophorus strophium #####
setwd(paste0(wd,"/",folders[47]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Odontophorus strophium'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
#El umbral sobre el cual se trabajo fue 30% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Odontophorus_strophium_30_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)
#Recortar por poligono (Juan Miguel Ruiz Ovalle)
polyEoo <- rasterize(sp.shp1,sp.raster,field=1) 
sp.raster<-sp.raster*polyEoo
plot(sp.raster)
plot(sp.shp1, add=T)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 54. Ortalis columbiana #####
setwd(paste0(wd,"/",folders[48]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Ortalis columbiana'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[5]),encoding = "GeoJSON",verbose=FALSE)
sp.shp2<-readOGR(paste0(wd,"/",sp.name,"/",edits[6]),encoding = "GeoJSON",verbose=FALSE)
sp.shp3<-readOGR(paste0(wd,"/",sp.name,"/",edits[8]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
sp.shp2@data
sp.shp3@data
#El umbral sobre el cual se trabajo fue 20% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Ortalis_columbiana_20_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)
plot(sp.shp2,add=T)
plot(sp.shp3,add=T)
#Recortar por poligono (Marcia Carolina Munoz)
polyEoo <- rasterize(sp.shp2,sp.raster,field=1) 
sp.raster<-sp.raster*polyEoo
plot(sp.raster)
plot(sp.shp2, add=T)
sp.raster1 <- sp.raster

#El umbral sobre el cual se trabajo fue 20% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Ortalis_columbiana_20_MAXENT.tif")
#Recortar por poligono (Carolina Diaz Jaramillo)
polyEoo <- rasterize(sp.shp3,sp.raster,field=1) 
sp.raster<-sp.raster*polyEoo
plot(sp.raster)
plot(sp.shp3, add=T)
sp.raster2 <- sp.raster

#Consenso
sp.raster<-sp.raster1*sp.raster2
plot(sp.raster)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 55. Ortalis garrula #####
setwd(paste0(wd,"/",folders[49]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Ortalis garrula'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
sp.shp2<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)
sp.shp3<-readOGR(paste0(wd,"/",sp.name,"/",edits[4]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
sp.shp2@data
sp.shp3@data
#El umbral sobre el cual se trabajo fue 10% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Ortalis_garrula_10_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)
#Sustraer areas al modelo (David Alexander Prieto Torres)
polyRemove1<-rasterize(sp.shp1,sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove1==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1, add=T)

plot(sp.shp2[c(1,4),],add=T)
#Sustraer areas al modelo (Hugo Vides)
polyRemove2<-rasterize(sp.shp2[c(1,4),],sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove2==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp2[c(1,4),], add=T)

plot(sp.shp2[c(2:3,5),],add=T)
#Agregar areas al modelo (Hugo Vides)
polyAdd<-rasterize(sp.shp2[c(2:3,5),],sp.raster,field=1)
sp.raster[Which(polyAdd==1)]<-1
plot(sp.raster)
plot(sp.shp2[c(2:3,5),],add=T)

sp.raster1 <- sp.raster

#El umbral sobre el cual se trabajo fue 30% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Ortalis_garrula_30_MAXENT.tif")
plot(sp.raster)
plot(sp.shp3,add=T)

#Recortar por poligono (Carolina Diaz Jaramillo)
polyEoo <- rasterize(sp.shp3,sp.raster,field=1) 
sp.raster<-sp.raster*polyEoo
plot(sp.raster)
plot(sp.shp3, add=T)

sp.raster2<-sp.raster

#Consenso
sp.raster <- sp.raster1*sp.raster2
plot(sp.raster)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 56. Oxypogon cyanolaemus #####
sp.name <- 'Oxypogon cyanolaemus'
sp.shp1 <- readOGR("F:/IMac/IAVH/BioModelos/Aves/Modelos consenso/ediciones_18042022/Oxypogon cyanolaemus/Oxypogon cyanolaemus.shp")
plot(sp.shp1)
sp.raster<-rasterize(sp.shp1,raster.ref,field=1)
plot(sp.raster)
#Recortar por altura
#Cortar por 3200-4600 msnm
new.dem<-extend(dem,sp.raster)
extent(new.dem)<-extent(sp.raster)
sp.raster<-sp.raster*(new.dem>=3200)*(new.dem<=4600)
plot(sp.raster)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 57. Oxypogon guerinii #####
setwd(paste0(wd,"/",folders[50]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Oxypogon guerinii'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
sp.shp2<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
sp.shp2@data
#El umbral sobre el cual se trabajo fue 20% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Oxypogon_guerinii_20_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)
#Recortar por poligono (Alejandra Gonzalez Moncada)
polyEoo <- rasterize(sp.shp1,sp.raster,field=1) 
sp.raster<-sp.raster*polyEoo
plot(sp.raster)
plot(sp.shp1, add=T)

sp.raster1<-sp.raster

#El umbral sobre el cual se trabajo fue 30% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Oxypogon_guerinii_30_MAXENT.tif")
plot(sp.raster)
plot(sp.shp2,add=T)
#Recortar por poligono (Juan Miguel Ruiz Ovalle)
polyEoo <- rasterize(sp.shp2,sp.raster,field=1) 
sp.raster<-sp.raster*polyEoo
plot(sp.raster)
plot(sp.shp2, add=T)

sp.raster2<-sp.raster

#Consenso
sp.raster<-sp.raster1*sp.raster2
plot(sp.raster)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 58. Oxypogon stuebelii #####
setwd(paste0(wd,"/",folders[51]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Oxypogon stuebelii'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
#El umbral sobre el cual se trabajo fue 20% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Oxypogon_stuebelii_20_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)
#Recortar por poligono (Maria  Angela Echeverry-Galvis)
polyEoo <- rasterize(sp.shp1[2,],sp.raster,field=1) 
sp.raster<-sp.raster*polyEoo
plot(sp.raster)
plot(sp.shp1[2,], add=T)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 59. Penelope perspicax #####
setwd(paste0(wd,"/",folders[52]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Penelope perspicax'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)
sp.shp2<-readOGR(paste0(wd,"/",sp.name,"/",edits[3]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
sp.shp2@data
#El umbral sobre el cual se trabajo fue 20% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Penelope_perspicax_20_MAXENT.tif")
plot(sp.raster)
#plot(sp.shp1,add=T) Son la misma acción. Se toma la realizada más recientemente
plot(sp.shp2,add=T)
#Sustraer areas al modelo (Marcia Carolina Munoz)
polyRemove<-rasterize(sp.shp2,sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp2, add=T)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 60. NO TIENE EDICIONES (SELECCIÓN UMBRAL) Phylloscartes lanyoni #####
setwd(paste0(wd,"/",folders[53]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Phylloscartes lanyoni'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
#SELECCIÓN DE UMBRAL DEL 30%

## 61. NO TIENE EDICIONES (SELECCIÓN UMBRAL) Picumnus granadensis #####
setwd(paste0(wd,"/",folders[54]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Picumnus granadensis'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[3]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
#SELECCIÓN DE UMBRAL DEL 20%

## 62. NO TIENE EDICIONES (SELECCIÓN UMBRAL) Podiceps andinus #####
setwd(paste0(wd,"/",folders[55]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Podiceps andinus'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
#SELECCIÓN DE UMBRAL DEL 20%

## 63. Pyrrhura calliptera #####
setwd(paste0(wd,"/",folders[56]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Pyrrhura calliptera'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
#El umbral sobre el cual se trabajo fue 30% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Pyrrhura_calliptera_30_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)
#Sustraer areas al modelo (Esteban Botero-Delgadillo)
polyRemove<-rasterize(sp.shp1,sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1, add=T)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 64. Pyrrhura viridicata #####
setwd(paste0(wd,"/",folders[57]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Pyrrhura viridicata'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
#El umbral sobre el cual se trabajo fue 20% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Pyrrhura_viridicata_20_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)
#Agregar areas al modelo (Esteban Botero-Delgadillo)
polyAdd<-rasterize(sp.shp1,sp.raster,field=1)
sp.raster[Which(polyAdd==1)]<-1
plot(sp.raster)
plot(sp.shp1,add=T)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 65. NO TIENE EDICIONES (SELECCIÓN UMBRAL) Rallus semiplumbeus #####
setwd(paste0(wd,"/",folders[58]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Rallus semiplumbeus'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[4]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
#SELECCIÓN DE UMBRAL DEL 30%

## 66. Ramphomicron dorsale #####
setwd(paste0(wd,"/",folders[59]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Ramphomicron dorsale'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
#El umbral sobre el cual se trabajo fue 10% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Ramphomicron_dorsale_10_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)
#Recortar por poligono (Maria  Angela Echeverry-Galvis)
polyEoo <- rasterize(sp.shp1,sp.raster,field=1) 
sp.raster<-sp.raster*polyEoo
plot(sp.raster)
plot(sp.shp1, add=T)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 67. Saucerottia castaneiventris #####
setwd(paste0(wd,"/",folders[60]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Saucerottia castaneiventris'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
#El umbral sobre el cual se trabajo fue 20% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Saucerottia_castaneiventris_20_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)
#Sustraer areas al modelo (Sergio Andres Collazos-Gonzalez)
polyRemove<-rasterize(sp.shp1,sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1, add=T)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)


## 68. Saucerottia cyanifrons #####
sp.name <-'Saucerottia cyanifrons'
#Expert0: Sergio Chaparro
sp.raster1 <- raster("F:/IMac/IAVH/BioModelos/Aves/Modelos consenso/ediciones_18042022/Saucerottia cyanifrons/Saucerottia_cyanifrons_conAdd.tif")
sp.raster<-extend(sp.raster1,raster.ref)
extent(sp.raster)<-extent(raster.ref)
#Recortar por altura
#Cortar por 400-2100 msnm
new.dem<-extend(dem,sp.raster)
extent(new.dem)<-extent(sp.raster)
sp.raster<-sp.raster*(new.dem>=400)*(new.dem<=2100)
plot(sp.raster)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)
## 69. Scytalopus alvarezlopezi #####
sp.name <-'Scytalopus alvarezlopezi'
#Expert0: Sergio Chaparro
sp.raster1 <- raster("F:/IMac/IAVH/BioModelos/Aves/Modelos consenso/ediciones_18042022/Scytalopus alvarezlopezi/Scytalopus alvarezlopezi_con.tif")
sp.raster<-extend(sp.raster1,raster.ref)
extent(sp.raster)<-extent(raster.ref)
#Recortar por altura
#Cortar por 1300-1800 msnm
new.dem<-extend(dem,sp.raster)
extent(new.dem)<-extent(sp.raster)
sp.raster<-sp.raster*(new.dem>=1300)*(new.dem<=1800)
plot(sp.raster)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 70. Scytalopus canus #####
sp.name <-'Scytalopus canus'
#Expert0: Sergio Chaparro
sp.raster1 <- raster("F:/IMac/IAVH/BioModelos/Aves/Modelos consenso/ediciones_18042022/Scytalopus canus/Scytalopus canus_con.tif")
sp.raster<-extend(sp.raster1,raster.ref)
extent(sp.raster)<-extent(raster.ref)
#Recortar por altura
#Cortar por 2800-3200 msnm
new.dem<-extend(dem,sp.raster)
extent(new.dem)<-extent(sp.raster)
sp.raster<-sp.raster*(new.dem>=2800)*(new.dem<=3200)
plot(sp.raster)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 71. Scytalopus latebricola#####
sp.name <-'Scytalopus latebricola'
#Expert0: Sergio Chaparro
sp.raster1 <- raster("F:/IMac/IAVH/BioModelos/Aves/Modelos consenso/ediciones_18042022/Scytalopus latebricola/Scytalopus latebricola_con.tif")
sp.raster<-extend(sp.raster1,raster.ref)
extent(sp.raster)<-extent(raster.ref)
#Recortar por altura
#Cortar por 2000-3200 msnm
new.dem<-extend(dem,sp.raster)
extent(new.dem)<-extent(sp.raster)
sp.raster<-sp.raster*(new.dem>=2000)*(new.dem<=3200)
plot(sp.raster)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 72. Scytalopus rodriguezi#####
sp.name <-'Scytalopus rodriguezi'
#Expert0: Sergio Chaparro
sp.raster1 <- raster("F:/IMac/IAVH/BioModelos/Aves/Modelos consenso/ediciones_18042022/Scytalopus rodriguezi/Scytalopus rodriguezi_con.tif")
sp.raster<-extend(sp.raster1,raster.ref)
extent(sp.raster)<-extent(raster.ref)
#Recortar por altura
#Cortar por 1400-2000 msnm
new.dem<-extend(dem,sp.raster)
extent(new.dem)<-extent(sp.raster)
sp.raster<-sp.raster*(new.dem>=1400)*(new.dem<=2000)
plot(sp.raster)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 73. Scytalopus sanctaemartae#####
setwd(paste0(wd,"/",folders[61]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Scytalopus sanctaemartae'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
sp.shp2<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)
sp.shp3<-readOGR(paste0(wd,"/",sp.name,"/",edits[3]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
sp.shp2@data
sp.shp3@data
#El umbral sobre el cual se trabajo fue 10% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Scytalopus_sanctaemartae_10_MAXENT.tif")
plot(sp.raster)
plot(sp.shp2,add=T)
#Recortar por poligono (David Alexander Prieto Torres)
polyEoo <- rasterize(sp.shp2,sp.raster,field=1) 
sp.raster<-sp.raster*polyEoo
plot(sp.raster)
plot(sp.shp1, add=T)
sp.raster1 <- sp.raster

#El umbral sobre el cual se trabajo fue 30% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Scytalopus_sanctaemartae_30_MAXENT.tif")
plot(sp.raster)
plot(sp.shp3,add=T)
#Sustraer areas al modelo (Nicholas Bayly)
polyRemove<-rasterize(sp.shp3,sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp3, add=T)
sp.raster2<- sp.raster

#Consenso
sp.raster<-sp.raster1*sp.raster2
plot(sp.raster)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 74. Scytalopus stilesi#####
sp.name <-'Scytalopus stilesi'
#Expert0: Sergio Chaparro
sp.raster1 <- raster("F:/IMac/IAVH/BioModelos/Aves/Modelos consenso/ediciones_18042022/Scytalopus stilesi/Scytalopus stilesi_con.tif")
sp.raster<-extend(sp.raster1,raster.ref)
extent(sp.raster)<-extent(raster.ref)
#Recortar por altura
#Cortar por 1400-2200 msnm
new.dem<-extend(dem,sp.raster)
extent(new.dem)<-extent(sp.raster)
sp.raster<-sp.raster*(new.dem>=1400)*(new.dem<=2200)
plot(sp.raster)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 75. Synallaxis fuscorufa#####
setwd(paste0(wd,"/",folders[62]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Synallaxis fuscorufa'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
sp.shp2<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
sp.shp2@data
#El umbral sobre el cual se trabajo fue 30% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Synallaxis_fuscorufa_30_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)
#Sustraer areas al modelo (Esteban Botero-Delgadillo, Nicholas Bayly)
polyRemove<-rasterize(sp.shp1[c(1,3),],sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1[c(1,3),], add=T)
#Agregar areas al modelo (Esteban Botero-Delgadillo)
polyAdd<-rasterize(sp.shp1[2,],sp.raster,field=1)
sp.raster[Which(polyAdd==1)]<-1
plot(sp.raster)
plot(sp.shp1[2,],add=T)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 76. Synallaxis subpudica#####
sp.name <-'Synallaxis subpudica'
#Expert0: Sergio Chaparro y David Ocampo
sp.raster <- raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Synallaxis_subpudica_20_MAXENT.tif")
#Recortar por altura
#Cortar por 1500-3500 msnm
new.dem<-extend(dem,sp.raster)
extent(new.dem)<-extent(sp.raster)
sp.raster<-sp.raster*(new.dem>=1500)*(new.dem<=3500)
plot(sp.raster)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 77. Thryophilus nicefori#####
setwd(paste0(wd,"/",folders[64]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Thryophilus nicefori'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[2]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
#El umbral sobre el cual se trabajo fue 20% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Thryophilus_nicefori_20_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)
#Sustraer areas al modelo (Sergio Andres Collazos-Gonzalez)
polyRemove<-rasterize(sp.shp1,sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1, add=T)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 78. Thryophilus sernai#####
setwd(paste0(wd,"/",folders[65]))
edits <- list.files(patter=".json$")
edits
sp.name <-'Thryophilus sernai'
sp.shp1<-readOGR(paste0(wd,"/",sp.name,"/",edits[1]),encoding = "GeoJSON",verbose=FALSE)
#Los aportes 
sp.shp1@data
#El umbral sobre el cual se trabajo fue 10% 
sp.raster<-raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Thryophilus_sernai_10_MAXENT.tif")
plot(sp.raster)
plot(sp.shp1,add=T)
#Sustraer areas al modelo (Andres Chinome)
polyRemove<-rasterize(sp.shp1[c(1:3,5:8),],sp.raster,field=1) #poligono que se elimina del modelo
sp.raster[Which(polyRemove==1&!(is.na(sp.raster)))]<-0
plot(sp.raster)
plot(sp.shp1[c(1:3,5:8),], add=T)
#Agregar areas al modelo (Andres Chinome)
polyAdd<-rasterize(sp.shp1[4,],sp.raster,field=1)
sp.raster[Which(polyAdd==1)]<-1
plot(sp.raster)
plot(sp.shp1[4,],add=T)
#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 79. Troglodytes monticola#####
sp.name <-'Troglodytes monticola'
#Expert0: Sergio Chaparro y David Ocampo
sp.raster <- raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Troglodytes_monticola_10_MAXENT.tif")
#Recortar por altura
#Cortar por 3000-4000 msnm
new.dem<-extend(dem,sp.raster)
extent(new.dem)<-extent(sp.raster)
sp.raster<-sp.raster*(new.dem>=3000)*(new.dem<=4000)
plot(sp.raster)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 80. Bucco noanamae
sp.name <-'Bucco noanamae'
#Experto: Sergio Chaparro
sp.raster1 <- raster("F:/IMac/IAVH/BioModelos/Aves/Modelos consenso/ediciones_18042022/Bucco noanamae/Bucco noanamae_con.tif")
sp.raster<-extend(sp.raster1,raster.ref)
extent(sp.raster)<-extent(raster.ref)
#Recortar por altura
#Cortar por 0-300 msnm
new.dem<-extend(dem,sp.raster)
extent(new.dem)<-extent(sp.raster)
sp.raster<-sp.raster*(new.dem>=0)*(new.dem<=300)
plot(sp.raster)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 81. Psarocolius cassini
sp.name <-'Psarocolius cassini'
#Experto: Sergio Chaparro
sp.raster1 <- raster("F:/IMac/IAVH/BioModelos/Aves/Modelos consenso/ediciones_18042022/Psarocolius cassini/Psarocolius cassini_con.tif")
sp.raster<-extend(sp.raster1,raster.ref)
extent(sp.raster)<-extent(raster.ref)
#Recortar por altura
#Cortar por 0-400 msnm
new.dem<-extend(dem,sp.raster)
extent(new.dem)<-extent(sp.raster)
sp.raster<-sp.raster*(new.dem>=0)*(new.dem<=400)
plot(sp.raster)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)

## 82. Henicorhina anachoreta
sp.name <-'Henicorhina anachoreta'
#Experto: Sergio Chaparro
#Se escogió el umbral del 30% y se cortó por altura
sp.raster <- raster("//192.168.11.113/Lab_biogeografia2/Modelos/2021/Aves endemicas/Henicorhina_anachoreta_30_MAXENT.tif")
#Recortar por altura
#Cortar por 2000-4000 msnm
new.dem<-extend(dem,sp.raster)
extent(new.dem)<-extent(sp.raster)
sp.raster<-sp.raster*(new.dem>=2000)*(new.dem<=4000)
plot(sp.raster)

#Finalmente, recortamos por el area continental de Colombia y obtenemos el mapa consenso final para la especie
sp.raster<-sp.raster*col
sp.raster[Which(sp.raster>0)]<-1
sp.raster[Which(sp.raster==0)]<-NA
plot(sp.raster)

writeRaster(sp.raster, paste0(outpath,"/",sp.name,"_con.tif"),overwrite=T)
