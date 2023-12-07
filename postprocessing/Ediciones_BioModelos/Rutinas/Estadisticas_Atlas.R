library(terra)

#############################################################################
## Identificar Unidades político administrativas por pol[i]gono para Atlas ## 
#############################################################################
wd<-('I:/BioModelos/')
setwd(wd)
group<-'G_Roedores' #Nombre del grupo

folder<-(paste0('Especies/MDP_', group))
dir.create(folder)

#MDP
mod1<-rast(list.files(paste0(wd,'Especies/N1/'), full.names= T, recursive = T))
mod2<-rast(list.files(paste0(wd,'Especies/N2/'), full.names= T, recursive = T))

## AOI
depto<- vect("Info_base/Departamentos/Departamentos_full.shp")
nm_depto<-c('Amazonas', 'Antioquia', 'Arauca', 'Atlantico', 'Bolivar', 'Boyaca', 'Caldas', 'Caqueta', 'Casanare', 
            'Cauca', 'Cesar', 'Choco', 'Cordoba', 'Cundinamarca', 'Guainia', 'Guaviare', 'Huila', 'La Guajira', 'Magdalena',
            'Meta', 'Narino', 'Norte de Santander', 'Putumayo', 'Quindio', 'Risaralda', 'San Andres Providencia Y Santa Catalina', 
            'Santander', 'Sucre', 'Tolima', 'Valle Del Cauca', 'Vaupes', 'Vichada')

depto$DEPARTAMEN<-nm_depto

cars<- vect("Info_base/Cars/cars.shp")

#--------------------------------------#
#   Matrix for the potencial species   #
#--------------------------------------#

#Depto
pot_spp<-data.frame(matrix(ncol= (length(depto$DEPARTAMEN)+1), nrow= nlyr(mod1))) 
names(pot_spp)<- c('Especie', nm_depto)
pot_spp$Especie<-names(mod1)
pot_spp$Especie<- gsub('_con', '', pot_spp$Especie)

for (i in 1:length(depto$DEPARTAMEN)) {
  pb   <- txtProgressBar(1, 100, style=3)
    area.i<- depto[i,]
    for (j in 1:nlyr(mod1)) {
      specie.i<- crop(mod1[[j]], area.i)
      specie.i2<-mask(specie.i, area.i)
      if (!is.nan((minmax(specie.i2)[1]))){
        pot_spp[j,paste0(depto$DEPARTAMEN[i])]<-'Presente'
      } else
        pot_spp[j, paste0(depto$DEPARTAMEN[i])]<-'Ausente' 
    }
rm(specie.i, specie.i2)
setTxtProgressBar(pb, ((i*100)/length(depto$DEPARTAMEN)))

}

write.csv(pot_spp, paste0(folder,"/spp_potenciales_depto_", group,".csv"), fileEncoding='UTF-8')

#car's
pot_spp_car<-data.frame(matrix(ncol= (length(cars$ea)+1), nrow= nlyr(mod1))) 
names(pot_spp_car)<- c('Especie', cars$ea)
pot_spp_car$Especie<-names(mod1)
pot_spp_car$Especie<- gsub('_con', '', pot_spp_car$Especie)

i<-1
j<-1

for (i in 1:length(cars$ea)) {
  pb   <- txtProgressBar(1, 100, style=3)
  area.i<- cars[i,]
  for (j in 1:nlyr(mod1)) {
    specie.i<- crop(mod1[[j]], area.i)
    specie.i2<-mask(specie.i, area.i)
    if(!is.nan((minmax(specie.i2)[1]))) {
      pot_spp_car[j, paste0(cars$ea[i])]<-'Presente'
    } else
      pot_spp_car[j, paste0(cars$ea[i])]<-'Ausente' 
  }
rm(specie.i, specie.i2)
setTxtProgressBar(pb, ((i*100)/length(depto$DEPARTAMEN)))
}

rm(mod1, i, j)

write.csv(pot_spp_car, paste0(folder,"/spp_potenciales_cars_", group,".csv"), fileEncoding='UTF-8')


#-----------------------------------#
#   Matrix for the remanent areas   #
#-----------------------------------#

#Depto
rem_areas<-data.frame(matrix(ncol= (length(depto$DEPARTAMEN)+1), nrow= nlyr(mod2))) 
names(rem_areas)<- c('Especie', nm_depto)
rem_areas$Especie<-names(mod1)
rem_areas$Especie<- gsub('_con', '', rem_areas$Especie)

for (i in 1:length(depto$DEPARTAMEN)) {
  pb   <- txtProgressBar(1, 100, style=3)
  area.i<- depto[i,]
  for (j in 1:nlyr(mod1)) {
    specie.i<- crop(mod2[[j]], area.i)
    specie.i2<-mask(specie.i, area.i)
    if (!is.nan((minmax(specie.i2)[1]))){
      rem_areas[j, paste0(depto$DEPARTAMEN[i])]<-'Presente'
    } else
      rem_areas[j, paste0(depto$DEPARTAMEN[i])]<-'Ausente' 
  }
  rm(specie.i, specie.i2)
setTxtProgressBar(pb, ((i*100)/length(depto$DEPARTAMEN)))
}

write.csv(rem_areas, paste0(folder,"/rem_areas_depto_", group,".csv"), fileEncoding='UTF-8')

#car's
rem_areas_car<-data.frame(matrix(ncol= (length(cars$ea)+1), nrow= nlyr(mod2))) 
names(rem_areas_car)<- c('Especie', cars$ea)
rem_areas_car$Especie<-names(mod1)
rem_areas_car$Especie<- gsub('_con', '', rem_areas_car$Especie)

for (i in 1:length(cars$ea)) {
  pb   <- txtProgressBar(1, 100, style=3)
  area.i<- cars[i,]
  for (j in 1:nlyr(mod1)) {
    specie.i<- crop(mod2[[j]], area.i)
    specie.i2<-mask(specie.i, area.i)
    if (!is.nan((minmax(specie.i2)[1]))){
      rem_areas_car[j, paste0(cars$ea[i])]<-'Presente'
    } else
      rem_areas_car[j, paste0(cars$ea[i])]<-'Ausente' 
  }
  rm(specie.i, specie.i2)
  setTxtProgressBar(pb, ((i*100)/length(depto$DEPARTAMEN)))
}

rm(mod2, i, j)

write.csv(rem_areas_car, paste0(folder,"/rem_areas_cars_", group,".csv"), fileEncoding='UTF-8')

