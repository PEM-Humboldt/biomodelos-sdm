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
output_dir <- "modelos_N1/aves_media_evaluacion"

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
alt<-crop(alt, y=c(xmin=-83, xmax=-60, ymin=-14, ymax=13))


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

########################
####  Edicion Modelos
#######################

#------------------------
# BAJA_REEDITADOS

# Penelope perspicax

Pen_per <- rast("modelos_to_edit/aves_media_evaluacion/baja_reeditados/Penelope perspicax_con/Penelope perspicax_con.tif")
edit1 <- vect("modelos_to_edit/aves_media_evaluacion/baja_reeditados/Penelope perspicax_con/remover_popayan.shp",
              crs = crs(Pen_per))

Pen_per  <- Pen_per |> mask(edit1, inverse = T)
names(Pen_per) <- "Penelope perspicax"

writeRaster(Pen_per, paste0(output_dir, "/Penelope_perspicax_con.tif"), datatype = "INT2S", overwrite = TRUE)

# Scytalopus rodriguezi

Scy_rod <- rast("modelos_to_edit/aves_media_evaluacion/baja_reeditados/Scytalopus rodriguezi_con/Scytalopus rodriguezi_con.tif")
edit1 <- vect("modelos_to_edit/aves_media_evaluacion/baja_reeditados/Scytalopus rodriguezi_con/adicionar_yariguies.shp",
              crs = crs(Scy_rod)) |> rasterize(Scy_rod)
edit1[is.na(edit1)] <- 0

Scy_rod[is.na(Scy_rod)] <- 0

Scy_rod <- Scy_rod + edit1
Scy_rod[Scy_rod == 0] <- NA
Scy_rod[Scy_rod == 2] <- 1

names(Scy_rod) <- "Scytalopus rodriguezi"

writeRaster(Scy_rod, paste0(output_dir, "/Scytalopus_ rodriguezi_con.tif"), datatype = "INT2S", overwrite = TRUE)

#------------------------
# DIFICULTAD 1

ediciones <- read.csv("modelos_to_edit/aves_media_evaluacion/dificultad_1/ediciones.csv", check.names = F)

# Bangsia aureocincta

ed <- ediciones[which(ediciones$`'species_id'` == 552), 7]

Ban_aur <- rast("modelos_to_edit/aves_media_evaluacion/dificultad_1/08082022_Bangsia aureocincta_con/08082022_Bangsia aureocincta_con.tif")
other <- lapply(ed, vect, crs = crs(Ban_aur )) |> vect()

Ban_aur  <- Ban_aur |> mask(other, inverse = T)
names(Ban_aur) <- "Bangsia aureocincta"

writeRaster(Ban_aur , paste0(output_dir, "/Bangsia_aureocincta_con.tif"), datatype = "INT2S", overwrite = TRUE)

# Cercomacroides parkeri

ed <- ediciones[which(ediciones$`'species_id'` == 979), 7]

Cer_par <- rast("modelos_to_edit/aves_media_evaluacion/dificultad_1/08082022_Cercomacroides parkeri_con/08082022_Cercomacroides parkeri_con.tif")
edit1 <- vect("modelos_to_edit/aves_media_evaluacion/dificultad_1/08082022_Cercomacroides parkeri_con/sp-979_usr-398_thr-30.geojson")
edit2 <- vect("modelos_to_edit/aves_media_evaluacion/dificultad_1/08082022_Cercomacroides parkeri_con/sp-979_usr-1648_thr-30.geojson")
other <- lapply(ed, vect, crs = crs(Cer_par)) |> vect()

Cer_par <- Cer_par |> mask(edit1, inverse = T) |> mask(edit2, inverse = T) |> mask(other, inverse = T)
names(Cer_par) <- "Cercomacroides parkeri"

writeRaster(Cer_par, paste0(output_dir, "/Cercomacroides_parkeri_con.tif"), datatype = "INT2S", overwrite = TRUE)

# Cistothorus apolinari

ed <- ediciones[which(ediciones$`'species_id'` == 1110), 7]

Cis_apo <- rast("modelos_to_edit/aves_media_evaluacion/dificultad_1/08082022_Cistothorus apolinari_con/08082022_Cistothorus apolinari_con.tif")
edit1 <- vect("modelos_to_edit/aves_media_evaluacion/dificultad_1/08082022_Cistothorus apolinari_con/sp-1110_usr-366_thr-C.geojson")
edit2 <- vect("modelos_to_edit/aves_media_evaluacion/dificultad_1/08082022_Cistothorus apolinari_con/sp-1110_usr-1648_thr-30.geojson")
other <- lapply(ed, vect, crs = crs(Cis_apo)) |> vect() 

Cis_apo <- Cis_apo |> mask(edit1, inverse = T) |> mask(edit2, inverse = T) |> mask(other, inverse = T)
names(Cis_apo) <- "Cistothorus apolinari"

writeRaster(Cis_apo, paste0(output_dir, "/Cistothorus_apolinari_con.tif"), datatype = "INT2S", overwrite = TRUE)

# Coeligena orina

ed <- ediciones[which(ediciones$`'species_id'` == 8478), 7]

Coe_ori <- rast("modelos_to_edit/aves_media_evaluacion/dificultad_1/08082022_Coeligena orina/Coeligena.orina_20_MAXENT.tif")
edit1 <- vect("modelos_to_edit/aves_media_evaluacion/dificultad_1/08082022_Coeligena orina/remover_coord_central_flancoOccOcc.shp", 
              crs = crs(Coe_ori))

Coe_ori <- Coe_ori |> mask(edit1, inverse = T) |> extend(alt)
Coe_ori[Coe_ori == 0] <- NA  
names(Coe_ori) <- "Coeligena orina"

writeRaster(Coe_ori, paste0(output_dir, "/Coeligena_orina_con.tif"), datatype = "INT2S", overwrite = TRUE)

# Coeligena prunellei

ed <- ediciones[which(ediciones$`'species_id'` == 1188), 7]

Coe_pru <- rast("modelos_to_edit/aves_media_evaluacion/dificultad_1/08082022_Coeligena prunellei_con/08082022_Coeligena prunellei_con.tif")
edit1 <- vect("modelos_to_edit/aves_media_evaluacion/dificultad_1/08082022_Coeligena prunellei_con/sp-1188_usr-398_thr-30.geojson")
other <- lapply(ed, vect, crs = crs(Coe_pru)) |> vect() 

Coe_pru <- Coe_pru |> mask(edit1, inverse = T) |> mask(other, inverse = T)
names(Coe_pru) <- "Coeligena prunellei"

writeRaster(Coe_pru, paste0(output_dir, "/Coeligena_prunellei_con.tif"), datatype = "INT2S", overwrite = TRUE)

# Grallaria bangsi

ed <- ediciones[which(ediciones$`'species_id'` == 2011), 7]

Gra_ban <- rast("modelos_to_edit/aves_media_evaluacion/dificultad_1/08082022_Grallaria bangsi/Grallaria_bangsi_10_MAXENT.tif")
edit1 <- vect("modelos_to_edit/aves_media_evaluacion/dificultad_1/08082022_Grallaria bangsi/remover_perija.shp", 
              crs = crs(Gra_ban))

Gra_ban <- Gra_ban |> mask(edit1, inverse = T) |> extend(alt)
Gra_ban[Gra_ban == 0] <- NA  
names(Gra_ban) <- "Grallaria bangsi"

writeRaster(Gra_ban, paste0(output_dir, "/Grallaria_bangsi_con.tif"), datatype = "INT2S", 
            overwrite = TRUE)

# Grallaria milleri

ed <- ediciones[which(ediciones$`'species_id'` == 2016), 7]

Gra_mil <- rast("modelos_to_edit/aves_media_evaluacion/dificultad_1/08082022_Grallaria milleri_con/08082022_Grallaria milleri_con.tif")
edit1 <- vect("modelos_to_edit/aves_media_evaluacion/dificultad_1/08082022_Grallaria milleri_con/sp-2016_usr-1648_thr-30.geojson")
other <- lapply(ed, vect, crs = crs(Gra_mil)) |> vect() 

Gra_mil <- Gra_mil |> mask(edit1, inverse = T) |> mask(other, inverse = T)
names(Gra_mil) <- "Grallaria milleri"

writeRaster(Gra_mil, paste0(output_dir, "/Grallaria_milleri_con.tif"), datatype = "INT2S", overwrite = TRUE)

# Habia gutturalis

ed <- ediciones[which(ediciones$`'species_id'` == 2102), 7]

Hab_gut <- rast("modelos_to_edit/aves_media_evaluacion/dificultad_1/08082022_Habia gutturalis_con/08082022_Habia gutturalis_con.tif")
edit1 <- vect("modelos_to_edit/aves_media_evaluacion/dificultad_1/08082022_Habia gutturalis_con/sp-2102_usr-398_thr-20.geojson")
other <- lapply(ed, vect, crs = crs(Hab_gut )) |> vect() 

Hab_gut <- Hab_gut |> mask(edit1, inverse = T) |> mask(other, inverse = T)
names(Hab_gut) <- "Habia gutturalis"

writeRaster(Hab_gut, paste0(output_dir, "/Habia_gutturalis_con.tif"), datatype = "INT2S", overwrite = TRUE)

# Henicorhina negreti

ed <- ediciones[which(ediciones$`'species_id'` == 8259), 7]

Hen_neg <- rast("modelos_to_edit/aves_media_evaluacion/dificultad_1/08082022_Henicorhina negreti_con/08082022_Henicorhina negreti_con.tif")
edit1 <- vect("modelos_to_edit/aves_media_evaluacion/dificultad_1/08082022_Henicorhina negreti_con/sp-8259_usr-398_thr-30.geojson")
other <- lapply(ed, vect, crs = crs(Hen_neg)) |> vect() 

Hen_neg <- Hen_neg |> mask(edit1, inverse = T) |> mask(other, inverse = T)
names(Hen_neg) <- "Henicorhina negreti"

writeRaster(Hen_neg, paste0(output_dir, "/Henicorhina_negreti_con.tif"), datatype = "INT2S", overwrite = TRUE)

# Leptotila conoveri

ed <- ediciones[which(ediciones$`'species_id'` == 6993), 7]

Lep_con <- rast("modelos_to_edit/aves_media_evaluacion/dificultad_1/08082022_Leptotila conoveri_con/08082022_Leptotila conoveri_con.tif")
edit1 <- vect("modelos_to_edit/aves_media_evaluacion/dificultad_1/08082022_Leptotila conoveri_con/sp-6993_usr-1648_thr-C.geojson")
other <- lapply(ed, vect, crs = crs(Lep_con)) |> vect() 

Lep_con <- Lep_con |> mask(edit1, inverse = T) |> mask(other, inverse = T)
names(Lep_con) <- "Leptotila conoveri"

writeRaster(Lep_con, paste0(output_dir, "/Leptotila_conoveri_con.tif"), datatype = "INT2S", overwrite = TRUE)

# Megascops gilesi

ed <- ediciones[which(ediciones$`'species_id'` == 8293), 7]

Meg_gil <- rast("modelos_to_edit/aves_media_evaluacion/dificultad_1/08082022_Megascops gilesi/Megascops.gilesi_0_MAXENT.tif")

Meg_gil <- Meg_gil |> extend(alt)
Meg_gil[Meg_gil == 0] <- NA  
names(Meg_gil) <- "Megascops gilesi"

writeRaster(Meg_gil, paste0(output_dir, "/Megascops_gilesi_con.tif"), datatype = "INT2S", overwrite = TRUE)

# Podiceps andinus: adicionar poligonos

ed <- ediciones[which(ediciones$`'species_id'` == 8358), 7]

Pod_and <- rast("modelos_to_edit/aves_media_evaluacion/dificultad_1/08082022_Podiceps andinus_con/08082022_Podiceps andinus_con.tif")
edit1 <- vect("modelos_to_edit/aves_media_evaluacion/dificultad_1/08082022_Podiceps andinus_con/sp-8358_usr-398_thr-30.geojson")|>
  rasterize(Pod_and)
edit1[is.na(edit1)] <- 0

edit2 <- vect("modelos_to_edit/aves_media_evaluacion/dificultad_1/08082022_Podiceps andinus_con/sp-8358_usr-398_thr-C.geojson")|>
  rasterize(Pod_and)
edit2[is.na(edit2)] <- 0

otherAd <- lapply(ed[2:length(ed)], vect, crs = crs(Pod_and)) |> vect() |> rasterize(Pod_and)
otherAd[is.na(otherAd)] <- 0

Pod_and[is.na(Pod_and)] <- 0

Pod_and <- Pod_and + edit1 + edit2 + otherAd

Pod_and[Pod_and == 0] <- NA
Pod_and[Pod_and == 2] <- 1

otherRem <- ed[1] |> vect(crs = crs(Pod_and))
Pod_and <- Pod_and |> mask(otherRem)

names(Pod_and) <- "Podiceps andinus"

writeRaster(Pod_and, paste0(output_dir, "/Podiceps_andinus_con.tif"), datatype = "INT2S", overwrite = TRUE)

# Scytalopus sanctaemartae

ed <- ediciones[which(ediciones$`'species_id'` == 8480), 7]

Scy_san <- rast("modelos_to_edit/aves_media_evaluacion/dificultad_1/08082022_Scytalopus sanctaemartae/Scytalopus.sanctaemartae_0_MAXENT.tif")

Scy_san <- Scy_san |> extend(alt)
Scy_san[Scy_san == 0] <- NA  
names(Scy_san) <- "Scytalopus sanctaemartae"

writeRaster(Scy_san, paste0(output_dir, "/Scytalopus_sanctaemartae_con.tif"), datatype = "INT2S", overwrite = TRUE)

#--------------------
# DIFICULTAD 2

ediciones <- read.csv("modelos_to_edit/aves_media_evaluacion/dificultad_2_3/ediciones.csv", check.names = F)

# Diglossa gloriosissima

ed <- ediciones[which(ediciones$`'species_id'` == 5565), 7]

Dig_glo <- rast("modelos_to_edit/aves_media_evaluacion/dificultad_2_3/08082022_Diglossa gloriosissima_con/Diglossa.gloriosissima_10_MAXENT.tif") |>
  extend(alt)
edit1 <- vect("modelos_to_edit/aves_media_evaluacion/dificultad_2_3/08082022_Diglossa gloriosissima_con/sp-5565_usr-398_thr-30.geojson")
edit2 <- vect("modelos_to_edit/aves_media_evaluacion/dificultad_2_3/08082022_Diglossa gloriosissima_con/sp-5565_usr-366_thr-10.geojson")
other <- lapply(ed, vect, crs = crs(Dig_glo)) |> vect()

Dig_glo <- Dig_glo |> mask(edit1, inverse = T) |> mask(edit2, inverse = T) |> mask(other, inverse = T)

Dig_glo[Dig_glo == 0] <- NA  

names(Dig_glo) <- "Diglossa gloriosissima"

# limitar entre 2900 y 3800m s.n.m

## minimo y maximo altitud
min_alt_aoi <- terra::minmax(alt)[1,]
max_alt_aoi <- terra::minmax(alt)[2,] 

# minimo de la especie
min_i <- 2900
max_i <- 3800

# tabla para reclasificar la altitud 
breaks_ <- rbind(c(min_alt_aoi, min_i, NA),
                 c(min_i, max_i, 1),
                 c(max_i, max_alt_aoi, NA)
)
alt.dful <- terra::classify(alt, breaks_, include.lowest=TRUE)

# eliminando lo que no se encuentra entre el limite superior y el inferior
a <- mask(alt.dful, Dig_glo)

writeRaster(a, paste0(output_dir, "/Diglossa_gloriosissima_con.tif"), datatype = "INT2S", overwrite = TRUE)

# Drymophila caudata

ed <- ediciones[which(ediciones$`'species_id'` == 1600), 7]

Dry_cau <- rast("modelos_to_edit/aves_media_evaluacion/dificultad_2_3/08082022_Drymophila caudata_con/08082022_Drymophila caudata_con.tif")
edit1 <- vect("modelos_to_edit/aves_media_evaluacion/dificultad_2_3/08082022_Drymophila caudata_con/sp-1600_usr-398_thr-20.geojson")
edit2 <- vect("modelos_to_edit/aves_media_evaluacion/dificultad_2_3/08082022_Drymophila caudata_con/remocion.shp")
other <- lapply(ed, vect, crs = crs(Dry_cau)) |> vect() 

Dry_cau <- Dry_cau |> mask(edit1, inverse = T) |> mask(edit2, inverse = T) |> mask(other, inverse = T)
names(Dry_cau) <- "Drymophila caudata"

writeRaster(Dry_cau, paste0(output_dir, "/Drymophila_caudata_con.tif"), datatype = "INT2S", overwrite = TRUE)

# Hypopyrrhus pyrohypogaster

ed <- ediciones[which(ediciones$`'species_id'` == 2318), 7] 
# se volvieron shapefile dado que algunos poligonos se construyeron a una
# escala muy amplia y cortaba zonas donde no deberia, son los que estan llamados como edit3

Hyp_pyr <- rast("modelos_to_edit/aves_media_evaluacion/dificultad_2_3/08082022_Hypopyrrhus pyrohypogaster_con/08082022_Hypopyrrhus pyrohypogaster_con.tif")
edit1 <- vect("modelos_to_edit/aves_media_evaluacion/dificultad_2_3/08082022_Hypopyrrhus pyrohypogaster_con/sp-2318_usr-1648_thr-30.geojson")
edit2 <- vect("modelos_to_edit/aves_media_evaluacion/dificultad_2_3/08082022_Hypopyrrhus pyrohypogaster_con/adicion.shp")|>
  rasterize(Hyp_pyr)
edit3 <- vect("modelos_to_edit/aves_media_evaluacion/dificultad_2_3/08082022_Hypopyrrhus pyrohypogaster_con/remover.shp")

Hyp_pyr <- Hyp_pyr |> mask(edit1, inverse = T) |> mask(edit3, inverse = T)

# adicionar zonas de huila/tolima

edit2[is.na(edit2)] <- 0
Hyp_pyr[is.na(Hyp_pyr)] <- 0

Hyp_pyr <- Hyp_pyr + edit2

Hyp_pyr[Hyp_pyr == 0] <- NA
Hyp_pyr[Hyp_pyr == 2] <- 1

names(Hyp_pyr) <- "Hypopyrrhus pyrohypogaster"

# limitar entre 1000 y 2800m s.n.m

## minimo y maximo altitud
min_alt_aoi <- terra::minmax(alt)[1,]
max_alt_aoi <- terra::minmax(alt)[2,] 

# minimo de la especie
min_i <- 900
max_i <- 2800

# tabla para reclasificar la altitud 
breaks_ <- rbind(c(min_alt_aoi, min_i, NA),
                 c(min_i, max_i, 1),
                 c(max_i, max_alt_aoi, NA)
)
alt.dful <- terra::classify(alt, breaks_, include.lowest=TRUE)

# eliminando lo que no se encuentra entre el limite superior y el inferior
a <- mask(alt.dful, Hyp_pyr)

writeRaster(a, paste0(output_dir, "/Hypopyrrhus_pyrohypogaster_con.tif"), datatype = "INT2S", overwrite = TRUE)

# Myiothlypis conspicillata

ed <- ediciones[which(ediciones$`'species_id'` == 7115), 7]

Myi_con <- rast("modelos_to_edit/aves_media_evaluacion/dificultad_2_3/08082022_Myiothlypis conspicillata_con/Myiothlypis.conspicillata_0_5002265.tif")

Myi_con <- Myi_con |> extend(alt)
Myi_con[Myi_con == 0] <- NA  
names(Myi_con) <- "Myiothlypis conspicillata"

writeRaster(Myi_con, paste0(output_dir, "/Myiothlypis_conspicillata_con.tif"), datatype = "INT2S", overwrite = TRUE)

# Ortalis garrulla

Ort_garr <- rast("modelos_to_edit/aves_media_evaluacion/dificultad_2_3/080802022_Ortalis garrulla/Ortalis.garrula_0_MAXENT.tif")
edit1 <- vect("modelos_to_edit/aves_media_evaluacion/dificultad_2_3/080802022_Ortalis garrulla/sp-3288_usr-366_thr-0.geojson")
edit2 <- vect("modelos_to_edit/aves_media_evaluacion/dificultad_2_3/080802022_Ortalis garrulla/sp-3288_usr-398_thr-0.geojson")
edit3 <- vect("modelos_to_edit/aves_media_evaluacion/dificultad_2_3/080802022_Ortalis garrulla/sp-3288_usr-1648_thr-0.geojson")

Ort_garr <- Ort_garr |> mask(edit1, inverse = T) |> mask(edit2, inverse = T) |> mask(edit3, inverse = T)

Ort_garr <- Ort_garr |> extend(alt)
Ort_garr[Ort_garr == 0] <- NA  
names(Ort_garr) <- "Ortalis garrula"

writeRaster(Ort_garr, paste0(output_dir, "/Ortalis_garrula_con.tif"), datatype = "INT2S", overwrite = TRUE)

#-------------------------
# DIFICULTAD 3

# Crax alberti
Cra_alb <- rast("modelos_to_edit/aves_media_evaluacion/dificultad_2_3/08082022_Crax alberti_con/08082022_Crax alberti_con.tif") |>
  extend(alt)
edit1 <- sf::read_sf("modelos_to_edit/aves_media_evaluacion/dificultad_2_3/08082022_Crax alberti_con/adicion_BST_carmen.shp")|>
  st_transform(crs(Cra_alb)) |>
  dplyr::mutate(value = 1) |> 
  rasterize(Cra_alb, field = "value")

edit2 <- sf::read_sf("modelos_to_edit/aves_media_evaluacion/dificultad_2_3/08082022_Crax alberti_con/adicion_BST_valledupar.shp")|>
  st_transform(crs(Cra_alb)) |>
  dplyr::mutate(value = 1) |> 
  rasterize(Cra_alb, field = "value")

## adicionar zonas de bosque seco tropical

edit1[is.na(edit1)] <- 0
edit2[is.na(edit2)] <- 0
Cra_alb[is.na(Cra_alb)] <- 0

Cra_alb <- Cra_alb + edit1 + edit2

Cra_alb[Cra_alb == 0] <- NA
Cra_alb[Cra_alb == 2] <- 1

writeRaster(Cra_alb, paste0(output_dir, "/Crax_alberti_con.tif"), datatype = "INT2S", overwrite = TRUE)

# Ortalis columbiana

ed <- ediciones[which(ediciones$`'species_id'` == 8474), 7]

Ort_col <- rast("modelos_to_edit/aves_media_evaluacion/dificultad_2_3/08082022_Ortalis columbiana_con/08082022_Ortalis columbiana_con.tif")
edit1 <- vect("modelos_to_edit/aves_media_evaluacion/dificultad_2_3/08082022_Ortalis columbiana_con/sp-8474_usr-398_thr-30.geojson")
edit2 <- vect("modelos_to_edit/aves_media_evaluacion/dificultad_2_3/08082022_Ortalis columbiana_con/remover_cucuta.shp")
other <- lapply(ed, vect, crs = crs(Ort_col)) |> vect()

Ort_col <- Ort_col |> mask(edit1, inverse = T) |> mask(edit2, inverse = T) |> mask(other, inverse = T)

Ort_col <- Ort_col |> extend(alt)
Ort_col[Ort_col == 0] <- NA  
names(Ort_garr) <- "Ortalis columbiana"

writeRaster(Ort_col, paste0(output_dir, "/Ortalis_columbiana_con.tif"), datatype = "INT2S", overwrite = TRUE)

# Saucerottia cyanifrons

Sau_cya <- rast("modelos_to_edit/aves_media_evaluacion/dificultad_2_3/08082022_Saucerottia cyanifrons_con/08082022_Saucerottia cyanifrons_con.tif")
edit1 <- vect("modelos_to_edit/aves_media_evaluacion/dificultad_2_3/08082022_Saucerottia cyanifrons_con/remover_ocaña_ntSantander.shp")
edit2 <- rast("modelos_to_edit/aves_media_evaluacion/dificultad_2_3/08082022_Saucerottia cyanifrons_con/Saucerottia_florencia_bello_adicion.tif") |>
  extend(alt)

Sau_cya <- Sau_cya |> mask(edit1, inverse = T)

## adicionar zonas de buenavista a bello

edit2[is.na(edit2)] <- 0
Sau_cya[is.na(Sau_cya )] <- 0

Sau_cya <- Sau_cya + edit2

Sau_cya[Sau_cya == 0] <- NA  
names(Sau_cya) <- "Saucerottia cyanifrons"

writeRaster(Sau_cya, paste0(output_dir, "/Saucerottia_cyanifrons_con.tif"), datatype = "INT2S", overwrite = TRUE)

# Scytalopus alvarezlopezi

ed <- ediciones[which(ediciones$`'species_id'` == 8389), 7]

Scy_alv <- rast("modelos_to_edit/aves_media_evaluacion/dificultad_2_3/08082022_Scytalopus alvarezlopezi_con/08082022_Scytalopus alvarezlopezi_con.tif")
edit1 <- vect("modelos_to_edit/aves_media_evaluacion/dificultad_2_3/08082022_Scytalopus alvarezlopezi_con/remocion.shp")
edit2 <- vect("modelos_to_edit/aves_media_evaluacion/dificultad_2_3/08082022_Scytalopus alvarezlopezi_con/sp-8389_usr-1648_thr-20.geojson")
other <- lapply(ed, vect, crs = crs(Scy_alv)) |> vect()

Scy_alv <- Scy_alv |> mask(edit1, inverse = T) |> mask(edit2, inverse = T) |> mask(other, inverse = T)

Scy_alv <- Scy_alv |> extend(alt)
Scy_alv[Scy_alv == 0] <- NA  
names(Scy_alv) <- "Scytalopus alvarezlopezi"

writeRaster(Scy_alv, paste0(output_dir, "/Scytalopus_alvarezlopezi_con.tif"), datatype = "INT2S", overwrite = TRUE)
