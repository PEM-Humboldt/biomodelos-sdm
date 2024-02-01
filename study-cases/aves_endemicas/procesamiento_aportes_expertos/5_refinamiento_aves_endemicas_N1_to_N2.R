library(data.table)
library(dplyr)
library(stringr)
library(terra)

#--------------------------------------

# lista de archivos raster: modelos N1 validados
lst <- list.files("modelos_N1/aves_N1_validados", pattern = "*.tif$|*.geotiff$", full.names = T, recursive = T)
nm <- list.files("modelos_N1/aves_N1_validados", pattern = "*.tif$|*.geotiff$", full.names = F, recursive = T)

#---------------------------------------

# Cargar las capas CLC (Corine Land Cover) de 2018
clc18 <- list.files("Estadisticas_BioModelos/Info_base/CapasBioModelos/clc_tif_2018/", pattern = ".tif$", full.names = T) |>
  rast() |>
  extend(r)
clc18_id <- list.files("Estadisticas_BioModelos/Info_base/CapasBioModelos/clc_tif_2018/", pattern = ".tif$", full.names = F)
clc18_id <- gsub('_con.tif|.tif', '', clc18_id)
names(clc18) <- clc18_id

#---------------------------------------

# leer coberturas disponibles en biomodelos
clc <- fread("Estadisticas_BioModelos/Info_base/CapasBioModelos/BioModelos2CLC.csv", encoding = "Latin-1") %>% 
  data.frame() %>% 
  select(-IDShape) %>% 
  filter(!duplicated(.))

# Extraer y procesar información desde la matriz de nombres de coberturas
chars <- str_split(clc$Nombre.Cobertura, pattern = "\\. ", simplify = T)

num <- sub("\\.\\d+\\s*$", ".", x = gsub("[^0-9.]", "", clc$Nombre.Cobertura))
num <- gsub(pattern = "\\.", replacement = "", x = num) |> as.numeric()
clc$id <- num

clc$name_clc <- chars[,2] %>% tolower() %>% 
  stringi::stri_trans_general(id = "Latin-ASCII") %>% 
  str_squish()

# existe un problema entre la matriz de nombes de coberturas y las que estan para elegir en 
# biomodelos: " con " es " y "
index_ <- clc$name_clc == "mosaico de cultivos con espacios naturales"| clc$name_clc == "mosaico de pastos con espacios naturales"
clc$name_clc[index_] <- sub(pattern = "con", replacement = "y", clc$name_clc[index_])

clc <- na.omit(clc)

#---------------------------------------

# leer coberturas que eligieron los expertos, estos estan en varios archivos csv

cob_exp <- list.files("modelos_N2/aves_endemicas_N2/ecovars_aves_endemicas/", pattern = "*eco_vars.csv", full.names = T) %>% 
  lapply(X= ., FUN = function(X){fread(X, encoding = "UTF-8") %>% data.frame()}) 
cob_exp <- do.call("rbind", cob_exp) %>% 
  mutate(name_clc = stringi::stri_trans_general(str_squish(tolower(name.1)), id = "Latin-ASCII")) %>% 
  rename("taxID" = "species_id")
# write.csv(cob_exp, "modelos_N2/aves_endemicas_N2/ecovars_aves_endemicas/_consolidado.csv", row.names = F)
#---------------------------------------

# cruzar matriz de nombres de coberturas con coberturas elegidas por los expertos

clc_cob_exp <- left_join(cob_exp, clc, by = "name_clc")

#---------------------------------------
# leer nombres de especies y taxID y cruzarlos

# nombre y taxid de especie tomado de 
# https://docs.google.com/spreadsheets/d/1m7cxTUyiUkcn5YU42Q-IPUBv1xaKf37l/edit#gid=1739285760

aves_endemicas_spp_id <- read.csv("C:/humboldt/Atlas/aves_endemicas/spp_taxid.csv")

# cruzar nombre de especies con taxID

clc_cob_nm <- left_join(clc_cob_exp, aves_endemicas_spp_id, by = "taxID")

# ¿De cuales especies aun no se han elegido coberturas?
aves_endemicas_spp_id[!(aves_endemicas_spp_id$taxID %in% cob_exp$taxID), ]

#---------------------------------------
missing <- as.character()

# Proceso de refinamiento para cada especie
for (i in 1:length(lst)) {
  # i <- 83
  r <- rast(lst[i])
  
  cat('Processing', names(r), 'covers...(', i, 'of', length(names(r)), ')\n')
  
  # Seleccionar datos de la especie actual
  x.i <- clc_cob_nm %>% filter(Especie == names(r)) %>% select(id, taxID, Especie, name)
  
  if(nrow(x.i) == 0) {
    missing[i] <- names(r)
    cat("     missing info of: ", names(r), "\n")
    next()
  }
  
  x.i <- x.i[which(x.i[,"id"] != 0), ]
  cob.xi <- x.i$id |> as.character() |> unique()
  
  nm.xi <- x.i[, c("Especie", "taxID", "name")] |> unique()
  
  write.csv(nm.xi, paste0('modelos_N2/aves_endemicas_N2/ecovars_aves_endemicas/', names(r)[i], '_namesExp.csv'), 
            fileEncoding = "UTF-8", row.names = F)
  
  # Filtrar coberturas de CLC por código de especie
  cb_fil <- clc18[[names(clc18) %in% cob.xi , ]]
  cb_fil[is.na(cb_fil)] <- 0 
  
  # Realizar una suma con la capa de la especie
  s.r <- sum(cb_fil) + r
  
  # Ajustar valores
  s.r[s.r == 1 | s.r == 0] <- NA
  s.r[s.r == 2] <- 1
  
  names(s.r) <- paste0(names(r), "_veg")
  
  # Guardar el resultado en un archivo tif
  writeRaster(s.r, paste0('modelos_N2/aves_endemicas_N2/', names(r)[i], '_veg.tif'), overwrite = T, datatype = "INT2S" )
}

missing <- na.omit(missing)
