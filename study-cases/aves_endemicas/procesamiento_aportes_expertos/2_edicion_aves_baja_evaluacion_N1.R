library(terra)
library(rgdal)
library(dplyr)
library(openxlsx)

#Ediciones aves endemicas baja evaluacion
crs.new<- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'

########################
####  Informaci?n Base
#######################

#models
archivos <- list.files("modelos_to_edit/aves_baja_evaluacion/", pattern = '.tif', full.names = F, recursive = T)
archivos_name <- gsub('*.tif$','', archivos)
archivos <- list.files("modelos_to_edit/aves_baja_evaluacion/", pattern = '.tif', full.names = T, recursive = T)


#geojson files
lf<- list.files('ediciones/aves_baja_evaluacion/', full.names=T, pattern = '.json', recursive = T)
lf2<- list.files('ediciones/aves_baja_evaluacion/', full.names=F, pattern = '.json', recursive = T)

m <- rbind(c(0, NA), c(1, 1)) #clasificar rasters
m2<- rbind(c(1, 1), c(2, 1))

ras_bs <- rast(nrows=3240, ncols=2760, xmin=-83, xmax=-60, ymin=-14, ymax=13)

# output directory
output_dir <- "modelos_N1/aves_baja_evaluacion/"

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


# name of species to edit and taxID
edit_data <- openxlsx::read.xlsx("C:/humboldt/Atlas/aves_endemicas/ratings_2022-10-01_2022-10-31_users.xlsx", sheet = 3)
tax.id.wd.gr<- edit_data[,c('species', 'taxID')]

########################
####  Edicion Modelos
#######################
lt.sp.gr<-data.frame( file= numeric(),
                      action = character(),
                      message = character(),
                      userID = numeric(),
                      userName = character(),
                      taxID = numeric(),
                      threshold = character()
                      )
lt.x.t <- data.frame(
  action= character(),
  userID= numeric(),
  userName = character(),
  taxID = numeric(),
  threshold = character(),
  file = numeric(),
  message = character()
)

for (k in 1:length(lf)) {
  cat('reviewing the', k , 'file/n/n')
  #k = 3
  test <- tryCatch({
      lt.x<- vect(readOGR(lf[k], encoding = 'GeoJSON'))
      lt.x.t<- as.data.frame(lt.x)
      lt.x.t$file <- k
      if('message' %in%  colnames(lt.x.t)){
        lt.sp.gr<-rbind(lt.sp.gr, lt.x.t)  
      }else {
        lt.x.t$message  <- NA
        lt.sp.gr<-rbind(lt.sp.gr, lt.x.t)  
      }
  }, error = function(e){
      print(paste0("error in ", k, " file"))
      FALSE
    }
  )
  if(!is.data.frame(test) == TRUE){
    if(!test == TRUE){
      k.x <- lf2[k]
      strings <- stringi::stri_split(k.x, regex = "_", ) %>% unlist()
      lt.x.t <- lt.x.t[0, ]
      lt.x.t[1, "action"] <- "mantener umbral"
      lt.x.t[1, "userID"] <- gsub(pattern = "usr-", replacement = "", strings[2])
      lt.x.t[1, "taxID"] <- gsub(pattern = "sp-", replacement = "", strings[1])
      lt.x.t[1, "threshold"] <- gsub(pattern = "thr-|.geojson", replacement = "", strings[3]) %>% paste0("%")
      lt.x.t[1, "file"] <- k
      lt.sp.gr<-rbind(lt.sp.gr, lt.x.t)  
    }
  }
  rm(test)
}

ed.list.final <- merge(lt.sp.gr, tax.id.wd.gr, by ='taxID') |> mutate(action = iconv(tolower(action)))
rm(lt.sp.gr, lt.x, k, lt.x.t)

base <- terra::rast("Zamia_amazonum_0_mx.tif")

# ediciones
ediciones <- split(ed.list.final, f = ed.list.final$taxID)

edit_model_function <- function(sp.data = ediciones[[3]], archivos, m, lf, output_dir, bs = base) {
  sp_ <- sp.data$species[1] %>% stringr::str_to_sentence()
  unik <- unique(sp.data$threshold)
  
  if (length(unik) == 1) {
    
    model.edit <- archivos[grepl(pattern = sp_, x = archivos)] |> rast()
    model.edit <- classify(model.edit, m)
    
    for(b in 1:length(unique(sp.data$file))){
      #b <- 1
      sp.data.b <- sp.data %>% dplyr::filter(file == (unique(sp.data$file)[b]))
      for(a in 1:nrow(sp.data.b)){
        #a <- 1
        sp.data.x <- sp.data.b[a, ]
        edition.x <- vect(readOGR(lf[sp.data.x$file], encoding = 'GeoJSON'))[a]
        if(grepl(pattern = "mantener", x = sp.data.x$action)){
          inversed = FALSE
        }else{
          inversed = TRUE
        }
        model.edit <- terra::mask(x = model.edit, mask = edition.x, inverse = inversed)
      }
    }
      
    
    crs(model.edit) <- crs.new
    model.edit <- terra::extend(model.edit, bs)
    writeRaster(model.edit, paste0(output_dir, sp_, "_con.tif"), overwrite = TRUE, datatype = 'INT1U')
    return(model.edit)
  } else {
    message("Editions are applied to more than one threshold or layer, verify")
  }
}

ediciones <- lapply(X = ediciones, FUN = function(X){
      edit_model_function(sp.data = X, archivos = archivos, m = m, lf = lf, output_dir = output_dir) 
    }
  )

# agregar los raster de las especies que no tienen ediciones por poligono si no solo de altitud

ediciones_x_altitud <- openxlsx::read.xlsx("C:/humboldt/Atlas/aves_endemicas/ediciones_altitud.xlsx", sheet = 1)

index_no_editados <- !(ediciones_x_altitud$taxID %in% names(ediciones))
no_editados <- archivos[grepl(pattern = paste(ediciones_x_altitud$species[index_no_editados], collapse = "|"), x = archivos)]

r_no_editados <- list()
for(i in 1:length(no_editados)){
  r_no_editados[[i]] <- no_editados[i] |> terra::rast() |> classify(m) |> terra::extend(base)
}
names(r_no_editados) <- ediciones_x_altitud$taxID[index_no_editados] %>% sort()

ediciones <- c(ediciones, r_no_editados)

# ediciones por altitud:tabla con species name, taxid, minimun altitud, maximun altitud

edit_model_altitud <- function(ediciones_ = ediciones, ediciones_csv = ediciones_x_altitud, altitud_r = alt, output_dir){
  
  result <- list()
  
  min_alt_aoi <- terra::minmax(altitud_r)[1,]
  max_alt_aoi <- terra::minmax(altitud_r)[2,] 
  
  for(i in 1:length(ediciones_)){
    # i <- 5
    taxID_ediciones <- names(ediciones_[i]) 
    index_taxID_alt_ediciones <- ediciones_csv$taxID %in% taxID_ediciones 
    
    if(sum(index_taxID_alt_ediciones) != 0){
      ediciones_csv_i <- ediciones_csv[index_taxID_alt_ediciones, ]  
      
      taxID_i <- ediciones_csv_i$taxID
      species_i <- ediciones_csv_i$species
      min_i <- ediciones_csv_i$min
      max_i <- ediciones_csv_i$max
      
      ediciones_i <- ediciones_[[i]]  
      
      breaks_ <- rbind(c(min_alt_aoi, min_i, NA), 
                       c(min_i, max_i, 1),
                       c(max_i, max_alt_aoi, NA)
                       )
      alt.dful <- terra::classify(altitud_r, breaks_, include.lowest=TRUE)
      terra::crs(alt.dful) <- terra::crs(ediciones_i)
      temp_ <- terra::mask(ediciones_i, alt.dful)
      
      result[[i]] <- temp_
      writeRaster(temp_, paste0(output_dir, species_i, "_con.tif"), overwrite = TRUE, datatype = 'INT1U')
    }else{
      result[[i]] <- ediciones_[[i]]
    }
  }
  return(result)
}

ediciones_con_altitud <- edit_model_altitud(output_dir = output_dir)
names(ediciones_con_altitud) <- names(ediciones)











