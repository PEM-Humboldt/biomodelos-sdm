# paquetes
library(data.table)
library(sf)
library(terra)
library(dplyr)

# reading csv
# se necesita una lista de archivos csv o de tabla

files <- list.files("temp/", ".csv", full.names = T)
csvs <- lapply(X = files, FUN = function(X){read.csv(X)})

# plantilla raster de rios, puede ser cualquier variable del conjunto de variables
raster_rios <- rast("a.tif")

# moving_occurences es una función que permite mover los registros de presencia que 
# se encuentran alredor de una cuenca acuatica, esta puede mover los puntos desde cualquier
# distancia hacia el rio. Sin embargo, puede que un registro que no esta en el pixel
# contiguo del rio pertenezca a una cuenca muy diferente por lo que deberia pensarse 
# si vale la pena mover los pixeles a una distancia que se encuentra a mas de 1 km
#
# csvData = data.frame, con las columnas "acceptedNameUsage", "decimalLongitude","decimalLatitude"
# rastData = raster, plantilla de localización de las cuencas acuaticas
# dirout = character vector, directorio en donde se guardara el csv con los registros movidos
# distBuf= numeric vector, distancia maxima en metros de la cuenca desde la cual se 
# permitira mover los registros
#
# return = data.frame, con los registros con su nueva ubicación con las siguientes
# columnas
# acceptedNameUsage: character vector, nombre de la especie
# decimalLatitude: numeric vector, latitud del registro
# decimalLongitude: numeric vector, longitud del registro
# nodat: logical, se encontraba el registro en una ubicación con informacion de la 
# plantilla de las cuencas hidricas 
# indist: logical, ¿se encontraba el registro a la distancia permitida para el movimiento
# de los registros?
# moved: logical, ¿fue movido el registro?
#
# ejemplo
#
# acceptedNameUsage	        decimalLatitude	decimalLongitude	nodat	indist	moved
# Chelonoidis denticulatus	8.0875	        -61.84583333	    FALSE	NA	    NA
# Chelonoidis denticulatus	8.0875	        -61.84583333	    FALSE	NA	    NA
# Chelonoidis denticulatus	7.3706	        -60.4911	        FALSE	NA	    NA



moving_occurences <- function(csvData, rastData, dirout, distBuf){
  csv_i <- csvData
  print(csv_i[1, "acceptedNameUsage"])
  csv_i$nodat <- NA
  csv_i$indist <- NA
  csv_i$moved <- NA
  
  for(a in 1:nrow(csv_i)){
    print(a)
    dat_a <- csv_i[a, c("decimalLongitude","decimalLatitude")]
    if(is.nan(extract(rastData, dat_a)[,2])){
      dat_spat <- dat_a %>% st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), 
                                     crs = crs(rastData))
      dat_buffer <- dat_spat %>% st_buffer(dist = distBuf) %>% vect() 
      tryCatch(exp = {
        coords <- rastData %>% crop(dat_buffer) %>% mask(dat_buffer) %>% crds(df=T, na.rm=F)
      }, error = function(e){return(NA)}
      )
      buffer_extract <- extract(rastData, coords)
      index_1 <- which(buffer_extract[,2] == 1)
      if(length(index_1) != 0){
        coords_1 <- coords[index_1,]
        coords_1_spat <- coords_1 %>% st_as_sf(coords = c("x", "y"), 
                                               crs = crs(rastData)) 
        dist <- st_distance(dat_spat, coords_1_spat)
        new_coords <- coords_1[which(dist == min(dist)), ]
        csv_i[a, c("decimalLongitude", "decimalLatitude") ] <- new_coords
        csv_i[a, "indist" ] <- T
        csv_i[a, "moved" ] <- T
      }else{
        csv_i[a, "indist" ] <- F
        csv_i[a, "moved" ] <- F
      }
      
      csv_i[a, "nodat" ] <- T
      
    }else{
      csv_i[a, "nodat" ] <- F
    }
  }
  dir.create(paste0(dirout, "/moved"), showWarnings = F)
  write.csv(csv_i, paste0(dirout, "/moved/", csv_i[1, "acceptedNameUsage"],"_", 
                          distBuf, "m_mov.csv"), row.names = F)
  
  return(csv_i)
}

# unica especie
moving_occurences(csvData = csvs[[4]], rastData = raster, dirout = "temp/", distBuf = 2000)

# especies multiples
lapply(X = csvs[acuaticas], FUN = function(X){moving_occurences(
   csvData = X, rastData = raster, dirout = "temp/", distBuf = 1000)}
   )



