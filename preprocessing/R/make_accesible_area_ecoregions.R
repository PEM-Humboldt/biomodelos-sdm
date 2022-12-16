# 4 de noviembre de 2022
# Este código genera los PMC y crea las M externas en una carpeta elegida 
# por el usuario. Vale la pena decir que la ubicación generada en este 
# paso será escrita en un archivo de salida, el cual será usado para iniciar 
# la corrida de los biomodelos.

# Paquetes

library(dplyr)
library(sf)
sf::sf_use_s2(F)
library(stringr)
library(terra)

#--------------------------------
# funciones

# Generar puntos espaciales
gen.st.points <- function(dat, collon = col.lon, collat = col.lat) {
  st.points <- dat %>%
    dplyr::select(collon, collat) %>%
    st_as_sf(coords = c(collon, collat), crs = st_crs("EPSG:4326")) %>%
    st_transform(st_crs("EPSG:4326"))
}

# Generar poligono minimo convexo
gen.MCP <- function(stobject) {
  st_convex_hull(st_union(stobject))
}

#--------------------------------------------------------------------
#1- Construir PMC (poligono minimo convex0)

# Cargar registros
load("results/plan_dico_4k_dups_11_2022_Jair1.RData")

# dividir la base de datos por nombre de especie, guardar en lista
DB <- split(plan_dico_4k_dups_11_2022_Jair1, 
            f = plan_dico_4k_dups_11_2022_Jair1$spec.name)

# ruta en donde se van a guardar los PMC
dir.polys <- "results/shapesUICN_4k2022_Jair1/"
dir.create(dir.polys, showWarnings = F)

suppressMessages(
  for(i in 1:length(DB)){
    print(i)
    DBi <- DB[[i]] %>% as.data.frame()
    sppnm <- names(DB[i]) %>% gsub(" ", "_", .) 
    
    points <- gen.st.points(DBi, collon = "lon", collat = "lat")
    st_write(points, paste0(dir.polys, sppnm, "_AOO_poly.shp"), quiet = T,
             delete_layer = T )
    
    if(nrow(DBi) >= 3){
      mcp <- gen.MCP(points)
      st_write(mcp, paste0(dir.polys, sppnm, "_EOO_poly.shp"), quiet = T, 
               delete_layer = T )
    }
  }
)

rm(list=setdiff(ls(), "dir.polys"))

#2- construir M

# Llamar archivos auxiliares(ecoregiones_BM, nacional_wgs84, w_BM)
# ecorregiones 2017
ecoreg <- vect("data/Ecoregiones_BM/Ecoregions2017_BM.shp")

# plantilla COL
nal <- vect("data/nacional_wgs84.shp")

# ventana biomodelos
w_BM <- vect("data/w_BM.shp")

# 1) leer el archivo Tabla de resumen de especies (*summary.csv) y filtrar las especies
# que tuvieran mas de 20 registros
S_4k2022 <- read.csv("results/R_4k2022_summary.csv") %>% filter(unik_1k >= 20)

# ordenar y seleccionar las primeras 4 columnas
S_4k2022 <- S_4k2022[order(S_4k2022$species), 1:4 ]

#cambiar nombres de columnas para guardar las rutas del poligono minimo convexo, 
# y la M a calcular
colnames(S_4k2022) <- c("species", "MCP", "subpop", "M")
S_4k2022$MCP <- NA; S_4k2022$subpop <- NA; S_4k2022$M <- NA

# 2.1) especies con poligono minimo convexo, (mas de tres registros)
mcp <- list.files(dir.polys, pattern = "*EOO_poly.shp$", full.names = T)

# 2.2) registros/poblaciones (conR  *subpop_poly.shp)
AOO_ <- list.files(dir.polys, pattern = "*AOO_poly.shp$", full.names = T)

# 3) intersectar las especies totales y las que tienen poligono minimo, llenar tabla con datos del path
# de las subpoblaciones y el poligono minimo convexo

for(i in 1:length(S_4k2022$species)){
  print(i)
  sp <- S_4k2022$species[i] %>% gsub(pattern = " ", replacement = "_")
  indexmcp <- grep(pattern = sp, x = mcp)[1]
  if(length(indexmcp) >= 1){
    path <- mcp[indexmcp][1]
    S_4k2022[i, "MCP"] <- path  
  }
  indexpo <- grep(pattern = sp, x = AOO_)
  if(length(indexpo) >= 1){
    path <- AOO_[indexpo][1]
    S_4k2022[i, "subpop"] <- path  
  }
}

# carpeta para guardar las M
dir.M <- "results/M/"
dir.create(dir.M, showWarnings = F)

# Generar M: de aquellas especies que tienen MCP, elegir las ecorregiones (Olson et al, 2014 actualizados a 2017)
# que intersectan, disolver la superficie y transformar (en terminos de  arcmap: multipart to singlepart polygon, 
# en sf: -MULTYPOLYGON TO POLYGON-) y limpiar los poligonos sueltos al intersectar las superficies agrupadas que
# intersectan con las subpoblaciones.
for(a in 1:nrow(S_4k2022)){
  print(a)
  path_spp <- S_4k2022[a, "MCP"]
  if(!is.na(path_spp)){
    MCP_vector <-  path_spp %>% vect()
    ecoreg_mcpi <- ecoreg[MCP_vector] %>% aggregate() %>% st_as_sf() %>% 
      st_cast("POLYGON")
    poly <- S_4k2022[a, "subpop"] %>% st_read()
    ecoreg_i <- ecoreg_mcpi[poly,]
    pathM <- paste0(dir.M, S_4k2022[a, "species"], "_M.shp")
    S_4k2022[a, "M"] <- pathM
    st_write(ecoreg_i, pathM, delete_layer = T, quiet = T)
    
    #generar imagen para inspección rapida
    tiff(filename = paste0(dir.M, S_4k2022[a, "species"], "_M.tiff"),  
         width = 4, height = 4, 
         units = 'in', res = 300, compression ="lzw")
    plot(w_BM, border = NA)
    plot(nal, add = T, col = colors()[239], border = NA)
    plot(ecoreg_i, add = T, col = colors()[258], border = NA)
    mtext(S_4k2022[a, "species"], side=3, cex = 0.5)
    plot(MCP_vector, add = T, col = NA, border = colors()[175])
    plot(poly, add = T, col = colors()[76], border = NA)
    dev.off()
  }
}

write.csv(S_4k2022, "results/M_data_spp.csv", row.names = F)



