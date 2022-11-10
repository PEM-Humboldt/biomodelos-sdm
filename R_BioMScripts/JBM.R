# September 19 2022

# Setup
source("setup.R")
do.check(vector.packages)
do.load(vector.packages)

R_JBM <- read.csv("occurrences/R_BM_JBM_dataDups.csv") %>% filter(in_BM == T, Dup_1k == 0 )

list_JBM <- split(R_JBM, f = R_JBM$species); rm(R_JBM)

#csv with paths of M: defined with ecoregions, before change root path with excel.
# results/M/sppname to Data/biogeographic_shp/M
m_data <- read.csv("Data/biogeographic_shp/M_data_spp_JBM.csv")

# rutina
source("R/Bio2_routine.R")


for(i in 1:length(list_JBM)){
  num_data <- list_JBM[[i]] %>% nrow()
  if(num_data >= 6){
    spp_nm <- names(list_JBM[7])
    path_M <- m_data[which(m_data$species == spp_nm), "M"]
    Bio2_routine(occ = list_JBM[[i]],
                 col_lat = "decimalLatitude",
                 col_lon = "decimalLongitude", 
                 col_sp = "species",
                 proj_models = "M-M",
                 clim_vars = "worldclim",
                 dir_clim = "Data/env_vars/",
                 dir_other = "Data/env_vars/other/",
                 uniq1k_method = NULL,
                 cor_eval = F,
                 area_M = path_M,
                 algos = "MAXENT",
                 keep_files = "essential",
                 transf_biomo_ext = TRUE,
                 extrapo = "no_ext",
                 fc_25 = c("l", "q", "lq", "lp", "lqp"),
                 beta_25 =  seq(0.5, 6, 1),
                 do_future = F, 
                 #area_F = "Data/env_vars/MPI-ESM1-2-LR/2021-2040/ssp126/bio_1.tif", 
                 #compute_F = T,
                 #dir_F = "Data/F_variables/"
                 
    )
    
    closeAllConnections()
    gc()
  }
}

# rutina
source("R/Bio2_routine_bioclim.R")

for(i in 1:length(list_JBM)){
  num_data <- list_JBM[[i]] %>% nrow()
  if(num_data >= 3 & num_data < 6){
    spp_nm <- names(list_JBM[i])
    path_M <- m_data[which(m_data$species == spp_nm), "M"]
    Bio2_routine(occ = list_JBM[[i]],
                 col_lat = "decimalLatitude",
                 col_lon = "decimalLongitude", 
                 col_sp = "species",
                 proj_models = "M-M",
                 clim_vars = "worldclim",
                 dir_clim = "Data/env_vars/",
                 dir_other = "Data/env_vars/other/",
                 uniq1k_method = NULL,
                 cor_eval = T,
                 cor_detail = 3,
                 area_M = path_M,
                 #algos = "MAXENT",
                 keep_files = "essential",
                 transf_biomo_ext = TRUE,
                 extrapo = "no_ext",
                 fc_25 = c("l", "q", "lq", "lp", "lqp"),
                 beta_25 =  seq(0.5, 6, 1),
                 do_future = F, 
                 #area_F = "Data/env_vars/MPI-ESM1-2-LR/2021-2040/ssp126/bio_1.tif", 
                 #compute_F = T,
                 #dir_F = "Data/F_variables/"
                 
    )
    
    closeAllConnections()
    gc()  
  }
  
}
#####################

missingspp <- c("Brassavola nodosa", "Microgramma megalophylla")

for(i in 1:length(list_JBM)){
  missingi<- missingspp[2]
  spp_nm <- which(names(list_JBM) == missingi)
  if(length(spp_nm) != 0){
    path_M <- m_data[which(m_data$species == missingi), "M"]
    Bio2_routine(occ = list_JBM[[spp_nm]],
                 col_lat = "decimalLatitude",
                 col_lon = "decimalLongitude", 
                 col_sp = "species",
                 proj_models = "M-M",
                 clim_vars = "worldclim",
                 dir_clim = "Data/env_vars/",
                 dir_other = "Data/env_vars/other/",
                 uniq1k_method = NULL,
                 cor_eval = F,
                 area_M = path_M,
                 algos = "MAXENT",
                 keep_files = "essential",
                 transf_biomo_ext = TRUE,
                 extrapo = "no_ext",
                 fc_25 = c("l", "q", "lq", "lp", "lqp"),
                 beta_25 =  seq(0.5, 6, 1),
                 do_future = F, 
                 pckg = "kuenm" 
                 #area_F = "Data/env_vars/MPI-ESM1-2-LR/2021-2040/ssp126/bio_1.tif", 
                 #compute_F = T,
                 #dir_F = "Data/F_variables/"
                 
    )
    
    closeAllConnections()
    gc()  
  }
  
}

# generalizar a pixeles de 2x2km

rtemplate <- raster("Data/env_vars/other/current/wc21elev_s.tif")

for(i in 1:length(list_JBM)){
  num_data <- list_JBM[[i]] %>% nrow()
  if(num_data <= 2){
    spp_nm <- names(list_JBM[i])
    occ <-  list_JBM[[spp_nm]]
    
    # create generalization to 2kmx2km
    xy <- st_as_sf(occ, coords = c("decimalLongitude", "decimalLatitude"), 
                   agr = "constant")
    generalizacion_shp <- 
      sf::st_buffer(xy, dist = 0.008333, nQuadSegs=1, endCapStyle = "SQUARE") %>% 
      st_set_crs("EPSG:4326") %>% st_union()
    
    #rasterize polygon
    generalizacion_r <- 
      raster::rasterize(x = as_Spatial(generalizacion_shp), y = rtemplate)
    
    # create folders to emulate principal workflow
    spp_point <- gsub(pattern = " ", replacement = ".", x = spp_nm)
    spp_hyphen <- gsub(pattern = " ", replacement = "_", x = spp_nm)
    
    dir.create(paste0(spp_point, "/ensembles/generalizacion/"), showWarnings = F,
               recursive = T)
    dir.create(paste0(spp_point, "/interest_areas/"), showWarnings = F,
               recursive = T)
    dir.create(paste0(spp_point, "/occurrences"), showWarnings = F, 
               recursive = T)
    
    write.csv(occ, paste0(spp_point, "/occurrences/occ_jointID.csv"))
    
    st_write(generalizacion_shp, paste0(spp_point, "/interest_areas/generalizacion.shp"),
             delete_layer = T)
    
    writeRaster(generalizacion_r, paste0(
      spp_point, "/ensembles/generalizacion/", spp_hyphen, "_generalizacion.tif"
    ),
    format = "GTiff",
    overwrite = T,
    datatype = "INT2S",
    NAflag = -9999,
    options = "COMPRESS=LZW"
    )
  }
}
