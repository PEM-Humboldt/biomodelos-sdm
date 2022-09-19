# September 19 2022

# Setup
source("setup.R")
# do.install(vector.packages)
# do.check(vector.packages)
do.load(vector.packages)

R_JCN_maps <- read.csv("occurrences/R_BM_JCNeita_dataDups.csv") %>% filter(Dup_1k == 0 ) %>% 
  dplyr::select(decimalLatitude, decimalLongitude, acceptedNameUsage)

R_CAM_maps <- read.csv("occurrences/R_BM_CAM_dataDups.csv") %>% filter(Dup_1k == 0) %>% 
  dplyr::select(decimalLatitude, decimalLongitude, acceptedNameUsage)

R_coleopteros <- rbind(R_JCN_maps, R_CAM_maps)

listCol <- split(R_coleopteros, f = R_coleopteros$acceptedNameUsage)

#csv with paths of M: defined with ecoregions, before change root path with excel.
# results/M/sppname to Data/biogeographic_shp/M
m_data <- read.csv("Data/biogeographic_shp/M_data_spp.csv")

# rutina
source("R/Bio2_routine.R")

for(i in 1:length(listCol)){
  spp_nm <- names(listCol[1])
  path_M <- m_data[which(m_data$acceptedNameUsage == spp_nm), "M"]
  Bio2_routine(occ = listCol[[1]],
               col_lat = "decimalLatitude",
               col_lon = "decimalLongitude",
               proj_models = "M-M",
               clim_vars = "worldclim",
               dir_clim = "Data/env_vars/",
               dir_other = "Data/env_vars/other/",
               uniq1k_method = NA,
               col_eval = T,
               area_M = path_M,
               algos = "MAXENT",
               keep_files = "essential",
               transf_biomo_ext = TRUE,
               extrapo = "no_ext"
  )
  
  closeAllConnections()
  gc()
}

