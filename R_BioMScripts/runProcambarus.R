sf::sf_use_s2(F)
source("setup.R")
# do.install(vector.packages)
# do.check(vector.packages)
do.load(vector.packages)
# do.folder.structure(clim.datasets = "worldclim")
# file_names <- dir("Depurados por expertos/procambarus/", full.names = TRUE)


dataSp <- read.csv("occurrences/Procambarus clarkiiCol_4000m_mov.csv")
list <- split(dataSp, f  =dataSp$species)
source("R/Bio2_routine.R")

for (i in 1:length(list)){
Bio2_routine(
  occ = list[[i]],########
  col_sp = "species",
  col_lat = "lat", #######
  col_lon = "lon",########
  clim_vars = "clim_rios",#########
  dir_clim = "Data/env_vars/clim_rios/", #######
  dir_other = "Data/env_vars/other_rios/",#######
  TGS_kernel = NULL,#######,
  uniq1k_method = "sqkm",#########
  area_G = NULL,
  compute_G = FALSE,
  method_M = "polygon_buffer", 
  dist_MOV = 50, 
  proj_models = "M-M", ##########
  algos = "MAXENT", 
  use_bias = FALSE,
  keep_files = "essential",
  extrapo = "no_ext",
  dist_uniq = 1, 
  polygon_data = "Data/biogeographic_shp/Basins/hybas_sa_lev04_v1c.shp",
  
    ########
) 
}
closeAllConnections()
