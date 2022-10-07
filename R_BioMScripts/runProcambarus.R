sf::sf_use_s2(F)
source("setup.R")
do.load(vector.packages)
do.check()

dataSp <- read.csv("occurrences/Procambarus clarkiiCol_4000m_mov.csv")
source("R/Bio2_routine.R")

Bio2_routine(
  occ = dataSp,########
  col_sp = "species",
  col_lat = "lat", #######
  col_lon = "lon",########
  clim_vars = "worldclim",#########
  dir_clim = "Example/Data/env_vars/", #######
  dir_other = "Example/Data/env_vars/other/",#######
  TGS_kernel = NULL,#######,
  uniq1k_method = "sqkm",#########
  area_M = "Data/biogeographic_shp/nacional_wgs84.shp",
  area_G = "Data/biogeographic_shp/Basins/Cacutirostris.shp",
  compute_G = T,
  method_M = "polygon_buffer", 
  dist_MOV = 50, 
  proj_models = "M-G", ##########
  algos = "MAXENT", 
  use_bias = FALSE,
  keep_files = "essential",
  extrapo = "no_ext",
  dist_uniq = 1, 
  polygon_data = "Data/biogeographic_shp/Basins/hybas_sa_lev04_v1c.shp", 
  fc_25 = c("l", "lq"), 
  beta_25 = seq(1, 3, 1)
    ########
) 
closeAllConnections()
