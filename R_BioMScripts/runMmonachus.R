sf::sf_use_s2(F)
source("setup.R")
do.load(vector.packages)
do.check()

dataSp <- read.csv("occurrences/Myiopsitta monachus.csv", sep = "\t") %>% 
  dplyr::filter(countryCode != "", !is.na(decimalLatitude))
# set.seed(23)
# dataSp <- dataSp[sample(1:nrow(dataSp), 24, replace = F), ]
source("R/Bio2_routine.R")

Bio2_routine(
  occ = dataSp,########
  col_sp = "species",
  col_lat = "decimalLatitude", #######
  col_lon = "decimalLongitude",########
  clim_vars = "worldclim",#########
  dir_clim = "Example/Data/env_vars/", #######
  dir_other = "Example/Data/env_vars/other/",#######
  TGS_kernel = NULL,#######,
  uniq1k_method = "sqkm",#########
  area_G = "Data/biogeographic_shp/nacional_wgs84.shp",
  compute_G = T,
  method_M = "points_MCP_buffer", 
  dist_MOV = 50, 
  proj_models = "M-G", ##########
  algos = "MAXENT", 
  use_bias = FALSE,
  keep_files = "all",
  extrapo = "no_ext",
  dist_uniq = 1, 
  fc_25 = c("l", "lq"), 
  beta_25 = seq(1, 3, 1),
    ########
) 
closeAllConnections()
