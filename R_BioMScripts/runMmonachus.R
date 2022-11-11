sf::sf_use_s2(F)
source("setup.R")
do.load(vector.packages)
do.check()

dataSp <- read.csv("Example/Occurrences/single_species.csv")# %>% 
#  dplyr::filter(countryCode != "", !is.na(decimalLatitude))
set.seed(23)
dataSp <- dataSp[sample(1:nrow(dataSp), 5, replace = F), ]
source("R/Bio2_routine.R")

Bio2_routine(
  occ = dataSp,########
  col_sp = "species",
  col_lat = "lat", #######
  col_lon = "lon",########
  clim_vars = "worldclim",#########
  dir_clim = "Data/env_vars_ex/", #######
  dir_other = "Data/env_vars_ex/other/",#######
  TGS_kernel = NULL,#######,
  uniq1k_method = "sqkm",#########
  #area_G = "Data/biogeographic_shp/nacional_wgs84.shp",
  #compute_G = T,
  #compute_F = T, 
  #do_future = T,
  #area_F = "Data/env_vars_ex/other/current/elev.tif", 
  method_M = "points_MCP_buffer", 
  dist_MOV = 50, 
  proj_models = "M-M", ##########
  use_bias = FALSE,
  keep_files = "all",
  extrapo = "no_ext",
  dist_uniq = 1, 
  #fc_25 = c("l", "lq"), 
  #beta_25 = seq(1, 2, 1),
  #tipo = "ENMeval2",
  #pckg = "ENMeval2"
    ########
) 
closeAllConnections()
# debugger

{col_sp = NULL; col_lat = NULL; col_lon = NULL; dir_clim = NULL; dir_other = NULL;
extension_vars = NULL; uniq1k_method = NULL; dist_uniq = NULL; use_bias = NULL; TGS_kernel = NULL; 
method_M = NULL; dist_MOV = NULL; method_G = NULL; area_M = NULL; area_G = NULL;
compute_G = NULL; dir_G = NULL; do_future = NULL; method_F = NULL;area_F = NULL; compute_F = NULL; dir_F = NULL;
cor_eval = NULL; cor_method = NULL; cor_detail = NULL; algos = NULL; beta_5.25 = NULL;
fc_5.25 = NULL; beta_25 = NULL; fc_25 = NULL; E = NULL; extrapo = NULL; predic = NULL; kept = NULL; pckg = "ENMeval2";
crs_proyect = NULL; tipo = NULL;keep_files = NULL; transf_biomo_ext = NULL; redo = NULL; 
redo_path = NULL; polygon_data = NULL}

{occ = dataSp;########
col_sp = "species";
col_lat = "lat"; #######
col_lon = "lon";########
clim_vars = "worldclim";#########
dir_clim = "Example/Data/env_vars/"; #######
dir_other = "Example/Data/env_vars/other/";#######
TGS_kernel = NULL;#######;
uniq1k_method = "sqkm";#########
area_G = "Data/biogeographic_shp/nacional_wgs84.shp";
compute_G = T;
method_M = "points_MCP_buffer"; 
dist_MOV = 50; 
proj_models = "M-G"; ##########
algos = "MAXENT"; 
use_bias = FALSE;
keep_files = "all";
extrapo = "no_ext";
dist_uniq = 1; 
fc_25 = c("l", "lq"); 
beta_25 = seq(1, 2, 1)}


