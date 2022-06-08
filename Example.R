dataSp <- read.csv("Example/Occurrences/single_species.csv")
source("setup.R")
do.load(vector.packages)

source("R/Bio2_routine.R")

Bio2_routine(
  occ = dataSp, col_sp = "species", col_lat = "lat", 
  col_lon = "lon", clim_vars = "worldclim", dir_clim = "Example/Data/env_vars/", 
  dir_other = "Example/Data/env_vars/other/", method_M = "points_buffer", dist_MOV = 74,
  proj_models = "M-M", algos = "MAXENT", dist_uniq = 10
) 
