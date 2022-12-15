
# resolver problemas con diferentes rutas

# M-M
# M-G, calcular G
# M-G, sin calcular G
# M-M, calcular F
# M-G, calcular G, calcular F
# M-G, calcular G, sin calcular F
# M-G, sin calcular G, calcular F
# M-G, sin calcular G, sin calcular F

source("setup.R")
# do.install(vector.packages)
# do.check(vector.packages)
do.load(vector.packages)

dataSp <- read.csv("Example/Occurrences/single_species.csv")

# rutina
source("R/Bio2_routine.R")

# M-M

Bio2_routine(
  occ = dataSp, col_sp = "species", col_lat = "lat", 
  col_lon = "lon", clim_vars = "worldclim", dir_clim = "Example/Data/env_vars/", 
  dir_other = "Example/Data/env_vars/other/", method_M = "points_MCP_buffer", dist_MOV = 15,
  proj_models = "M-M", algos = "MAXENT", dist_uniq = 10, 
) 

closeAllConnections()