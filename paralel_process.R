# SETUP

source("setup.R")

do.load(vector.packages)

if (!require(kuenm)) {
  devtools::install_github("marlonecobos/kuenm")
}

# Set 16 Primates (test)

# csv

# rutina
source("R/Bio2_routine.R")

# initializing

library(parallel)


for(i in 1:length(a)){
  closeAllConnections()
  i2 <- a[i]
  Bio2_routine(occ = occ_list[[i2]],
               drop_out = "any",
               polygon_M = "Data/biogeographic_shp/bior_colextent/bior.shp",
               raster_M = NULL,
               proj_models = "M-M",
               dist_MOV = 200, 
               do_future = FALSE,
               clim_vars = "worldclim",
               dir_clim = "Data/env_vars/",
               dir_other = "Data/env_vars/other/",
               TGS_kernel = "bias_layer/primates.tif"
              )
}


####

# parallel



cl <- parallel::makeCluster(detectCores())
clusterEvalQ(cl, {library(dplyr, logical.return = T)
  library(ggplot2, logical.return = T)
  library(sf, logical.return = T)
  library(rnaturalearth, logical.return = T)
  library(rnaturalearthdata, logical.return = T)
  library(ggspatial, logical.return = T)
  library(raster, logical.return = T)
  library(CoordinateCleaner, logical.return = T)
  library(sqldf, logical.return = T)
  library(automap, logical.return = T)
  library(dismo, logical.return = T)
  library(biomod2, logical.return = T)
  library(kuenm, logical.return = T)
  library(PresenceAbscence, logical.return = T)
})
parallel::parLapply(cl = cl, X = occ_list, fun = rutina16k)
stopCluster(cl)

