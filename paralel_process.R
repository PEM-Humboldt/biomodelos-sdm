# SETUP

source("setup.R")

do.install(x = vector.packages, update_pck = TRUE)

do.load(vector.packages) #twice if error

# Installing kuenm

# Kuenm is a picky package that needs several dependencies installed, this process is a bit
# time consuming but not an overwhelming task. Please refer to https://github.com/marlonecobos/kuenm.
# First, install rtools from https://cran.r-project.org/bin/windows/Rtools/
# Second, install Java Development Kit from https://www.oracle.com/java/technologies/javase/javase-jdk8-downloads.html
# Third, move a copy of "maxent.jar" to working directory root
# Then run the next code
  
if (!require(kuenm)) {
  devtools::install_github("marlonecobos/kuenm")
}

# MISSING Explain what have to do with each folder and extensions

do.folder.structure(clim.datasets = c("chelsa", "worldclim"))

# MISSING BIAS LAYER, CUT DATA


# Set 16 Primates (test)

dfspp <- read.csv("Data/primary_occurrences/registros_primates_set16.csv",
  stringsAsFactors = FALSE, sep = ","
)

occ_list <- split(dfspp, f = dfspp$acceptedNameUsage)

# rutina
source("R/Bio2_routine.R")

# initializing

all1 <- Sys.time()

a <- c(1:36)

for(i in 1:length(a)){
  closeAllConnections()
  i2 <- a[i]
  Bio2_routine(occ = occ_list[[3]],
               drop_out = "freq",
               polygon_M = "Data/biogeographic_shp/bior_colextent/bior.shp",
               raster_M = "Data/biogeographic_shp/bior_colextent/bior.tif",
               proj_models = "M-M",
               dist_MOV = 10, 
               do_future = FALSE,
               clim_vars = "worldclim",
               dir_clim = "Data/env_vars/",
               dir_other = "Data/env_vars/other/",
               TGS_kernel = "bias_layer/primates.tif"
              )
}

all2 <- Sys.time()

alltime2 <- round(as.numeric(difftime(all1, all2, units = "hours")), 2)
t.min2 <- round((alltime2 - trunc(alltime2)) * 60)

fileConn2 <- file("timespent2.txt")
writeLines(paste("Completed in ", trunc(alltime2), "hours", t.min2, "mins"), fileConn2)
close(fileConn2)

####

# parallel


library(parallel){}

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

  ###############
  occ = occ_list[[1]] # Occurrence data at least must have species name, latitude, longitude, and date columns
  clim_vars = "worldclim"
  TGS_kernel = "bias_layer/primates.tif"  
  drop_out = "IQR"
  polygon_M = "Data/biogeographic_shp/wwf_ecoregions/wwf_terr_ecos.shp"
  proj_models = "M-M" 
  dist_MOV = 10
  clim_vars = "worldclim"
  dir_clim = "Data/env_vars/"
  dir_other = "Data/env_vars/other/"
  TGS_kernel = "bias_layer/primates.tif"
  
  do_future = TRUE

  raster_M = NULL
  area_G = NULL
  col_sp = NULL
  col_lat = NULL
  col_lon = NULL
  #do_future = NULL
  extension_vars = NULL
  tipo = NULL
  crs_proyect = NULL
  beta_5.25 = NULL
  fc_5.25 = NULL
  beta_25 = NULL
  fc_25 = NULL
  kept = NULL
  IQR_mtpl = NULL
  date_period = NULL
  event_date = NULL
