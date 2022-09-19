# SETUP

source("setup.R")

#do.load(vector.packages)

#if (!require(kuenm)) {
#  devtools::install_github("marlonecobos/kuenm")
#}

# Set 16 Primates (test)

set_ericaceas <- read.csv("AgendaModelado/ericaceas/ericaceae.csv", stringsAsFactors = F, sep = ",")
list_ericaceas <- split(set_ericaceas, f = set_ericaceas$scientificName)
list_ericaceas_r <- list_ericaceas[52:60]

# rutina
source("R/Bio2_routine.R")

# parallel

# initializing

library(parallel)

# show me the needed packages
# vector.packages
# [1] "plyr"              "dplyr"             "automap"           "PresenceAbsence"   "devtools"
# [6] "CoordinateCleaner" "sf"                "spThin"            "raster"            "dismo"
# [11] "biomod2"           "ENMeval"           "rgdal"
# plus
# "kuenm"

cl <- parallel::makeCluster(2)
clusterEvalQ(cl, {
  library(plyr, logical.return = T)
  library(dplyr, logical.return = T)
  library(PresenceAbsence, logical.return = T)
  library(CoordinateCleaner, logical.return = T)
  library(sf, logical.return = T)
  library(spThin, logical.return = T)
  library(raster, logical.return = T)
  library(dismo, logical.return = T)
  library(biomod2, logical.return = T)
  library(ENMeval, logical.return = T)
  library(rgdal, logical.return = T)
  library(kuenm, logical.return = T)
})
clusterExport(cl = cl, list("Bio2_routine"),
              envir=environment())
parallel::parLapply(cl = cl, X = list_ericaceas_r, function(X) {
  Bio2_routine(
    occ = X,
    drop_out = "any",
    polygon_M = "Data/biogeographic_shp/wwf_ecoregions/wwf_terr_ecos.shp",
    raster_M = NULL,
    proj_models = "M-M",
    dist_MOV = 84,
    do_future = FALSE,
    clim_vars = "worldclim",
    dir_clim = "Data/env_vars/",
    dir_other = "Data/env_vars/other/",
    TGS_kernel = "bias_layer/ericaceas.tif",
    do_clean = FALSE,
    uniq1k_method = "sq1km",
    col_sp = "scientificName"
  )
})
stopCluster(cl)
