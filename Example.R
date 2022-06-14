dataSp <- read.csv("Example/Occurrences/multi_species.csv")
dataSp <- split(dataSp, f = dataSp$species)
source("setup.R")
do.load(vector.packages)

source("R/Bio2_routine.R")

Ms <- list.files("modelosExample/", pattern = "M.shp", include.dirs = T, recursive = T) %>% 
  paste0("modelosExample/", .)
occs <- list.files("modelosExample/", pattern = "raw.csv", include.dirs = T, recursive = T) %>% 
  paste0("modelosExample/", .)
best_models <- list.files("modelosExample/", pattern = "best", include.dirs = T, recursive = T) %>% 
  paste0("modelosExample/", .)

a <- data.frame("occs" = occs, "Area M" = Ms, "area_G" = rep(NA, length(Ms)), 
                "area_F" = rep(NA, length(Ms)), "best" = best_models)

for(i in 1:length(dataSp)){
  occi <- read.csv(a[i, "occs"])
  Bio2_routine(
    occ = occi, col_sp = "species", col_lat = "lat", 
    col_lon = "lon", clim_vars = "worldclim", dir_clim = "Example/Data/env_vars/", 
    dir_other = "Example/Data/env_vars/other/", dist_MOV = 74, area_M = a[i, "Area.M"], 
    proj_models = "M-M", algos = "MAXENT", dist_uniq = 10, fc_25 = c("l", "lq", "lp"), 
    beta_25 = seq(1, 3, 1), redo = T, redo_path = a[i, "best"],
    area_G = NULL, compute_G = F, 
    do_future = T, area_F = "Data/biogeographic_shp/Basins/Cacutirostris.shp",
    compute_F = F, dir_F = "Data/F_variables/"
  ) 
  closeAllConnections()  
}
