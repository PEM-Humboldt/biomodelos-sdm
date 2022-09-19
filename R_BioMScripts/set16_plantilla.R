# Set 16 modeling
# Birds
# July 2 2021

# Setup
source("setup.R")
#do.install(vector.packages)
# do.check(vector.packages)
do.load(vector.packages)
# do.folder.structure("worldclim-CCAFS")

# Set aves by researcher

load("Occurrences/Jair2_set1610km_magno_12082021.RData") # path for sp data

list <- split(df_resi, f = df_resi$acceptedNameUsage)


# rutina
source("R/Bio2_routine.R")

# missed <- c("Ammonastes pelzelni", "Anabazenops dorsalis", "Anas acuta", 
#             "Anous stolidus", "Anser anser", "Arremon crassirostris", 
#             "Atlapetes fuscoolivaceus", "Attila torridus", "Cairina moschata", 
#             "Campylopterus largipennis", "Chaetocercus jourdanii", 
#             "Chordeiles acutipennis", "Cranioleuca hellmayri", 
#             "Crypturellus berlepschi", "Cyanerpes caeruleus", 
#             "Cyanocompsa brissonii", "Drymophila hellmayri", "Eremophila alpestris")

# index <- as.numeric()
# for(i in 1:length(missed)){
#   #  unlink(gsub(x = missed[i], pattern = " ", replacement = "."), recursive = T, force = T)
#   index[i] <- which(names(list) == missed[i])
# }


which(names(list) == "Clavija costaricana")

for(i in 579:length(list)){
  Bio2_routine(occ = list[[i]],
               col_lat = "lat",
               col_lon = "lon",
               drop_out = "any",
               polygon_data = NULL, #"Data/biogeographic_shp/datacrop.shp",
               proj_models = "M-M", # "M-G"
               dist_MOV = 78, 
               clim_vars = "worldclim-CCAFS",
               dir_clim = "Data/env_vars/",
               dir_other = "Data/env_vars/other/",
               TGS_kernel = NULL,
               do_clean = FALSE,
               uniq1k_method = "sqkm",
               dist_uniq = 10,
               method_M = "points_MCP_buffer",
               area_G = NULL, # MISSING area M to use a preprocessed one
               compute_G = FALSE, 
               method_G = NULL, # "points_MCP_buffer",
               dir_G = NULL,
               do_future = TRUE,
               compute_F = NULL,
               method_F = NULL,
               area_F = NULL,
               dir_F = "Data/F_variables",
               algos = "MAXENT",
               use_bias = FALSE,
               keep_files = "essential",
               transf_biomo_ext = TRUE, #FALSE,
               extrapo = "no_ext"
  )
  
  closeAllConnections()
  gc()
}


