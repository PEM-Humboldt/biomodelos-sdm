source("setup.R")
do.load(vector.packages)
do.check()

sf::sf_use_s2(FALSE)

files <- list.files(path = "AgendaModelado/moved/", pattern = "_1m_mov.csv$", full.names = T)
csv_files <- lapply(X=files, 
                    FUN = function(X){
                      Y = read.csv(X) 
                      #Y = filter(Y, nodat == T | moved == F)
                      return(Y)})

# aq
# [1] "occurrences/moved/Chelus fimbriata_1000m_mov.csv"         
# [2] "occurrences/moved/Chelus orinocensis_1000m_mov.csv"       
# [3] "occurrences/moved/Chelydra acutirostris_1000m_mov.csv"    
# [4] "occurrences/moved/Rhinoclemmys melanosterna_1000m_mov.csv"
# [5] "occurrences/moved/Trachemys medemi_1000m_mov.csv"

# terr
#[1] "AgendaModelado/moved/Chelonoidis denticulatus_1m_mov.csv"
#[2] "AgendaModelado/moved/Rhinoclemmys annulata_1m_mov.csv"

#aq_terr
# # [4] "occurrences/moved/Rhinoclemmys melanosterna_1000m_mov.csv"

source("R/Bio2_routine.R")

Bio2_routine(occ = csv_files[[1]],
             col_sp = "acceptedNameUsage",
             col_lat = "decimalLatitude",
             col_lon = "decimalLongitude",
             drop_out = "any",
             area_M = "Data/biogeographic_shp/Basins/Cdenticulata.shp",
             proj_models = "M-M",
             area_G = NULL,
             dist_MOV = NULL, 
             clim_vars = "worldclim",
             dir_clim = "Data/env_vars/",
             dir_other = "Data/env_vars/other_tortugas_terr/",
             TGS_kernel = NULL,
             do_clean = FALSE,
             uniq1k_method = "sqkm",
             dist_uniq = 10,
             method_M = NULL,
             algos = c("MAXENT"),
             use_bias = FALSE,
             extension_vars = "tif",
             fc_25 <- c(
               "lq", "lp", "qp", "qh", "lqp",
               "lqh", "lph", "qph", "lqph"
             ),
               tipo = "terr3",
             kept = T, 
             keep_files = "all"
)
closeAllConnections()
gc()

