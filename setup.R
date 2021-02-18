# BIOMODELOS 2

do.folder.structure <- function(clim.datasets) {

  # Mainbone folder structure

  dir.create("R", showWarnings = F) # to storage R scripts
  dir.create("bias_layer", showWarnings = F) # to storage bias file layers created (TGS focus see https://onlinelibrary.wiley.com/doi/10.1111/j.1600-0587.2013.07872.x)
  dir.create("Data", showWarnings = F) # to storage raw geographical data

  # Data sub folders

  dir.create("Data/biogeographic_shp", showWarnings = F) # storage of biogeographic, ecoregions, hydrosheets used to construct accesible are by each species
  dir.create("Data/env_vars", showWarnings = F) # env_vars: storage of environmental variables (other folder and climatic folders)
  dir.create("Data/primary_occurrences", showWarnings = F) # primary_occurrences: storage of occurrences without procesing, as they are downloaded from online repositories or give from other researchers
  dir.create("Data/shapes", showWarnings = F) # shapes: storage of useful shapefiles like Colombian or American borders

  # env_vars sub folders

  dir.create(paste0("Data/env_vars/other"), showWarnings = F)
  dir.create(paste0("Data/env_vars/other/current"), showWarnings = F)
  dir.create(paste0("Data/env_vars/other/future"), showWarnings = F)

  for (i in 1:length(clim.datasets)) {
    dir.create(paste0("Data/env_vars/", clim.datasets[i]), showWarnings = F)
    dir.create(paste0("Data/env_vars/", clim.datasets[i], "/current"), showWarnings = F)
    dir.create(paste0("Data/env_vars/", clim.datasets[i], "/future"), showWarnings = F)
  }

  message("Biomodelos 2 Folder structure complete")
}

# Packages

# Explanation

# dplyr #pipeline and useful functions to work data frames
# CoordinateCleaner # automatic cleaning for coordinate data
# sf # to work with spatial objects like points, vectors and polygons
# rnaturalearth # to use the r natural earth data (https://www.naturalearthdata.com/)
# rnaturalearthdata # to import the r natural earth data (https://www.naturalearthdata.com/)
# raster # to work with spatial objects like raster, it loads sp package (similar to sf)
# automap # to construct semivariograms in order to account spatial autocorrelation
# dismo # it is a package to make ENM, but here it is used to split occurrence data
# biomod2 # to make ENM, here is used to construct models for spp with more than 25 occurrences
# Artificial Neural Networks (ANN) and Generalized Boosting Model (GBM)
# kuenm # to fit Maxent models with more than 25 occurrences
# ENMeval # to fit Maxent models with less than 25 occurrences
# rJava # to link Low-Level R to Java Interface
# PresenceAbsence # to fit and compute roc eval outside of kuenm, enmeval or biomod2
# rgdal # to bind the 'Geospatial' Data Abstraction Library
# gsubfn # to work with Strings and Function Arguments (Sara Varela function)
# proto # Object-oriented programming with the prototype model (Sara Varela function)
# RSQLite # to embed the 'SQLite' database engine in R(Sara Varela function)
# devtools # Collection of package development tools.

vector.packages <- c(
  "plyr", "dplyr", "automap", "PresenceAbsence", "devtools",
  "CoordinateCleaner", "sf", "spThin", "raster", "dismo", "biomod2", "ENMeval","rgdal"
  )


# Installing

do.install <- function(x, update.packages = F) {
  tryCatch(
    expr = {
      
      missing_pkgs <- x[which(!x %in% installed.packages())]

      if (length(missing_pkgs)){
        install.packages(missing_pkgs, repos = "https://www.icesi.edu.co/CRAN/")
        return(paste0("packages installed ", length(missing_pkgs),",", missing_pkgs))
      }
      
      if (update.packages == T){
        install.packages(x, repos = "https://www.icesi.edu.co/CRAN/" )
        return(paste0("packages installed "))
      }
        
    },
    error = function(error_message) {
      e <- conditionMessage(error_message)
      message(paste0("Fail installing packages because ", e))
    },
    warning = function(warning_message) {
      w <- message(warning_message)
      message("Some warnings detected, please resolve them")
      message(w)
    }
  )
}

do.load <- function(x) {
  lapply(x, require, character.only = TRUE)
  return("ok")
}

