# BIOMODELOS 2

do.folder.structure <- function(clim.datasets) {

  # Mainbone folder structure

  dir.create("Bias_layer", showWarnings = F) # to storage bias file layers created (TGS focus see https://onlinelibrary.wiley.com/doi/10.1111/j.1600-0587.2013.07872.x)
  dir.create("Data", showWarnings = F) # to storage raw geographical data
  dir.create("Occurrences", showWarnings = F) # to storage raw geographical data

  # Data sub folders

  dir.create("Data/biogeographic_shp", showWarnings = F) # storage of biogeographic, ecoregions, hydrosheets used to construct accesible are by each species
  dir.create("Data/env_vars", showWarnings = F) # env_vars: storage of environmental variables (other folder and climatic folders)
  dir.create("Data/F_variables", showWarnings = F)
  
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
  "CoordinateCleaner", "sf", "spThin", "raster", "dismo", "biomod2", "ENMeval", "rgdal",
  "rJava", "kuenm", "terra"
)

# Installing

do.install <- function(x = vector.packages, repository = "https://www.icesi.edu.co/CRAN/", update.packages = F) {
  if (!require(devtools, warn.conflicts = FALSE)) {
    install.packages("devtools")
  }

  # Version of ENMeval now is 2.0.3 this routine will work with 2.0.3
  # make sure user has 0.3.1 and give a warning
  missing_enmeval <- !"ENMeval" %in% installed.packages()

  # if enmeval is not installed
   if (missing_enmeval == TRUE) {
     devtools::install_version("ENMeval", version = "2.0.3", repos = "http://cran.us.r-project.org")
   }else{
     # if enmeval is already installed, check the version, uninstall if it is not 0.3.1
  # 
     enmevalversion <- paste0(unlist(packageVersion("ENMeval")), collapse = ".")
   
     if (enmevalversion != "2.0.3") {
       remove.packages("ENMeval")
       devtools::install_version("ENMeval", version = "2.0.3", repos = "http://cran.us.r-project.org")
       warning(paste0("ENMeval was changed from ", enmevalversion, ", to 2.0.3"))
     }
   }
  
  # fixing last issues installing kuenm  
   if(!require(hier.part, warn.conflicts = FALSE)){
    install.packages("https://cran.r-project.org/src/contrib/Archive/gtools/gtools_3.9.3.tar.gz",
                     repos = NULL, type = "source", dependencies = T)
    install.packages("https://cran.r-project.org/src/contrib/Archive/modeltools/modeltools_0.2-22.tar.gz",
                     repos = NULL, type = "source", dependencies = T)
    install.packages("https://cran.r-project.org/src/contrib/Archive/flexmix/flexmix_2.3-18.tar.gz",
                     repos = NULL, type = "source", dependencies = T)
    install.packages("https://cran.r-project.org/src/contrib/Archive/lmtest/lmtest_0.9-39.tar.gz",
                     repos = NULL, type = "source", dependencies = T)
    install.packages("https://cran.r-project.org/src/contrib/Archive/sandwich/sandwich_3.0-1.tar.gz",
                     repos = NULL, type = "source", dependencies = T)
    install.packages("https://cran.r-project.org/src/contrib/Archive/betareg/betareg_3.1-3.tar.gz",
                     repos = NULL, type = "source", dependencies = T)
    install.packages("https://cran.r-project.org/src/contrib/Archive/hier.part/hier.part_1.0-6.tar.gz",
                     repos = NULL, type = "source", dependencies = T)
  } 
  
  # kuenm needs to be installed from github as it doesn't have link to CRAN
  if (!require(kuenm, warn.conflicts = FALSE)) {
    devtools::install_github("marlonecobos/kuenm")
  }

  x_noenmeval <- x[-which(x == "ENMeval")]
  x_nokuenm <- x_noenmeval[-which(x_noenmeval == "kuenm")]

  missing_pkgs <- x_nokuenm[which(!x_nokuenm %in% installed.packages())]

  if (length(missing_pkgs)) {
    install.packages(missing_pkgs, repos = repository)
    return(paste0("packages installed ", length(missing_pkgs), ",", missing_pkgs))
  }

  if (update.packages == T) {
    install.packages(missing_pkgs, repos = repository)
    return(paste0("packages installed "))
  }
}

do.check <- function(x = vector.packages) {
  # table of packages
  dfpcks <- data.frame("package" = vector.packages, "successfully_installed" = x %in% rownames(installed.packages()))
  # enmeval version
  #enmevalversion <- paste0(unlist(packageVersion("ENMeval")), collapse = ".") == "0.3.1"
  enmevalversion <- paste0(unlist(packageVersion("ENMeval")), collapse = ".") == "2.0.3"
  # message showing user results
  message(paste0(capture.output(dfpcks), collapse = "\n"), paste0("\n\nENMeval version 2.0.3 is ", enmevalversion))
}

do.load <- function(x) {
  lapply(x, require, character.only = TRUE)
  return("ok")
}
