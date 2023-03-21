#  October 20-21 2022
# Compare range of present and future variables

library(terra)
other <- list.files("Data/env_vars/other/current/", ".tif$",full.names = T) |> rast()
c <- list.files("Heterogomphus.dilaticollis/G_variables/Set_1/M/", ".asc$",full.names = T) |> rast()
f1 <- list.files("Heterogomphus.dilaticollis/G_variables/Set_1/GFDL-ESM4_2021-2040_ssp126/", ".asc$",full.names = T) |> rast()
f2 <- list.files("Heterogomphus.dilaticollis/G_variables/Set_1/GFDL-ESM4_2021-2040_ssp370/", ".asc$",full.names = T) |> rast()

names(f2)

c <- c(c, other)
f1 <- c(f1, other)
f2 <- c(f2, other)

cr <- lapply(X = c, FUN = function(X){global(X, range, na.rm = T)})
names(cr) <- names(c)
cr <- do.call("rbind", cr)
f1r <- lapply(X = f1, FUN = function(X){global(X, range, na.rm = T)})
names(f1r) <- names(f1)
f1r <- do.call("rbind", f1r)
f2r <- lapply(X = f2, FUN = function(X){global(X, range, na.rm = T)})
names(f2r) <- names(f2)
f2r <- do.call("rbind", f2r)

all <- cbind(cr,f1r, f2r)

colnames(all) <- c("cur_min", "cur_max", "f1_min", "f1_max","f2_min", "f2_max")

write.csv(all, "comparacion_current_future_asc.csv", row.names = T)

#------------------------------------
# October 24 2022
# Compare scripts: servidor and local in order to update github
install.packages("diffr")
library(diffr)

local <- "R/Bio2_routine.R"
serv <- "comparar/Bio2_routine.R"

diffr(local, serv)

#----------------------------------
library(raster)
library(dplyr)

biomodelos.thresh <- c(5, 1, 10, 20, 30)
names(biomodelos.thresh) <- c("E", "MTP", "TTP", "20TP", "30TP")

ras.all <- raster::stack(list.files(path = "vestitus/", pattern = "G.asc", full.names = T, recursive = T))
nms <- unlist(strsplit(list.files(path = "vestitus", pattern = "G.asc", full.names = F, recursive = T), split = "/"))[seq(1,by = 2,len =nlayers(ras.all))]
occ <- read.csv("Coendou.vestitus/occurrences/occ_jointID.csv")

# converting to binary the median ensemble for each biomodelos threshold
Bins <- list()
for(i in 1:nlayers(ras.all)){
  thresholdsstack <- stack()
  for (a in 1:length(biomodelos.thresh)) {
    Binsi <- do.bin(
      Ras = ras.all[[i]], dat = occ, lon = "decimalLongitude",
      lat = "decimalLatitude", thresh = biomodelos.thresh[a]
    )
    thresholdsstack <- raster::stack(Binsi[[1]], thresholdsstack)
  }
  Bins[[i]] <- thresholdsstack
}
names(Bins) <- nms
plot(stack.bin)
for(i in 1:length(Bins)){
  stack.bin <- Bins[[i]]
  pdf(file = paste0("cavia_aperea_x/bias/", names(Bins)[i], ".pdf"), onefile = T, width = 11, height = 7)
  plot(stack.bin[[1]])
  points(occ[,3:4])
  plot(stack.bin[[2:nlayers(stack.bin)]])
  dev.off()
}



#Maps by species

setwd("C:/modelos")

spp <- list.dirs("aves/flag", full.names = T, recursive = F)
nm_spp <- list.dirs("aves/flag", full.names = F, recursive = F)

for(i in 1:length(spp)){
  try(
    occ_bysp <- read.csv(paste0(spp[i], "/occurrences/occ_jointID.csv"), stringsAsFactors = F)
  )
  if(exists(x = "occ_bysp")){
    en_bysp <- list.dirs(paste0(spp[i], "/ensembles/current"), recursive = F, full.names = T)
    ras.all <- raster::stack(list.files(path = en_bysp, pattern = "*.tif$", full.names = T, recursive = T))
    if(nlayers(ras.all) != 0){
      pdf(file = paste0(nm_spp[i], ".pdf"), width = 9, height = 7)
      par(2,3)
      for(a in 1:nlayers(ras.all)){
        plot(ras.all[[a]], main = names(ras.all[[a]]))
        points(occ_bysp[,3:4], pch = 20)
      }
      dev.off()
    }
  }
}

#-----------------------
library(raster)

registros_consolidado<- read.csv("AgendaModelado/roedores/AM_rodentia_mongo.csv", sep = ',', header = T, encoding = 'UTF-8')
species<- as.character(unique(registros_consolidado$acceptedNameUsage))
#species<-species[15]



#correct <-with(species.t, species.t[is.na(species.t$reportedGeoIssueBm)&is.na(species.t$reportedOriginVagrant) & is.na(species.t$reportedIdIssueBm)& is.na(species.t$reportedOtherIssuesBm)& species.t$spatialDuplicated == FALSE & species.t$use == TRUE ,])
#incorrect <- with(species.t, species.t[!is.na(species.t$reportedGeoIssueBm)|!is.na(species.t$reportedOriginVagrant) | !is.na(species.t$reportedIdIssueBm)| !is.na(species.t$reportedOtherIssuesBm)& species.t$spatialDuplicated == FALSE & species.t$use == TRUE ,]) 

c <- raster("Data/biogeographic_shp/area.G.tif")


for(i in 1:length(species)){
  species.t <- registros_consolidado[registros_consolidado$acceptedNameUsage %in% species[i], ]  
  pdf(paste0(species[i], ".pdf"), width = 7, height = 7)
  plot(c, main = nrow(species.t))
  points(species.t[, c("decimalLongitude","decimalLatitude")], pch = 20, col = "red")
  dev.off()
}

#------------------------
# SETUP

source("setup.R")

do.install(x = vector.packages, update.packages = FALSE)

do.load(vector.packages)
library(SDMtune)

# Installing kuenm

# Kuenm is a picky package that needs several dependencies installed, this process is a bit
# time consuming but not an overwhelming task. Please refer to https://github.com/marlonecobos/kuenm.
# First, install rtools from https://cran.r-project.org/bin/windows/Rtools/
# Second, install Java Development Kit from https://www.oracle.com/java/technologies/javase/javase-jdk8-downloads.html
# Third, move a copy of "maxent.jar" to working directory root
# Then run the next code

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

###############
occ = occ_list[[31]] # Occurrence data at least must have species name, latitude, longitude, and date columns
clim_vars = "worldclim"
TGS_kernel = "bias_layer/primates.tif"  
drop_out = "freq"
polygon_M = "Data/biogeographic_shp/bior/bior.shp"
proj_models = "M-M" 
dist_MOV = 222
clim_vars = "worldclim"
dir_clim = "Data/env_vars/"
dir_other = "Data/env_vars/other/"

do_future = FALSE

raster_M = "Data/biogeographic_shp/bior/bior.tif"
area_G = NULL
col_sp = NULL
col_lat = NULL
col_lon = NULL
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
E = NULL
uniq1k_method <- "sq1km" # spthin


MCP_buffer = NULL
do_clean = TRUE

