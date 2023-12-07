# Load libraries

library(raster)
library(dplyr)
library(hms)
library(xlsx)
library(terra)
library(sf)

#__________________________________

# METADATA BETA VERSION 5 :
# April 2023  to XXX 2023
#__________________________________

# Get prerequisites

# reference map in case model is not projected to biomodelos extent
#biomodelos_template <- raster::raster("Zamia_amazonum_0_mx.tif")

# metadata template
meta_template <- read.csv("R/_metada_template.csv")

# directory in where is stored the folders coming from bio2_routine
dirmodels <- "C:/Users/Carlos/Desktop/invemar"

# directory in where metadata will be created and model tifs will be moved
tmp <- paste0(dirmodels,"_")
dir.create(tmp)
dirtowrite <- tmp

dates <- Sys.Date() %>% as.character()

source("R/automatic_metadata_functions.R")

a <- auto_metadata(dirmodels = dirmodels, dirtowrite = dirtowrite,
                   meta_template = meta_template, algos = "MAXENT", 
                   fut_proj = F, dates = dates, 
                   transf_ext = F, crs_project = NULL)


write.xlsx(a, file = paste0(dirtowrite, "/_metadata_", taxon,"_", dates, ".xlsx"),
           sheetName= "metadata", append= FALSE,
           showNA = FALSE, row.names = F)


b <- auto_metadata_invemar(dirmodels = dirmodels, dirtowrite = dirtowrite, fut_proj = F,
                           dates = dates, bm_umbral = 30, 
                           df_umbrales = "C:/humboldt/miscelanea/Invemar_areas_interes/umbrales.csv")


write.xlsx(b, file = paste0(dirtowrite, "/_metadata_b", dates, ".xlsx"),
           sheetName= "metadata", append= FALSE,
           showNA = FALSE, row.names = F)
