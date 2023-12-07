# Automated Metadata Generation for Species Modeling Results

# METADATA BETA VERSION 4
# October 2022 to XXX 2023

# Load Libraries
# Required libraries for executing the script.
library(raster)
library(dplyr)
library(hms)
library(xlsx)

# Get Prerequisites

# Load a reference map in case the model is not projected to the biomodelos extent.
biomodelos_template <- raster::raster("data/raster_template.tif")

# Load the metadata template
meta_template <- read.csv("data/metada_template.csv")

# Define the taxon for metadata generation
taxon <- "JBM"

# Define the directory containing the folders from the bio2_routine
dirmodels <- taxon

# Create a directory for storing metadata and moved model tifs
tmp <- paste0(taxon, "_2")
dir.create(tmp)
dirtowrite <- tmp

# Define the current date
dates <- Sys.Date() %>% as.character()

source("R/make_models_metadata_functions.R")

a <- auto_metadata(dirmodels = dirmodels, dirtowrite = dirtowrite,
                   meta_template = meta_template, algos = "MAXENT", 
                   fut_proj = F, dates = dates, 
                   transf_ext = F, crs_project = NULL)


write.xlsx(a, file = paste0(dirtowrite, "/_metadata_", taxon,"_", dates, ".xlsx"),
           sheetName= "metadata", append= FALSE,
           showNA = FALSE, row.names = F)


# b <- auto_metadata_invemar(dirmodels = dirmodels, dirtowrite = dirtowrite, fut_proj = F,
#                            dates = dates, bm_umbral = 30, 
#                            df_umbrales = "C:/humboldt/miscelanea/Invemar_areas_interes/umbrales.csv")
# 
# 
# write.xlsx(b, file = paste0(dirtowrite, "/_metadata_b", dates, ".xlsx"),
#            sheetName= "metadata", append= FALSE,
#            showNA = FALSE, row.names = F)