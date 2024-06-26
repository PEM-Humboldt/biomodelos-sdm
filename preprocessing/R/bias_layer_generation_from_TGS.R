# Aim: Creating a bias layer from a Target Group Sampling (TGS) dataset

# This R script generates a bias layer using a Target Group Sampling (TGS) dataset. 
# It integrates occurrence data with ecological regions and uses a climate raster 
# as a template. The script performs spatial operations to ensure consistent extents, 
# computes a kernel density estimate for the occurrence points, and writes the 
# resulting bias layer raster file. The output is formatted to meet requirements for 
# ecological modeling applications, particularly for use in species distribution 
# modeling and habitat suitability assessments.

# Load necessary libraries
library(dplyr)
library(sf)
library(raster)

# 1. Needed data: occurrence points, raster template, and shapefile of ecological regions

# Specify the path to the TGS dataset (Here, change to mammals, birds, reptiles, plants)
occ_all <- "occurrence data for a taxon"

# Load a template raster (e.g., climate layer)
chelsa_ex <- raster(".../modelling/Data/env_vars/worldclim/current/bio1.tif")

# Load shapefile of ecological regions (used to ensure consistent extent for kernel, climate data, and models)
eco_olson <- st_read(".../Data/biogeographic_shp/wwf_ecoregions/wwf_terr_ecos.shp")

# 2. Overlay TGS data with ecological regions

# Read and prepare TGS data
occ_all_df <- read.csv(occ_all, stringsAsFactors = FALSE) %>%
  dplyr::select(decimalLongitude, decimalLatitude) %>%
  dplyr::mutate_all(as.numeric) %>% 
  na.omit()

# Create spatial points from TGS data
crs_shape <- st_crs(eco_olson)
coord <- st_as_sf(occ_all_df, coords = c("decimalLongitude", "decimalLatitude"), crs = crs_shape)

# Spatially join TGS data with ecological regions
pi <- st_join(coord, eco_olson)$geometry
names(pi) <- "ecoregion_olson_TGS"
plot(pi)

# 3. Prepare raster template for density kernel

# Extract bounding box coordinates of overlay ecoregion-TGS
lonlat <- st_bbox(pi)

# Add extreme coordinates of ecoregion-TGS to occurrences
lonlat_matrix <- matrix(data = lonlat, nrow = 2, ncol = 2, byrow = TRUE) %>%
  as.data.frame() %>%
  rename("decimalLongitude" = "X1", "decimalLatitude" = "X2")

occ_all_df <- rbind(occ_all_df, lonlat_matrix)

# Crop raster to match the extent of ecoregion-TGS
chelsa_crop <- crop(chelsa_ex, lonlat)

# Clean environment
rm(chelsa_ex, coord, eco_olson, lonlat)

# 4. Generate density raster

# Use the kde2d function from MASS package to create a kernel density estimate
if (nrow(occ_all_df) > 20000) {
  occ_all_df <- occ_all_df[sample(1:nrow(occ_all_df), size = 20000), ]
  occ_all_df <- rbind(occ_all_df, lonlat_matrix)
}

# Rasterize occurrence data
occ_all_raster <- rasterize(occ_all_df, chelsa_crop, 1)
plot(occ_all_raster)

# Perform kernel density estimation and resample to match climate raster resolution
dens <- MASS::kde2d(x = occ_all_df[, 1], 
                    y = occ_all_df[, 2],
                    n = c(nrow(chelsa_crop), ncol(chelsa_crop)))

dens.ras <- raster(dens) %>%
  resample(chelsa_crop, method = "bilinear")

# Normalize and adjust values to prevent issues in MAXENT modeling
x <- ((dens.ras - cellStats(dens.ras, min)) / (cellStats(dens.ras, max) - cellStats(dens.ras, min))) + 0.0000009

# Write the bias layer raster to file
group <- # Specify the group name for the bias layer output file

writeRaster(x, filename = paste0("bias_layer/", group, ".tif"), overwrite = TRUE)

# Clean up workspace
rm(list = ls()) 
gc()
