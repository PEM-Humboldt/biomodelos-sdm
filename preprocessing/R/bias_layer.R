# Carlos Munoz
# December 14, 2020

# Aim: Creating bias layer from a TGS 

library(dplyr)
library(sf)
library(raster)


# 1. Needed data, shapes and raster

# All occurrence of primates database (Here change to mammals, birds, reptiles, plants)

occ_all <- "AgendaModelado/aves/AM_aves_240221_mongo.csv"

# Calling a template for the raster, in this case will be used a climate layer

chelsa_ex <- raster("Data/env_vars/worldclim/current/bio1.tif")

# Shape ecological regions (used to ensure the same extend for the kernel, climate data and models)
eco_olson <- st_read("Data/biogeographic_shp/wwf_ecoregions/wwf_terr_ecos.shp")


# 2. Overlay TGS data with ecoregions


# Extract the longitude and latitude of TGS data and throw out NA value

occ_all_df <- read.csv(occ_all, stringsAsFactors = F) %>%
              dplyr::select(decimalLongitude, decimalLatitude) %>%
              dplyr::mutate_all(as.numeric) %>% 
              na.omit()


# Intersect TGS data and shape
  
crs_shape <- st_crs(eco_olson)
coord <- occ_all_df %>% 
         st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = crs_shape)
pi <- eco_olson$geometry[coord]
names(pi) <- "ecoregion_olson_TGS"
plot(pi)

# 3. Preparing raster template for density kernel
  
# Extracting coordinate box of the overlay ecoregion-TGS  

lonlat <- st_bbox(pi)

# Adding extreme coordinates of ecoregion-TGS to occurrences

lonlat_matrix <- matrix(data = lonlat, nrow = 2, ncol = 2, byrow = T) %>%
                 data.frame() %>% 
                 dplyr::rename("decimalLongitude" = "X1", "decimalLatitude" = "X2")

occ_all_df <- rbind(occ_all_df, lonlat_matrix)

# cropping raster 

chelsa_crop <- raster::crop(chelsa_ex, lonlat)

# cleaning environment
rm(chelsa_ex, coord, eco_olson, lonlat)

# 4. Making density raster

# We will use the kde2d function from the MASS package, which gives us a two-dimensional kernel
# density estimate, based on the coordinates of the occurrence points. (Note: the output of this 
# function is sensitive to the bandwidth selection; if in doubt, use the default.)
# https://scottrinnan.wordpress.com/2015/08/31/how-to-construct-a-bias-file-with-r-for-use-in-maxent-modeling/

# Used in: Muscatello, A., Elith, J., & Kujala, H. (2020). How decisions about fitting species 
# distribution models affect conservation outcomes. Conservation Biology.
# band let by default

if(nrow(occ_all_df) > 20000){
  occ_all_df<- occ_all_df[sample(1:nrow(occ_all_df), size = 20000), ]
  # ensure that extreme coordinates are in
  occ_all_df <- rbind(occ_all_df, lonlat_matrix)
}
occ_all_raster <- rasterize(occ_all_df, chelsa_crop, 1)
plot(occ_all_raster)

dens <- MASS::kde2d(x = occ_all_df[ , 1], 
                    y = occ_all_df[ , 2],
                    n = c(nrow(occ_all_raster), ncol(occ_all_raster))
                    )

dens.ras <- raster(dens) %>%
            resample( chelsa_crop, method="bilinear")

# normalize and sum a constant to drop out 0 data as MAXENT complains about NA, 0 and negative data
# in the bias file

x <- ((dens.ras - cellStats(dens.ras, min)) / (cellStats(dens.ras, max) - cellStats(dens.ras, min))) + 0.0000009

# Writing

group <- 

writeRaster(x, filename = paste0("bias_layer/", group, ".tif"), overwrite = T)

rm(list=ls()) 
gc()
