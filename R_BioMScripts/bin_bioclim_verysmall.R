library(raster)
library(dplyr)

fls <- list.dirs("x/", full.names = F, recursive = F)
fls <- fls[8]
nm <- fls %>% gsub(pattern = "\\.", replacement = "_", x = .)

r <- list.files(paste0("x/", fls, "/ensembles/current/bioclim/"), full.names = T)[6] %>% raster()
csv <- list.files(paste0("x/", fls, "/ensembles/current/bioclim/"), full.names = T)[1] %>% read.csv()
dat <- csv$X0 %>% round(2)

r[r < dat-0.01] <- 0
r[r >= dat] <- 1

bins <- c(0,10,20,30)

for(i in 1:length(bins)){
  writeRaster(r, filename = paste0("x/", fls, "/ensembles/current/bioclim/", 
                                   nm, "_", bins[i],"_bioclim.tif"),
              format = "GTiff",
              overwrite = T,
              datatype = "INT2S",
              NAflag = -9999,
              options = "COMPRESS=LZW"
  )
}

