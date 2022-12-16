# 2 de noviembre de 2022
# Revisar datos 4k 2022

library(dplyr)
library(measurements)
library(sf)
library(sp)
library(stringr)
library(terra)
library(CoordinateCleaner)
library(ggplot2)

col <- st_read("data/nacional_wgs84.shp")
clean_dup <- function(data = b, longitude = "decimalLongitude", latitude = "decimalLatitude", 
                      threshold = 0.008333333, id1 = "X1", id2 = "fuente"){
  dat_sp <- SpatialPointsDataFrame(data[,c(longitude ,latitude)],data)
  dat_sp1 <- remove.duplicates(dat_sp, zero = threshold)
  val <- paste(dat_sp1@data[ , longitude], dat_sp1@data[ , latitude], dat_sp1@data[ , id1], dat_sp1@data[ , id2])
  matchval <- ((paste(data[ , longitude], data[ , latitude], data[ , id1], data[ , id2]) %in% val)-1)*-1
  return(matchval)
}

load("data/plan_dico_4k_11_2022.RData")

R_4k2022 <- dico_set16_curada_1_11_2022
rm(dico_set16_curada_1_11_2022)

# duplicados
R_4k2022 <- R_4k2022 %>% mutate(NReg_Interno = seq(1, nrow(.)), 
                          DupIndex_sp = (cc_dupl(x = ., lon = "lon", lat = "lat", 
                                        species = "acceptedNameUsage", value = "flagged" )-1)*-1)
R_4k2022 <- R_4k2022[order(R_4k2022$acceptedNameUsage), ]
colnames(R_4k2022)

# duplicados a 1 kilometro

R_4k2022$Dup_1k <- R_4k2022 %>% 
  split(., f = R_4k2022$acceptedNameUsage) %>% 
  lapply(X = ., FUN = function(X){
    X1 <- as.data.frame(X) %>% mutate(lon = as.numeric(lon), lat = as.numeric(lat))
    clean_dup(data = X1, longitude = "lon", latitude = "lat", 
                                            id1 = "occurrenceID", id2 = "locality" )}) %>% 
  unlist()

save(R_4k2022, file = "results/plan_dico_4k_dups_11_2022.RData")


cat_labels <- c("r <= 2", "2 < r <=5", "5 < r < 20", "r >= 20")
total <- table(R_4k2022$acceptedNameUsage) %>% as.data.frame() %>% 
  mutate(cat_total = cut(Freq, breaks = c(0, 2, 5, 19, max(Freq)),
                         labels = cat_labels, right = T))

Unik <- table(R_4k2022[R_4k2022$DupIndex_sp == 0, "acceptedNameUsage"]) %>% 
  as.data.frame()%>% 
  mutate(cat_unik = cut(Freq, breaks = c(0, 2, 5, 19, max(Freq)),
                         labels = cat_labels, right = T))

Unik_1 <- table(R_4k2022[R_4k2022$DupIndex_sp == 0& R_4k2022$Dup_1k == 0, "acceptedNameUsage"]) %>% 
  as.data.frame()%>% 
  mutate(cat_Unik1k = cut(Freq, breaks = c(0, 2, 5, 19, max(Freq)),
                         labels = cat_labels, right = T))

summary_df <- data.frame("species" = total$Var1, 
                         "reg_total" = total$Freq, "reg_total_cat" = total$cat_total, 
                         "unik" = Unik$Freq, "unik_cat" = Unik$cat_unik,
                         "unik_1k" = Unik_1$Freq, "unik_1k_cat" = Unik_1$cat_Unik1k )

write.csv(summary_df, "results/R_4k2022_summary.csv", row.names = F)

