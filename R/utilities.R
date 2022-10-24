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
