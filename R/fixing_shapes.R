#trying to fix bior holes and no valid geometry

install.packages("cleangeo")
library(cleangeo)

bior_shp <- readOGR("Data/biogeographic_shp/bior/bior.shp")

report <- cleangeo::clgeo_CollectionReport(bior_shp)
summary <- cleangeo::clgeo_SummaryReport(report)
issues <- report[report$valid == FALSE,]

nv <- clgeo_SuspiciousFeatures(report)

bior.fixed <- clgeo_Clean(bior_shp)

report2 <- cleangeo::clgeo_CollectionReport(bior.fixed)
summary2 <- cleangeo::clgeo_SummaryReport(report2)
issues <- report[report2$valid == FALSE,]

raster::shapefile(x = bior.fixed, filename = "bior", overwrite = T )
