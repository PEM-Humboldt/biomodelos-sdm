#convert2PNG.R
#This function creates a KMZ file, georeferenced PNG and a thumb PNG for any given tif file.
#Projection, extent and color scheme have been optimized for BioModelos
#This function can be wrapped in a for loop or apply (sapply or sfClusterApplyLB) to
#convert all tif files on a list. See example below
#
#Args:
# sp.raster = character string with tif filename (including extension)
# in.folder = folder that contains the .tif file to convert
# col.pal = color palette to be used in resulting maps
# add.trans = add trasparent color to palette? Usually you would use TRUE when the tif file
# contains NA, 0 and 1 values, whereas you would use FALSE when the tif only
# has NA and 1 values.
# params = list with elements dem, corresponding to a raster with elevation to be displayed in
#  the background of thumbnails, and shape, corresponding to a SpatialPolygonsDataFrame with 
#  administrative boundaries to be displayed in thumbnail.
#
#Usage:
#   in.folder = "~/Modelos/librorojo2"
#   col.pal = rgb(193,140,40,maxColorValue=255)
#   sp.raster = "Anas_bahamensis_0.tif"
#   convert2PNG(sp.raster, in.folder, col.pal, TRUE, params=params)
#
#Example on parallel loop
# require(snowfall)
# sfInit(parallel=T,cpus=16)#Initialize nodes
# sfExportAll() #Export vars to all the nodes
# sfClusterSetupRNG()
# sfClusterApplyLB(sp.list, convert2PNG, in.folder=in.folder, 
# col.pal=col.pal, add.trans=TRUE, params=params)
# sfStop()
#
#Author: Jorge Velasquez
#Date created: 05-09-2014

convert2PNG<-function(sp.raster, name,in.folder, col.pal, add.trans, params,w,h){
  require(raster)
  require(sp)
  require(rgdal)
  
  #Create dirs
  dir.create(paste0(in.folder,"/PNG"), recursive=T)
  dir.create(paste0(in.folder,"/KMZ"), recursive=T)
  dir.create(paste0(in.folder,"/thumb"), recursive=T)
  
  #Plots for geovisor
  if(class(sp.raster)=="RasterLayer"){
    in.raster <- sp.raster
  } else {
    in.raster <- raster(paste0(in.folder, "/", sp.raster))
    if(is.na(projection(in.raster))){
      projection(in.raster)<-"+proj=longlat +ellps=WGS84 +datum=WGS84"
    }
  }
    
  #Remove colors if not enough categories
  # freq.cat <- freq(in.raster)
  # ind <- freq.cat[which(freq.cat[, 1]>0)]
  # col.pal <- col.pal[ind]
  tr<-rgb(255, 255, 255, 0, maxColorValue=255)
  if(add.trans){
    col.pal <- c(tr, col.pal)
  }

  #Create KML
  if(is.null(name)){
    name <- strsplit(sp.raster,"[.]")[[1]][1]
  }
  KML(in.raster, filename=paste0(in.folder,"/KMZ/",name,".kmz"),
      maxpixels=ncell(in.raster), col=col.pal, overwrite=T, blur=1)
  unzip(paste0(in.folder, "/KMZ/", name, ".kmz"), exdir=paste0(in.folder,"/PNG"))
  file.remove(paste0(in.folder, "/PNG/", name,".kml"))
  
  #Generate thumbnails
  in.raster.co <- in.raster
  png(paste0(in.folder, "/thumb/", name, "_thumb.png"),
      width=w, height=h, units="px", type="cairo")
  op <- par(mar = rep(0, 4), bg=NA)
  image(params$dem, axes=F, xlab="", ylab="", col=c(tr, "grey90"))
  image(in.raster.co, col=col.pal, axes=FALSE, add=T)
  plot(params$shape, add=T, lwd=1, border="grey40")
  dev.off()
  unlink(list.files(tempdir()),recursive=T)
}

