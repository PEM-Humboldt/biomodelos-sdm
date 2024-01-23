#' Function to calculate VIF values for each variable based on the extracted values.
#' 
#' @description This function applies the vif_func function (defined below) to calculate the VIF values for 
#' the variables. Finally, it returns the calculated VIF values. The VIF is calculated by running a linear 
#' regression on each variable while using all other variables as predictors. The function returns 
#' the names of the variables that have a VIF value below a specified threshold (vifdetails).
#' 
#' @param vifsample data.frame containing points from the sample area constructed with cor_sample function
#' @param envars raster file containing the explanatory variables.
#' @param vifdetails numeric value specifying the VIF threshold.

vif_apply <- function(vifsample, envars, vifdetails){
  
  d <- terra::extract(envars, vifsample) %>% 
    dplyr::select(-ID) %>% 
    na.omit()
  
  vifd <- vif_func(d, thresh = vifdetails)
  return(vifd) # aplicar el vif
}

#' Calculates VIF values for a set of explanatory variables using linear regression models and iteratively
#' removes variables with high VIF values until all VIF values are below a specified threshold. 
#' 
#' @description This function computes the VIF values for a set of explanatory variables using a data frame 
#' (in_frame). The VIF is calculated by running a linear regression on each variable while using all other 
#' variables as predictors. The function returns the names of the variables that have a VIF value below a 
#' specified threshold (thresh).
#' 
#' @param in_frame data frame containing the data used to calculate the VIF.
#' @param thresh numeric value specifying the VIF threshold.
#' @param trace logical value indicating whether to print the output of each iteration.
#' @param ... additional arguments to be passed to the lm() function.

vif_func<-function(in_frame,thresh=10,trace=F,...){
  
  require(fmsb) 
  require(dismo)
  require(rgdal)

  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}

#' Extracts environmental variable values at sampled points from a shapefile

#' @description This function takes in a shapefile, a set of environmental variables, 
#' and VIF (variance inflation factor) details as inputs. It then samples 
#' a subset of points from the shapefile, extracts the values of the environmental variables at those 
#' points
#' 
cor_sample <- function(shapeM, envar){
  
  Mpoints <- shapeM %>% 
    st_as_sf() %>%  
    terra::vect() %>% 
    rasterize(envar) %>% 
    terra::as.data.frame(xy = T) %>% 
    dplyr::select(-layer)
  
  if (nrow(Mpoints) > 10000) {
    Sbg <- Mpoints[
      sample(
        x = seq(1:nrow(Mpoints)),
        size = 10000,
        replace = F
      ),
      1:2
    ]
  } else {
    Sbg <- Mpoints[
      sample(
        x = seq(1:nrow(Mpoints)),
        size = ceiling(nrow(Mpoints) * 0.3),
        replace = F
      ),
      1:2
    ]
  }
  
  return(Sbg)
}




