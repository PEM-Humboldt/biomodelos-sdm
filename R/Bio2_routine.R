# title: "Biomodelos 2 routine"
# date: "1/02/2021"

#--------------------------------------

# occ = x, # Occurrence data at least must have species name, latitude, longitude, and date columns
# drop_out = "IQR", # "freq", # "IQR"
# polygon_M, # "Data/biogeographic_shp/wwf_ecoregions/wwf_terr_ecos.shp", # Spatial data to construct M composed, must be inside project file
# raster_M = NULL, # "Data/biogeographic_shp/bior_colextent/bior.tif", #MISSING solo se "prende cuando drop out es freq"
# proj_models = "M-M", # "M-G
# area_G = NULL, # "Data/biogeographic_shp/areaG.tif", # MISSING solo se prende cuando projection es M-G, MISSING puede ser raster o shape
# dist_MOV, # = 10, # Movement distance of the group
# clim_vars, # = "worldclim", # Which climatic data use (Chelsa, worldclim, other "MISSING")
# dir_clim = "Data/env_vars/", # Folder worldclim data,   "Data/present/chelsa/", inside folder project
# dir_other = "Data/env_vars/other",
# TGS_kernel, # = "bias_layer/primates.tif", # Where is the bias file, in case of do.bias active

## Rutina

Bio2_routine <- function(occ, drop_out, polygon_M, raster_M = NULL, proj_models, area_G = NULL,
                         dist_MOV, clim_vars, dir_clim, dir_other, TGS_kernel, col_sp = NULL, col_lat = NULL,
                         col_lon = NULL, do_future = NULL, extension_vars = NULL, tipo = NULL, crs_proyect = NULL,
                         beta_5.25 = NULL, fc_5.25 = NULL, beta_25 = NULL, fc_25 = NULL, kept = NULL,
                         IQR_mtpl = NULL, E = NULL, do_clean = NULL, uniq1k_method = NULL,
                         MCP_buffer = NULL, polygon_select = NULL, points_Buffer = NULL, algos = NULL,
                         use_bias = NULL, compute_G = NULL, dir_G = NULL
) {
  
  # ellipsis arguments
  if (is.null(col_sp)) col_sp <- "acceptedNameUsage" # Which is the species name column
  if (is.null(col_lat)) col_lat <- "decimalLatitude" # Which is the latitude coordinate name column
  if (is.null(col_lon)) col_lon <- "decimalLongitude" # Which is the longitude coordinate name column
  if (is.null(do_future)) do_future <- FALSE # MISSING kuenm modelling
  # if (is.null(date_period)) date_period <- "1970-01-01" # "From" date to limit chronologically occurrence data "year-month-day"
  # if (is.null(event_date)) event_date <- "eventDate"
  if (is.null(extension_vars)) extension_vars <- "*.tif$" # ?Solo aceptara tifs? como hacer para que lea solamente archivos que pueda usar raster
  if (is.null(tipo)) tipo <- "" # optional, in case of experiment (it is attached to folder sp name created)*
  if (is.null(crs_proyect)) crs_proyect <- "+init=epsg:4326"
  if (is.null(beta_5.25)) beta_5.25 <- seq(0.5, 4, 0.5)
  if (is.null(fc_5.25)) fc_5.25 <- c("l", "q", "lq") # solo minusculas
  if (is.null(beta_25)) beta_25 <- seq(1, 6, 1)
  if (is.null(fc_25)) {
    fc_25 <- c(
      "lq", "lp", "lt", "lh", "qp", "qt", "qh", "pt", "ph", "th", "lqp",
      "lqt", "lqh", "lpt", "lph", "qpt", "qph", "qth", "pth", "lqpt",
      "lqph", "lqth", "lpth", "lqpth"
    )
  } # solo minusculas
  if (is.null(kept)) kept <- FALSE # kuenm argument to clean competitor models
  if (is.null(IQR_mtpl)) IQR_mtpl <- 5
  if (is.null(E)) E <- 5
  if (is.null(do_clean)) do_clean <- FALSE
  if (is.null(uniq1k_method)) uniq1k_method <- "sq1km" # spthin #MISSING user choose the grid
  if (is.null(MCP_buffer)) MCP_buffer <- FALSE
  if (is.null(polygon_select)) polygon_select <- FALSE
  if (is.null(points_Buffer)) points_Buffer <- FALSE
  if (is.null(algos)) algos <- c("MAXENT", "GBM", "ANN")
  if (is.null(use_bias)) use_bias <- TRUE
  if (is.null(compute_G)) compute_G <- FALSE
  
  

  #--------------------------------------
  # 0. Setup
  #--------------------------------------
  
  # 0.1 Calling individual functions
  
  source("R/1_clean_rawocc.R")
  source("R/2_uniq1km.R")
  source("R/3_m.R") ## Overlaping occurrences, biogeographic units and Minimun Convex Polygon (MCP) by ENMeval: https://tinyurl.com/y3u3c6fj
  source("R/4_process_env.R")
  source("R/5_Bias.R")
  source("R/6B_4_dosdmtune.R")
  source("R/6B_doindeva.R")
  source("R/7_doensemble.R")
  source("R/give.msg.time.R")
  source("R/doDE.MCP.R")
  
  # 0.2 Starting time
  
  time1 <- Sys.time()
  
  # 0.3 set and create species folder
  
  # extract the name of the species
  
  sp_name <- occ[1, col_sp] %>% gsub(pattern = " ", replacement = ".")
  
  folder_sp <- paste0(sp_name, tipo)
  
  dir.create(folder_sp, showWarnings = F)
  
  # 0.4 writing occurrences data withouth processing, aka raw occurrences.
  
  dir.create(paste0(folder_sp, "/occurrences"), showWarnings = F)
  
  occ$occ.ID <- 1:nrow(occ)
  
  write.csv(occ, paste0(folder_sp, "/occurrences/occ_raw.csv"), row.names = F)
  
  
  # ----- tracking file
  
  filelog <- file(paste0(folder_sp, "/log_file.txt"), "w")
  
  linesmsg0 <- paste0(
    time1, "\n", "Species name: ", sp_name, "\n", "M shapefile: ", polygon_M, "\n",
    "Movement distance vector: ", dist_MOV, " km\n", # "Dates to filter: ", date_period,
    "\n", "Projecting models from ", proj_models,
    "\n", "Climatic variables: ", clim_vars, "\n",
    "Experimental type:", tipo, "\n", "Raw occurrences: ", nrow(occ),
    "\n", "Outliers manage by ", drop_out, "\nCleaning occurrences", do_clean
  )
  
  writeLines(text = linesmsg0, con = filelog, sep = "\n")
  
  #--------------------------------------
  # 1. clean data
  #--------------------------------------
  print("Cleaning data")
  
  linesmsg1 <- tryCatch(
    exp = {
      occClean <- clean_rawocc(
        occ. = occ, col.lon = col_lon, col.lat = col_lat, spp.col = col_sp,
        drop.out = drop_out, IQR.mtpl = IQR_mtpl, do.clean = do_clean
      )
      # col.date = event_date, date = date_period,
      write.csv(occClean, paste0(folder_sp, "/occurrences/occ_cleanCoord.csv"), row.names = F)
      paste0("\nClean occurrences: yes\nNumber of cleaning occurrences: ", nrow(occClean))
    },
    error = function(error_message) {
      e <- conditionMessage(error_message)
      return(paste0("Clean occurrences: fail.\nError R: ", e))
    }
  )
  
  #------- tracking file
  writeLines(
    text = linesmsg1,
    con = filelog, sep = "\n"
  )
  
  ### -----------------------------
  # ask5occ
  ### -----------------------------
  
  try(
    exp = {
      if (nrow(occClean) <= 5) {
        do.DE.MCP(
          occ. = occClean, col.lon = col_lon, col.lat = col_lat, folder.sp = folder_sp, dist.Mov = dist_MOV
        )
        linestime <- give.msg.time(time.1 = time1)
        #------- tracking file
        writeLines(
          text = paste(paste0(
            "\nNot enough occurrences. Distribution estimated by rasterize a MCP.
             \nStop"
          ), linestime),
          con = filelog, sep = "\n"
        )
        close(filelog)
        return("not enough occurrences")
      }
    }
  )
  
  #--------------------------------------
  # 2. Unique occurrences to 1 km
  #--------------------------------------
  print("Thining database to 1km")
  
  linesmsg2 <- tryCatch(
    expr = {
      occ_1km <- do.uniq1km(
        occ. = occClean, col.lon = col_lon, col.lat = col_lat, sp.col = col_sp,
        sp.name = sp_name, uniq1k.method = uniq1k_method
      )
      write.csv(occ_1km, paste0(folder_sp, "/occurrences/occ_1km.csv"), row.names = F)
      paste("Occurrences 1 km : ok.\nNumber of occurrences 'unique by pixel': ", nrow(occ_1km))
    },
    error = function(error_message) {
      e <- conditionMessage(error_message)
      return(paste0("Dropping bias: fail.\nError R: ", e))
    }
  )
  
  #------- tracking file
  
  writeLines(text = linesmsg2, con = filelog, sep = "\n")
  
  ### -----------------------------
  # ask5occ
  ### -----------------------------
  
  try(
    exp = {
      if (nrow(occ_1km) <= 5) {
        do.DE.MCP(
          occ. = occ_1km, col.lon = col_lon, col.lat = col_lat, folder.sp = folder_sp,
          dist.Mov = dist_MOV
        )
        linestime <- give.msg.time(time.1 = time1)
        #------- tracking file
        writeLines(
          text = paste(paste0(
            "\nNot enough occurrences. Distribution estimated by rasterize a MCP.
             \nStop"
          ), linestime),
          con = filelog, sep = "\n"
        )
        close(filelog)
        return("not enough occurrences")
      }
    }
  )
  
  
  #--------------------------------------
  # 3. Accesible Area.
  #--------------------------------------
  print("Constructing accesible area - M")
  
  linesmsg3 <- tryCatch(
    expr = {
      M_ <- M_area(
        polygon.M = polygon_M, raster.M = raster_M, occ. = occ_1km, col.lon = col_lon,
        col.lat = col_lat, folder.sp = folder_sp, dist.Mov = dist_MOV, drop.out = drop_out,
        MCPbuffer = MCP_buffer, polygon.select = polygon_select, pointsBuffer = points_Buffer
      )
      
      write.csv(M_$occurrences, paste0(folder_sp, "/occurrences/occ_jointID.csv"), row.names = F)
      paste("\nAccesible area: ok.")
    },
    error = function(error_message) {
      e <- conditionMessage(error_message)
      return(paste0("\nAccesible area: fail.\nError R: ", e))
    }
  )
  
  #------- tracking file
  
  writeLines(text = linesmsg3, con = filelog, sep = "\n")
  
  ### -----------------------------
  # ask5occ
  ### -----------------------------
  
  try(
    exp = {
      if (nrow(M_$occurrences) <= 5) {
        do.DE.MCP(
          occ. = M_$occurrences, col.lon = col_lon, col.lat = col_lat, folder.sp = folder_sp, dist.Mov = dist_MOV
        )
        linestime <- give.msg.time(time.1 = time1)
        #------- tracking file
        writeLines(
          text = paste(paste0(
            "\nNot enough occurrences. Distribution estimated by rasterize a MCP.
             \nStop"
          ), linestime),
          con = filelog, sep = "\n"
        )
        close(filelog)
        return("not enough occurrences")
      }
    }
  )
  
  #--------------------------------------
  # 4. Procesing environmental layers
  #--------------------------------------
  print("Procesing environmental layers")
  
  linesmsg4 <- tryCatch(
    expr = {
      
      if (compute_G == TRUE){
        env_G <- paste0(folder_sp, "/G_variables") 
      }else{
        env_G <- dir_G
      } 
      
      envars <- process_env.current(
        clim.dataset = clim_vars, clim.dir = dir_clim, extension = extension_vars,
        crs.proyect = crs_proyect, area.M = M_$shape_M, area.G = area_G,
        env.other = dir_other, folder.sp = folder_sp, dofuture = do_future,
        proj.models = proj_models, compute.G = compute_G
      )
      paste("Processing environmental layers: ok.")
    },
    error = function(error_message) {
      e <- conditionMessage(error_message)
      return(paste0("Processing environmental layers: fail.\nError R: ", e))
    }
  )

  #------- tracking file

  writeLines(text = linesmsg4, con = filelog, sep = "\n")

  #--------------------------------------
  # 5. Bias file by species
  #--------------------------------------
  
  print("Procesing bias layer")
    
    linesmsg5 <- tryCatch(
      expr = {
        BiasSp <- get_BiasSp(
          data. = M_$occurrences,
          TGS.kernel = TGS_kernel,
          shape.M = M_$shape_M,
          env.M = envars$M,
          ext = "*.asc",
          folder.sp = folder_sp,
          col.lon = col_lon,
          col.lat = col_lat,
          col.sp = col_sp
        )
        paste("Bias file development: ok")
      },
      error = function(error_message) {
        e <- conditionMessage(error_message)
        return(paste0("Bias file development: fail.\nError R: ", e))
      }
    )
  
    #------- tracking file
    writeLines(text = linesmsg5, con = filelog, sep = "\n")
  
  #--------------------------------------
  # 6. Paths of calibration and evaluation
  #--------------------------------------
  
  if (nrow(M_$occurrences) >= 5 & nrow(M_$occurrences) <= 25) {
    if(length(which(algos == "MAXENT"))!=0){
      
      ##########
      # Path A # Jackknife, enmeval maxent
      ##########
      
      print("Path A, calibrating and evaluating Maxent models")
      
      linesmsg6.1 <- tryCatch(
        expr = {
          PathAMaxent <- do.sdmtunes(
            occ. = M_$occurrences, bias.file = BiasSp, beta.mult = beta_5.25, f.class = fc_5.25,
            env.Mdir = paste0(folder_sp, "/M_variables"), env.Gdir = env_G,
            env.Fdir = paste0(folder_sp, "/F_variables"), do.future = do_future, folder.sp = folder_sp,
            col.lon = col_lon, col.lat = col_lat, proj.models = proj_models, partitionMethod = "jackknife",
            use.bias = use_bias, crs.proyect = crs_proyect, dist.Mov = dist_MOV
          )
          paste("\nPath A, number occ less or equal to 25\nSmall samples Maxent modelling: ok.")
        },
        error = function(error_message) {
          e1 <- conditionMessage(error_message)
          return(paste0("\nPath A, number occ less or equal to 25 \nSmall samples Maxent modelling fail.\nError R: ", e1))
        }
      )
      
      writeLines(text = linesmsg6.1, con = filelog, sep = "\n")
    }
  } 
  
  if (nrow(M_$occurrences) > 25) {

    if(length(which(algos == "MAXENT"))!=0){
      ##########
      # Path B # split in test and train, kuenm maxent, biomod GBM y ANN
      ##########
      
      print("Path B, calibrating and evaluating models")
      
      linesmsg6.1 <- tryCatch(
        expr = {
          PathBMaxent <- do.sdmtunes(
            occ. = M_$occurrences, bias.file = BiasSp, beta.mult = beta_25, f.class = fc_25,
            env.Mdir = paste0(folder_sp, "/M_variables"), env.Gdir = env_G,
            env.Fdir = paste0(folder_sp, "/F_variables"), do.future = do_future, folder.sp = folder_sp,
            col.lon = col_lon, col.lat = col_lat, proj.models = proj_models, partitionMethod = "block",
            use.bias = use_bias, crs.proyect = crs_proyect, dist.Mov = dist_MOV
          )
          paste0("\nPath B, number occ greater than 25 \nLarge samples Maxent modelling: ok. Check Final models folder in the species directory folder.")
        },
        error = function(error_message) {
          e1 <- conditionMessage(error_message)
          return(paste0("\nPath B, number occ greater than 25 \n Large samples Maxent modelling: fail.\nError R: ", e1))
        }
      )
      writeLines(text = linesmsg6.1, con = filelog, sep = "\n")
    }
    
    algos2 <- algos[-which(algos == "MAXENT")]
    
    if(length(algos2) != 0){
      # Biomod (ANNN, GBM)
      
      linesmsg6.2 <- tryCatch(
        expr = {
          PathBOther <- do.biomod(
            data.splitted = PathBOcc, sp.name = sp_name, folder.sp = folder_sp,
            Biasfile = BiasSp, env.Mdir = paste0(folder_sp, "/M_variables"),
            env.Gdir = paste0(folder_sp, "/G_variables"),
            env.Fdir = paste0(folder_sp, "/F_variables"), nrep.s = 10,
            do.future = do_future, proj.models = proj_models, crs.proyect = crs_proyect, 
            algorithms = algos2
          )
          paste0(
            "\nLarge samples Maxent modelling:: ok. Check Final_models_biomod folder for predictions."
          )
        },
        errror = function(error_message) {
          e3 <- conditionMessage(error_message)
          return(paste0("\nBiomod modelling: fail.\nError R: ", e3))
        }
      )
      writeLines(text = linesmsg6.2, con = filelog, sep = "\n")
    }
  }

  #--------------------------------------
  # 7. Ensembles
  print("Ensembling models")
  
  linesmsg7 <- tryCatch(
    expr = {
      res <- list()
      if(exists("PathAMaxent")) res <- list(res, PathAMaxent )  
      if(exists("PathBMaxent")) res <- list(res, PathBMaxent )      
      if(exists("PathBOther")) res <- list(res, PathBOther )
      
      ensemble <- do.ensemble(
        reslist = res, do.future = do_future,
        occ. = M_$occurrences, threshold. = E, col.lon = col_lon,
        col.lat = col_lat, folder.sp = folder_sp, crs.proyect = crs_proyect
      )
      paste("Ensemble of models: ok")
    },
    error = function(error_message) {
      e <- conditionMessage(error_message)
      return(paste0("Ensemble of models: fail.\nError R: ", e))
    }
  )
  
  #------- tracking file
  writeLines(text = linesmsg7, con = filelog, sep = "\n")

  #--------------------------------------
  # End
  #--------------------------------------

  erase <- c(
    paste0(folder_sp, "/F_variables"), # paste0(folder_sp,"models"),
    paste0(folder_sp,"proj_current_cal"), #paste0(folder_sp,".BIOMOD_DATA"),
    paste0(folder_sp, "/M_variables"), paste0(folder_sp, "/indEVA.csv"), 
    paste0(folder_sp, "/BiasfileM.asc"), paste0(folder_sp, "/maxent.cache")
  )
  for (i in 1:length(erase)) {
    if (file.exists(erase[i])) {
      unlink(erase[i], recursive = T, force = T)
    }
  }

  linestime <- give.msg.time(time.1 = time1)

  writeLines(linestime, filelog)

  close(filelog)

  return(c("ok", sp_name))
}

