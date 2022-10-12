# Biomodelos 2 Routine
#
# `Bio2_routine` automates the fitting of Species Distribution Models from occurrence
# and environmental data.

Bio2_routine <- function(occ, col_sp = NULL, col_lat = NULL, col_lon = NULL,
                         clim_vars, dir_clim = NULL, dir_other = NULL,
                         extension_vars = NULL, 
                         uniq1k_method = NULL, dist_uniq = NULL, use_bias = NULL, TGS_kernel = NULL, 
                         method_M = NULL, dist_MOV = NULL, 
                         proj_models, method_G = NULL, area_M = NULL, area_G = NULL,
                         compute_G = NULL, dir_G = NULL, do_future = NULL, method_F = NULL,
                         area_F = NULL, compute_F = NULL, dir_F = NULL,
                         cor_eval = NULL, cor_method = NULL, cor_detail = NULL,
                         algos = NULL, beta_5.25 = NULL, fc_5.25 = NULL, beta_25 = NULL, fc_25 = NULL, 
                         E = NULL, extrapo = NULL, predic = NULL, kept = NULL, pckg = NULL,
                         crs_proyect = NULL, tipo = NULL,keep_files = NULL, transf_biomo_ext = NULL, 
                         redo = NULL, redo_path = NULL, polygon_data = NULL
                         # mxnt.pckg = NULL, other.pckg = NULL
) {

  # checking concatenated arguments and format of files

  if (!exists("occ")) {
    stop("Provide an occurrence database as a data.frame at least with species name,
        longitud and latitud. You can use arguments col_sp, col_lat and col_lon
        to provide the column names of each one.")
  } else {
    if (!is.data.frame(occ)) {
      stop("The data base provided need to be a data.frame object.")
    } else {
      if (!is.null(col_lon) | !is.null(col_lat)) {
        try(
          expr = {
            ln <- occ[, col_lon]
            lt <- occ[, col_lat]
          }
        )
        if (!exists("ln")) {
          if (!exists("lt")) {
            stop("Longitude or latitude coordinates does not find at database. Please check the column names and retry.")
          } else {
            rm(ln, lt)
          }
        }
      }
    }
  } # close occ

  if (!exists("clim_vars")) {
    stop("Provide a climatic variable folder to use. Pre-processed by the user.")
  } else {
    if (!is.character(clim_vars)) {
      stop("Provide a character string mentioning the climatic variables to use.")
    } else {
      if (!is.null(dir_clim)) {
        if (!dir.exists(dir_clim)) stop("Climatic directory does not exist, please provide a valid one.")
      }
      if (!is.null(dir_other)) {
        if (!dir.exists(dir_other)) stop("Other directory does not exist, please provide a valid one.")
      }
    }
  } # close climatic

  if (!exists("proj_models")) {
    stop("You need to provide a method to calibrate and project the models, either be M-M or M-G.")
  } else {
    if (proj_models == "M-G") {
      if (compute_G == TRUE) {
        if (is.null(area_G) & is.null(method_G)) {
          stop("Provide a raster file of an area different to M or a constructing method for G in order to project the models.")
        } else {
          Gstr <- tail(unlist(strsplit(area_G, "\\.")), n = 1)
          if (Gstr == "shp") rtemp <- raster::shapefile(area_G)
          if (Gstr == "tif") rtemp <- raster::raster(area_G)

          if (!exists("rtemp")) {
            stop("Provide a raster file supported by the raster package. See documentation.")
          } else {
            rm(rtemp)
          }
          if (!exists("method_G")) {
            stop("Provide a r file supported by the raster package. See documentation.")
          }
        }
      } else {
        if (is.null(dir_G)) {
          stop("Provide a path directory in which are stored the variables pre-processed using G as geographic extent.")
        } else {
          if (!dir.exists(dir_G)) {
            stop("Directory of G variables does not exist, please provide a valid one.")
          } else {
            dir_GFiles <- list.files(dir_G)
            if (length(length(dir_GFiles)) == 0) {
              stop("Any file inside dir_G path. Are the variables located there?")
            }
          }
        }
      }
    }
  } # close proj models

  if (!is.null(extrapo)) {
    if (extrapo != "all") {
      if (extrapo != "ext_clam") {
        if (extrapo != "ext") {
          if (extrapo != "no_ext") stop("Provide a valid type to make the projections.")
        }
      }
    }
  }

  if (!is.null(do_future)) {
    if (do_future == TRUE) {
      if (compute_F == FALSE) {
        if (is.null(dir_F)) {
          stop("Provide a path directory in which are stored the future variables pre-processed using M or G as geographic extent.")
        } else {
          if (!dir.exists(dir_F)) {
            stop("Directory of F variables does not exist, please provide a valid one.")
          } else {
            dir_FFiles <- list.files(dir_F)
            if (length(length(dir_FFiles)) == 0) {
              stop("Any file inside dir_F path. Are the files located there?")
            }
          }
        }
      }
    }
  }


  # ellipsis arguments
  # occurrence arguments
  if (is.null(col_sp)) col_sp <- "acceptedNameUsage"
  if (is.null(col_lat)) col_lat <- "decimalLatitude"
  if (is.null(col_lon)) col_lon <- "decimalLongitude"

  # Environmental variables
  if (is.null(dir_clim)) dir_clim <- "Data/env_vars/"
  if (is.null(dir_clim)) dir_other <- "Data/env_vars/other/"
  if (is.null(extension_vars)) extension_vars <- "*.tif$"

  # Bias management
  if (is.null(use_bias)) use_bias <- FALSE

  # Areas and projections of interest

  ##
  if (is.null(method_M)) method_M <- NULL

  ## Projections
  if (is.null(compute_G)) compute_G <- FALSE
  if (is.null(do_future)) do_future <- FALSE
  if (is.null(compute_F)) compute_F <- FALSE

  ########################################################### Algorithms
  if (is.null(algos)) algos <- "MAXENT" #c("MAXENT", "GBM", "ANN")

  ########################################################## Maxent details
  if (is.null(beta_5.25)) beta_5.25 <- seq(0.5, 4, 0.5)
  if (is.null(fc_5.25)) fc_5.25 <- c("l", "q", "lq")
  if (is.null(beta_25)) beta_25 <- seq(1, 6, 1)
  if (is.null(fc_25)) {
    fc_25 <- c(
      "lq", "lp", "lt", "lh", "qp", "qt", "qh", "lqp",
      "lqt", "lqh", "lpt", "lph", "qpt", "qph", "qth", "lqpt",
      "lqph", "lqth", "lpth", "lqpth"
    )
  }
  if (is.null(extrapo)) extrapo <- "no_ext"
  if (is.null(E)) E <- 10
  if (is.null(predic)) predic <- "kuenm"
  if (is.null(pckg)) pckg <- "ENMeval2"

  # Colineality
  if (is.null(cor_eval)) col_eval <- FALSE
  if (isTRUE(cor_eval)) { # as we have only one method now, get defaults
    if (is.null(cor_method)) col_method <- "VIF"
    if (is.null(cor_detail)) col_detail <- 3
  }

  # other arguments
  if (is.null(tipo)) tipo <- ""
  if (is.null(crs_proyect)) crs_proyect <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"
  if (is.null(kept)) kept <- FALSE
  if (is.null(keep_files)) keep_files <- "essential"
  if (is.null(transf_biomo_ext)) transf_biomo_ext <- TRUE
  if (is.null(redo)) redo <- FALSE
  if (is.null(redo_path)) redo_path <- NULL

  # to develop
  #  if (is.null(mxnt.pckg)) mxnt.pckg <- "kuenm" # kuenm, enmeval, sdmtune [MISSING] develop an structure in which the user can choose the package needed, it can be made by create an intermediary function heading to each method and sourcing the needed functions
  #  if (is.null(other.pckg)) other.pckg <- "biomod" # biomod, sdmtune [MISSING]
  # predic with maxnet #[MISSING]

  #--------------------------------------
  # 0. Setup
  #--------------------------------------

  message("Preparing folders and files")

  # 0.1 Calling individual functions

  source("R/1_format_rawocc.R")
  source("R/2_uniq1km.R")
  source("R/3_m.R") ## Overlaping occurrences, biogeographic units and Minimun Convex Polygon (MCP) by ENMeval: https://tinyurl.com/y3u3c6fj
  source("R/4_process_env.R")
  source("R/5_Bias.R")
  source("R/6A_1_doenmEVAL.R")
  source("R/6B_1_dosplit.R")
  source("R/6B_2_dokuenm.R")
  source("R/6B_3_dobiomod.R")
  source("R/6B_doindeva.R")
  source("R/7_doensemble.R")
  source("R/give.msg.time.R")
  source("R/doDE.MCP.R")

  # 0.2 Starting time

  time1 <- Sys.time()

  # 0.3 set and create species folder

  # extract the name of the species

  sp_name <- occ[1, col_sp] %>% gsub(pattern = " ", replacement = ".")

  if (tipo != "") {
    folder_sp <- paste0(sp_name, ".", tipo)
  } else {
    folder_sp <- sp_name
  }

  dir.create(folder_sp, showWarnings = F)

  # 0.4 writing occurrences data withouth processing, aka raw occurrences.

  dir.create(paste0(folder_sp, "/occurrences"), showWarnings = F)

  occ$occ.ID <- 1:nrow(occ)

  write.csv(occ, paste0(folder_sp, "/occurrences/occ_raw.csv"), row.names = F)


  # ----- tracking file

  filelog <- file(paste0(folder_sp, "/log_file.txt"), "w")

  linesmsg0 <- paste0(
    time1, "\n",
    "Species name: ", sp_name, "\n",
    "Number of raw occurrences ", nrow(occ), "\n",
    "Experimental type:", tipo, "\n",
    "\n",
    "Method for unique records ", uniq1k_method, "\n",
    "Distance used for unique records ", dist_uniq, " km", "\n",
    "\n",
    "Accesible area constructed with:", method_M,
    "Movement distance vector (buffer depends on this vector): ", dist_MOV, " km", "\n",
    "\n",
    "Projections", "\n",
    "Projecting models from ", proj_models, "\n",
    "Projecting to future ", do_future, "\n",
    "CRS projecting ", crs_proyect, "\n",
    "\n",
    "Climatic variables: ", clim_vars, "\n",
    "G extent path", area_G, "\n",
    "Computing G Variables ", compute_G, "\n",
    "Computing F Variables ", compute_F, "\n",
    "\n",
    "Algorithms used ", algos, "\n",
    "Bias layer used ", use_bias, "\n",
    "Path of bias layer", "bias_layer/aves_set16.tif", "\n",
    "\n",
    "Maxent detailed", "\n",
    "Maxent beta multiplier occurrences less than 25 ", paste0(beta_5.25, collapse = ","), "\n",
    "Maxent beta multiplier occurrences greater than 25 ", paste0(beta_25, collapse = ","), "\n",
    "Maxent feature classes occurrences less than 25 ", paste0(fc_5.25, collapse = ","), "\n",
    "Maxent feature classes occurrences greater than 25 ", paste0(fc_25, collapse = ","), "\n",
    "Maxent prediction settings ", extrapo, "\n",
    "Maxent prediction function ", predic, "\n",
    "\n",
    "Final data", "\n",
    "Store files ", keep_files, "\n",
    "#############################################################################", "\n"
  )

  writeLines(text = linesmsg0, con = filelog, sep = "\n")

  # Raster setup

  dir.create(paste0(folder_sp, "/Temp"), showWarnings = FALSE)
  rasterOptions(tmpdir = paste0(folder_sp, "/Temp"))

  #--------------------------------------
  # 1. formating data
  #--------------------------------------
  message("formating data")

  linesmsg1 <- tryCatch(
    exp = {
      occ_no_dup <- format_rawocc(occ. = occ, col.lon = col_lon, col.lat = col_lat,
                                  spp.col = col_sp)
      write.csv(occ_no_dup, paste0(folder_sp, "/occurrences/occ_no_dup.csv"), row.names = F)
      paste0(
        "Handling occurrences", "\n",
        "Number of not duplicated occurrences: ", nrow(occ_no_dup), "\n"
      )
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

  #--------------------------------------
  # 2. Unique occurrences to x km
  #--------------------------------------
  message(paste0("Thining database to ", dist_uniq, "km, using  ", uniq1k_method))

  linesmsg2 <- tryCatch(
    expr = {
      occ_thin <- do.uniq1km(
        occ. = occ_no_dup, col.lon = col_lon, col.lat = col_lat, sp.col = col_sp,
        sp.name = sp_name, uniq1k.method = uniq1k_method, uniqDist = dist_uniq
      )
      write.csv(occ_thin, paste0(folder_sp, "/occurrences/occ_thin.csv"), row.names = F)
      paste0(
        "Unique occurrences to ", dist_uniq, "km, using  ", uniq1k_method, "\n",
        "Unique occurrences: ok.", "\n",
        "Number of unique occurrences: ", nrow(occ_thin), "\n"
      )
    },
    error = function(error_message) {
      e <- conditionMessage(error_message)
      return(paste0("Thining database: fail.\nError R: ", e))
    }
  )

  #------- tracking file

  writeLines(text = linesmsg2, con = filelog, sep = "\n")

  ### -----------------------------

  try(
    exp = {
      if (nrow(occ_thin) <= 2) {
        linestime <- give.msg.time(time.1 = time1)
        #------- tracking file
        writeLines(
          text = paste(paste0(
            "Not enough occurrences.", "\n",
            "Stop"
          ), linestime),
          con = filelog, sep = "\n"
        )
        close(filelog)
        return("not enough occurrences")
      }
    }
  )



  #--------------------------------------
  # 3. Accessible Area.
  #--------------------------------------
  message("Constructing accesible area")

  linesmsg3 <- tryCatch(
    expr = {
      M_ <- inte_areas(
        occ. = occ_thin,  col.lon = col_lon, col.lat = col_lat, folder.sp = folder_sp, dist.Mov = dist_MOV,
        method.M = method_M, method.G = method_G, method.F = method_F, area.M = area_M,
        area.G = area_G, area.F = area_F, proj.models = proj_models,
        do.future = do_future, compute.F = compute_F, polygon.data = polygon_data
      )
      write.csv(M_$occurrences, paste0(folder_sp, "/occurrences/occ_jointID.csv"), row.names = F)
      paste("Accesible area: ok.", "\n")
    },
    error = function(error_message) {
      e <- conditionMessage(error_message)
      return(paste0("\nAccesible area: fail\nError R: ", e))
    }
  )

  #------- tracking file

  writeLines(text = linesmsg3, con = filelog, sep = "\n")


  #--------------------------------------
  # 4. Processing environmental layers
  #--------------------------------------
  message("Processing environmental layers")

  linesmsg4 <- tryCatch( # missing at copy, use a vector of names instead of numbers to select what layers to copy
    expr = {
      envars <- process_env.current(
        clim.dataset = clim_vars, clim.dir = dir_clim, exten = extension_vars,
        compute.G = compute_G, compute.F = compute_F, dir.G = dir_G, dir.F = dir_F, 
        cor.eval = cor_eval, cor.method = cor_method, cor.detail = cor_detail,
        crs.proyect = crs_proyect, shape.M = M_$shape_M, shape.G = M_$shape_G, shape.F = M_$shape_F,
        env.other = dir_other, folder.sp = folder_sp, do.future = do_future, proj.models = proj_models
      )
      paste0(
        "Processing environmental layers: ok.", "\n",
        "Path of climatic variables used ", dir_clim, "\n",
        "Path of other variables used ", dir_other, "\n",
        "Variables used ", paste0(envars$layernames, collapse = ",")
      )
    },
    error = function(error_message) {
      e <- conditionMessage(error_message)
      return(paste0("Processing environmental layers: fail.\nError R: ", e))
    }
  )

  #------- tracking file

  writeLines(text = linesmsg4, con = filelog, sep = "\n")

  linestime <- give.msg.time(time.1 = time1)
  writeLines(linestime, filelog)

  #--------------------------------------
  # 5. Bias file by species
  #--------------------------------------

  linesmsg5 <- tryCatch(
    expr = {
      if (use_bias == TRUE) {
        message("Procesing bias layer")
        BiasSp <- get_BiasSp()
        paste("Bias file development: ok", "\n")
      } else {
        BiasSp <- NULL
        paste("Bias file NOT developed", "\n")
      }
    },
    error = function(error_message) {
      e <- conditionMessage(error_message)
      return(paste0("Bias file development: fail.\nError R: ", e))
    }
  )

  #------- tracking file
  writeLines(text = linesmsg5, con = filelog, sep = "\n")

  linestime <- give.msg.time(time.1 = time1)
  writeLines(linestime, filelog)


  #--------------------------------------
  # 6. Paths of calibration and evaluation
  #--------------------------------------

  message("Calibrating and evaluating SDM's")
  
  #pckg <- match.arg(use., c("Dismo", "kuenm", "ENMeval2", "SDMTune", "Biomod"))

  if (nrow(M_$occurrences) >= 5 & nrow(M_$occurrences) <= 25) {
    
      ##########
      # Path A # Jackknife, enmeval maxent
      ##########

      message("Path A, calibrating and evaluating Maxent models")

      linesmsg6.1 <- tryCatch(
        expr = {
          PathAMaxent <- do.enmeval(
            occ. = M_$occurrences, bias.file = BiasSp, beta.mult = beta_5.25, f.clas = fc_5.25,
            env.Mdir = paste0(folder_sp, "/M_variables"), env.Gdir = paste0(folder_sp, "/G_variables"),
            env.Fdir = paste0(folder_sp, "/G_variables"), do.future = do_future, folder.sp = folder_sp,
            col.lon = col_lon, col.lat = col_lat, proj.models = proj_models, partitionMethod = "jackknife",
            use.bias = use_bias, crs.proyect = crs_proyect, extrap = extrapo, predic = predic,
            write.intfiles = FALSE, sp.name = sp_name, redo. = redo, redo.path = redo_path, E = E
          )
          paste("\nPath A, number occ less or equal to 25\nSmall samples Maxent modelling: ok.")
        },
        error = function(error_message) {
          e1 <- conditionMessage(error_message)
          return(paste0("\nPath A, number occ less or equal to 25 \nSmall samples Maxent modelling fail.\nError R: ", e1))
        } ## MISSING FIXING DISMO PREDIC
      )

      writeLines(linestime, filelog)

      writeLines(text = linesmsg6.1, con = filelog, sep = "\n")
      linestime <- give.msg.time(time.1 = time1)

      message("\nEnsembles")

      linesmsg6.2 <- tryCatch(
        expr = {
          enscurr <- currentEns_byAlg(
            ras.Stack = PathAMaxent$c_proj, data. = M_$occurrences,
            collon = col_lon, collat = col_lat, e = E, algorithm = "MAXENT",
            foldersp = folder_sp, tim = "current", esc.nm = "",
            crs.proyect = crs_proyect, transf.biomo.ext = transf_biomo_ext,
            areas = M_, compute.F = compute_F, proj.models = proj_models
          )

          if (do_future == TRUE) {
            layersF <- futAuxiliar(PathAMaxent$f_proj)

            for (f in 1:length(layersF)) {
              currentEns_byAlg(
                ras.Stack = layersF[[f]], data. = M_$occurrences,
                collon = col_lon, collat = col_lat, e = E, algorithm = "MAXENT",
                foldersp = folder_sp, tim = "future", esc.nm = names(layersF[f]),
                crs.proyect = crs_proyect, transf.biomo.ext = transf_biomo_ext,
                areas = M_, compute.F = compute_F, proj.models = proj_models
              )
            }
          }
          paste("\nEnsembles: ok.")
        },
        error = function(error_message) {
          e1 <- conditionMessage(error_message)
          return(paste0("\nEnsembles fail.\nError R: ", e1))
        }
      )

      writeLines(text = linesmsg6.2, con = filelog, sep = "\n")

      linestime <- give.msg.time(time.1 = time1)
      writeLines(linestime, filelog)
  }

  if (nrow(M_$occurrences) > 25) {
    ##########
    # Path B # split in test and train, kuenm maxent, biomod GBM y ANN
    ##########
    
    message("Path B")
    
    if(pckg == "ENMeval2"){
      linesmsg6.1 <- tryCatch(
        expr = {
          PathBMaxent <- do.enmeval(
            occ. = M_$occurrences, bias.file = BiasSp, beta.mult = beta_25, f.clas = fc_25,
            env.Mdir = paste0(folder_sp, "/M_variables"), env.Gdir = paste0(folder_sp, "/G_variables"),
            env.Fdir = paste0(folder_sp, "/G_variables"), do.future = do_future, folder.sp = folder_sp,
            col.lon = col_lon, col.lat = col_lat, proj.models = proj_models, partitionMethod = "block",
            use.bias = use_bias, crs.proyect = crs_proyect, extrap = extrapo, predic = predic,
            write.intfiles = FALSE, sp.name = sp_name, redo. = redo, redo.path = redo_path, E = E
          )
          paste("\nPath B, number occ less or equal to 25\nSmall samples Maxent modelling: ok.")
        },
        error = function(error_message) {
          e1 <- conditionMessage(error_message)
          return(paste0("\nPath B, number occ greater or equal to 25 \nSmall samples Maxent modelling fail.\nError R: ", e1))
        } ## MISSING FIXING DISMO PREDIC
      )
      
      writeLines(linestime, filelog)
      
      writeLines(text = linesmsg6.1, con = filelog, sep = "\n")
      linestime <- give.msg.time(time.1 = time1)
      
      message("\nEnsembles")
      
      linesmsg6.2 <- tryCatch(
        expr = {
          enscurr <- currentEns_byAlg(
            ras.Stack = PathBMaxent$c_proj, data. = M_$occurrences,
            collon = col_lon, collat = col_lat, e = E, algorithm = "MAXENT",
            foldersp = folder_sp, tim = "current", esc.nm = "",
            crs.proyect = crs_proyect, transf.biomo.ext = transf_biomo_ext,
            areas = M_, compute.F = compute_F, proj.models = proj_models 
            #bins = PathBMaxent$Bins
          )
          
          if (do_future == TRUE) {
            layersF <- futAuxiliar(PathBMaxent$f_proj)
            
            for (f in 1:length(layersF)) {
              currentEns_byAlg(
                ras.Stack = layersF[[f]], data. = M_$occurrences,
                collon = col_lon, collat = col_lat, e = E, algorithm = "MAXENT",
                foldersp = folder_sp, tim = "future", esc.nm = names(layersF[f]),
                crs.proyect = crs_proyect, transf.biomo.ext = transf_biomo_ext,
                areas = M_, compute.F = compute_F, proj.models = proj_models,
                #bins = PathBMaxent$Bins
              )
            }
          }
          paste("\nEnsembles: ok.")
        },
        error = function(error_message) {
          e1 <- conditionMessage(error_message)
          return(paste0("\nEnsembles fail.\nError R: ", e1))
        }
      )
      
      writeLines(text = linesmsg6.2, con = filelog, sep = "\n")
      
      linestime <- give.msg.time(time.1 = time1)
      writeLines(linestime, filelog)
    }
    
    
    if(pckg == "kuenm"){
      
      linesmsg6.1 <- tryCatch(
        expr = {
          PathBOcc <- dosplit(
            occ. = M_$occurrences, bias.file = BiasSp, folder.sp = folder_sp, col.lon = col_lon,
            col.lat = col_lat, use.bias = use_bias, env.M = envars$M
          )
          paste0("\nPath B, number occ greater than 25\nOcc splited: ok.")
        },
        error = function(error_message) {
          e1 <- conditionMessage(error_message)
          return(paste0("\nPath B, number occ greater than 25\nOcc splited: fail.\nError R: ", e1))
        }
      )
      
      writeLines(text = linesmsg6.1, con = filelog, sep = "\n")
      
      linesmsg6.2 <- tryCatch(
          expr = {
            PathBMaxent <- do.kuenm(
              occ. = PathBOcc, sp.name = sp_name, folder.sp = folder_sp,
              biasfile = "BiasfileM.asc", beta.mult = beta_25, fc.clas = fc_25, kept. = kept,
              maxent.path = getwd(), proj.models = proj_models, E = E,
              do.future = do_future, env.Mdir = paste0(folder_sp, "/M_variables"),
              env.Gdir = paste0(folder_sp, "/G_variables"),
              crs.proyect = crs_proyect, use.bias = use_bias, extrap = extrapo,
              write.intfiles = FALSE, redo. = redo, redo.path = redo_path
              # MISSING for Unix and macOs the automated input of biasfile, ready for windows
            )
            paste0(
              "\nLarge samples Maxent modelling: ok. Check Final models folder in the species directory folder."
            )
          },
          error = function(error_message) {
            e2 <- conditionMessage(error_message)
            return(paste0("\nLarge samples maxent: fail\nError R:", e2))
          }
        )
        
        writeLines(text = linesmsg6.2, con = filelog, sep = "\n")
        
        linestime <- give.msg.time(time.1 = time1)
        writeLines(linestime, filelog)
        
        
        message("\nEnsembles")
        
        linesmsg6.3 <- tryCatch(
          expr = {
            currentEns_byAlg(
              ras.Stack = PathBMaxent$c_proj, data. = M_$occurrences,
              collon = col_lon, collat = col_lat, e = E, algorithm = "MAXENT",
              foldersp = folder_sp, tim = "current", esc.nm = "",
              crs.proyect = crs_proyect, transf.biomo.ext = transf_biomo_ext,
              areas = M_, compute.F = compute_F, proj.models = proj_models,
              #bins = PathBMaxent$Bins
            )
            
            if (do_future == TRUE) {
              layersF <- futAuxiliar(fut.list.ras = PathBMaxent$f_proj)
              
              for (f in 1:length(layersF)) {
                currentEns_byAlg(
                  ras.Stack = layersF[[f]], data. = M_$occurrences,
                  collon = col_lon, collat = col_lat, e = E, algorithm = "MAXENT",
                  foldersp = folder_sp, tim = "future", esc.nm = names(layersF[f]),
                  crs.proyect = crs_proyect, transf.biomo.ext = transf_biomo_ext,
                  areas = M_, compute.F = compute_F, proj.models = proj_models,
                  #bins = PathBMaxent$Bins
                )
              }
            }
            
            paste("\nEnsembles current: ok.")
          },
          error = function(error_message) {
            e1 <- conditionMessage(error_message)
            return(paste0("\nEnsemblig: fail.\nError R: ", e1))
          }
        )
        writeLines(text = linesmsg6.3, con = filelog, sep = "\n")
        
        linestime <- give.msg.time(time.1 = time1)
        writeLines(linestime, filelog)
      
    }

    
    # # more algorithms than Maxent?
    # 
    # algos2 <- algos[which(algos != "MAXENT")]
    # 
    # if (length(algos2) != 0) {
    #   # Biomod (ANN, GBM)
    #   message("Path B, calibrating other algorithms than Maxent")
    # 
    #   linesmsg6.4 <- tryCatch(
    #     expr = {
    #       PathBOther <- do.biomod(
    #         data.splitted = PathBOcc, sp.name = sp_name, folder.sp = folder_sp,
    #         Biasfile = BiasSp, env.Mdir = paste0(folder_sp, "/M_variables"),
    #         env.Gdir = paste0(folder_sp, "/G_variables"), nrep.s = 10,
    #         do.future = do_future, proj.models = proj_models, crs.proyect = crs_proyect,
    #         algorithms = algos2, use.bias = use_bias
    #       )
    #       paste0(
    #         "\nLarge samples other algorithms modelling: ok. Check Final_models_biomod folder for predictions."
    #       )
    #     },
    #     errror = function(error_message) {
    #       e3 <- conditionMessage(error_message)
    #       return(paste0("\nBiomod modelling and ensembling: fail.\nError R: ", e3))
    #     }
    #   )
    # 
    #   writeLines(text = linesmsg6.4, con = filelog, sep = "\n")
    # 
    #   linestime <- give.msg.time(time.1 = time1)
    #   writeLines(linestime, filelog)
    # 
    #   message("\nEnsembles")
    # 
    #   linesmsg6.5 <- tryCatch(
    #     expr = {
    #       for (i in 1:length(algos2)) {
    #         layersalgo_i <- grep(pattern = algos2[i], names(PathBOther$c_proj))
    #         if (length(layersalgo_i) != 0) {
    #           projBalgo_i <- PathBOther$c_proj[[layersalgo_i]]
    #           currentEns_byAlg(
    #             ras.Stack = projBalgo_i, data. = M_$occurrences, collon = col_lon, collat = col_lat,
    #             e = E, algorithm = algos2[i], foldersp = folder_sp
    #           )
    #         }
    #       }
    # 
    #       paste0(
    #         "\nEnsembling: ok. Check Final_models_biomod folder for predictions."
    #       )
    #     },
    #     errror = function(error_message) {
    #       e3 <- conditionMessage(error_message)
    #       return(paste0("\nEnsembling: fail.\nError R: ", e3))
    #     }
    #   )
    #   writeLines(text = linesmsg6.5, con = filelog, sep = "\n")
    # 
    #   linestime <- give.msg.time(time.1 = time1)
    #   writeLines(linestime, filelog)
    # }
  }

  #--------------------------------------
  # End
  #--------------------------------------

  if (keep_files == "all") {
    erase <- ""
  }

  if (keep_files == "essential" | keep_files == "none") {
    erase <- c(
      paste0(folder_sp, "/F_variables"), paste0(folder_sp, "/G_variables"),
      paste0(folder_sp, "/proj_current_cal"),
      paste0(folder_sp, "/M_variables"), paste0(folder_sp, "/indEVA.csv"),
      paste0(folder_sp, "/proj_G_models"), paste0(folder_sp, "/BiasfileM.asc"),
      paste0(folder_sp, "/maxent.cache"),
      list.files(path = paste0(folder_sp, "/occurrences/"), pattern = "biomod.csv", full.names = T),
      list.files(path = paste0(folder_sp, "/occurrences/"), pattern = "kuenm.csv", full.names = T),
      list.files(path = paste0(folder_sp), pattern = ".out", full.names = T),
      paste0(folder_sp, "/", "final_models.bat"), paste0(folder_sp, "/", "candidate_models.bat"),
      "spatial_thin_log.txt", paste0(folder_sp, "/Temp"),
      list.files(path = paste0(folder_sp, "/"), pattern = ".asc$", full.names = T, recursive = T)
    )
    if (keep_files == "none") {
      folders.inside <- list.dirs(path = paste0(folder_sp, "/"), full.names = T, recursive = F)
      erase2 <- c(
        folders.inside[grep(folders.inside, pattern = "final_")], folders.inside[grep(folders.inside, pattern = "eval_")]
      )
      erase <- c(erase, erase2)
    }
  }

  for (i in 1:length(erase)) {
    if (file.exists(erase[i])) {
      unlink(erase[i], recursive = T, force = T)
    }
  }

  # time taken to execute

  linestime <- give.msg.time(time.1 = time1)

  writeLines(paste0("keep_files: ", keep_files, "\n", linestime), filelog)

  close(filelog)

  return(c("ok", sp_name))
}
