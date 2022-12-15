# Biomodelos 2 Routine: redoing models
#
# `Bio2_routine_redo` automates the fitting of Species Distribution Models from occurrence 
# and environmental data.

Bio2_routine_redo <- function(redo_sp_path, col_sp = NULL, col_lat = NULL, col_lon = NULL,
                         clim_vars, dir_clim = NULL, dir_other = NULL,
                         extension_vars = NULL, use_bias = NULL, TGS_kernel = NULL,  
                         dist_MOV = NULL, proj_models, method_G = NULL, area_M = NULL, area_G = NULL,
                         compute_G = NULL, dir_G = NULL, do_future = NULL, method_F = NULL,
                         area_F = NULL, compute_F = NULL, dir_F = NULL, polygon_data = NULL,
                         raster_data = NULL, algos = NULL,E = NULL, extrapo = NULL,
                         predic = NULL, crs_proyect = NULL, kept = NULL,
                         keep_files = NULL, transf_biomo_ext = NULL
) {
  
  # checking concatenated arguments and format of files
  
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
  
  ## Projections
  if (is.null(compute_G)) compute_G <- FALSE
  if (is.null(do_future)) do_future <- FALSE
  if (is.null(compute_F)) compute_F <- FALSE
  
  # Algorithms
  if (is.null(extrapo)) extrapo <- "no_ext"
  if (is.null(E)) E <- 10
  if (is.null(predic)) predic <- "kuenm"
  
  # other arguments
  if (is.null(crs_proyect)) crs_proyect <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"
  if (is.null(kept)) kept <- FALSE
  if (is.null(keep_files)) keep_files <- "essential"
  if (is.null(transf_biomo_ext)) transf_biomo_ext <- TRUE
  
  # to develop
  #  if (is.null(mxnt.pckg)) mxnt.pckg <- "kuenm" # kuenm, enmeval, sdmtune [MISSING] develop an structure in which the user can choose the package needed, it can be made by create an intermediary function heading to each method and sourcing the needed functions
  #  if (is.null(other.pckg)) other.pckg <- "biomod" # biomod, sdmtune [MISSING]
  # predic with maxnet #[MISSING]
  
  #--------------------------------------
  # 0. Setup
  #--------------------------------------
  
  print("Preparing folders and files")
  
  library(stringr)
  
  # 0.1 Calling individual functions
  
  source("R/4_process_env.R")
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
  
  # 0.3 Finding occurrence file inside the redoing species folder
  
  files_sp_path <- list.files(redo_sp_path,full.names = T, recursive = T)
  occ <- files_sp_path[grep(pattern = "occ_thin.csv", x = files_sp_path)] %>% read.csv()
  
  # 0.3 set species folder
  
  name <- redo_sp_path %>% split("/") %>% unlist()
  sp_name <- name[length(name)]
  folder_sp <- redo_sp_path

  # ----- tracking file
  
  x <- as.vector(read.delim(paste0(folder_sp, "/log_file.txt")))
  
  if (is.null(dist_MOV)) {
    index <- which(str_detect(x[,1], "Movement distance vector") == TRUE)
    str_index <- x[index,1] %>% str_split(pattern = " ") %>% unlist()
    dist_MOV <- str_index[length(str_index)-1] %>% as.numeric()
  }
  if (is.null(proj_models)) {
    index <- which(str_detect(x[,1], "Projecting models") == TRUE)
    str_index <- x[index,1] %>% str_split(pattern = " ") %>% unlist()
    proj_models <- str_index[length(str_index)]
  }
  
  
  filelog <- file(paste0(folder_sp, "/log_file_redo.txt"))
  library(stringr)
  
  
  linesmsg0 <- paste0(
    "##################################################################\n",
    "Redoing models\n",
    time1, "\n",
    "Species name: ", sp_name, "\n",
    "Number of thin occurrences ", nrow(occ), "\n",
    "\n",
    "Movement distance vector (buffer depends on this vector): ", dist_MOV, " km", "\n",
    "\n",
    "Projections", "\n",
    "Projecting models from ", proj_models, "\n",
    "Projecting to future ", do_future, "\n",
    "CRS projecting ", crs_proyect, "\n",
    "\n",
    #"Climatic variables: ", clim_vars, "\n",
    #"G extent path", area_G, "\n",
    #"Computing G Variables ", compute_G, "\n",
    #"Computing F Variables ", compute_F, "\n",
    "\n",
    "Bias layer used ", use_bias, "\n",
    "\n",
    "Maxent detailed", "\n",
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
  # 4. Processing environmental layers
  #--------------------------------------
  print("Processing environmental layers")
  
  linesmsg4 <- tryCatch( # missing at copy, use a vector of names instead of numbers to select what layers to copy
    expr = {
      envars <- process_env.current(
        clim.dataset = clim_vars, clim.dir = dir_clim, exten = extension_vars,
        crs.proyect = crs_proyect, shape.M = M_$shape_M, shape.G = M_$shape_G,
        shape.F = M_$shape_F, env.other = dir_other, folder.sp = folder_sp, 
        do.future = do_future, proj.models = proj_models, compute.G = compute_G, 
        compute.F = compute_F, dir.G = dir_G, dir.F = dir_F
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
  # 6. Paths of calibration and evaluation
  #--------------------------------------
  
  print("Calibrating and evaluating SDM's")
  
  if (nrow(M_$occurrences) >= 5 & nrow(M_$occurrences) <= 25) {
    if (length(which(algos == "MAXENT")) != 0) {
      
      ##########
      # Path A # Jackknife, enmeval maxent
      ##########
      
      print("Path A, calibrating and evaluating Maxent models")
      
      linesmsg6.1 <- tryCatch(
        expr = {
          PathAMaxent <- do.enmeval(
            occ. = M_$occurrences, bias.file = BiasSp, beta.mult = beta_5.25, f.class = fc_5.25,
            env.Mdir = paste0(folder_sp, "/M_variables"), env.Gdir = paste0(folder_sp, "/G_variables"),
            env.Fdir = paste0(folder_sp, "/G_variables"), do.future = do_future, folder.sp = folder_sp,
            col.lon = col_lon, col.lat = col_lat, proj.models = proj_models, partitionMethod = "jackknife",
            use.bias = use_bias, crs.proyect = crs_proyect, extrap = extrapo, predic = predic,
            write.intfiles = FALSE, sp.name = sp_name
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
      
      print("\nEnsembles")
      
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
  }
  
  if (nrow(M_$occurrences) > 25) {
    ##########
    # Path B # split in test and train, kuenm maxent, biomod GBM y ANN
    ##########
    
    print("Path B, spliting data")
    
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
    
    if (length(which(algos == "MAXENT")) != 0) {
      linesmsg6.2 <- tryCatch(
        expr = {
          PathBMaxent <- do.kuenm(
            occ. = PathBOcc, sp.name = sp_name, folder.sp = folder_sp,
            biasfile = "BiasfileM.asc", beta.mult = beta_25, fc.clas = fc_25, kept. = kept,
            maxent.path = getwd(), proj.models = proj_models, E = E,
            do.future = do_future, env.Mdir = paste0(folder_sp, "/M_variables"),
            env.Gdir = paste0(folder_sp, "/G_variables"),
            crs.proyect = crs_proyect, use.bias = use_bias, extrap = extrapo,
            write.intfiles = FALSE
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
      
      
      print("\nEnsembles")
      
      linesmsg6.3 <- tryCatch(
        expr = {
          currentEns_byAlg(
            ras.Stack = PathBMaxent$c_proj, data. = M_$occurrences,
            collon = col_lon, collat = col_lat, e = E, algorithm = "MAXENT",
            foldersp = folder_sp, tim = "current", esc.nm = "",
            crs.proyect = crs_proyect, transf.biomo.ext = transf_biomo_ext,
            areas = M_, compute.F = compute_F, proj.models = proj_models
          )
          
          if (do_future == TRUE) {
            layersF <- futAuxiliar(fut.list.ras = PathBMaxent$f_proj)
            
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
    
    
    # more algorithms than Maxent?
    
    algos2 <- algos[which(algos != "MAXENT")]
    
    if (length(algos2) != 0) {
      # Biomod (ANN, GBM)
      print("Path B, calibrating other algorithms than Maxent")
      
      linesmsg6.4 <- tryCatch(
        expr = {
          PathBOther <- do.biomod(
            data.splitted = PathBOcc, sp.name = sp_name, folder.sp = folder_sp,
            Biasfile = BiasSp, env.Mdir = paste0(folder_sp, "/M_variables"),
            env.Gdir = paste0(folder_sp, "/G_variables"), nrep.s = 10,
            do.future = do_future, proj.models = proj_models, crs.proyect = crs_proyect,
            algorithms = algos2, use.bias = use_bias
          )
          paste0(
            "\nLarge samples other algorithms modelling: ok. Check Final_models_biomod folder for predictions."
          )
        },
        errror = function(error_message) {
          e3 <- conditionMessage(error_message)
          return(paste0("\nBiomod modelling and ensembling: fail.\nError R: ", e3))
        }
      )
      
      writeLines(text = linesmsg6.4, con = filelog, sep = "\n")
      
      linestime <- give.msg.time(time.1 = time1)
      writeLines(linestime, filelog)
      
      print("\nEnsembles")
      
      linesmsg6.5 <- tryCatch(
        expr = {
          for (i in 1:length(algos2)) {
            layersalgo_i <- grep(pattern = algos2[i], names(PathBOther$c_proj))
            if (length(layersalgo_i) != 0) {
              projBalgo_i <- PathBOther$c_proj[[layersalgo_i]]
              currentEns_byAlg(
                ras.Stack = projBalgo_i, data. = M_$occurrences, collon = col_lon, collat = col_lat,
                e = E, algorithm = algos2[i], foldersp = folder_sp
              )
            }
          }
          
          paste0(
            "\nEnsembling: ok. Check Final_models_biomod folder for predictions."
          )
        },
        errror = function(error_message) {
          e3 <- conditionMessage(error_message)
          return(paste0("\nEnsembling: fail.\nError R: ", e3))
        }
      )
      writeLines(text = linesmsg6.5, con = filelog, sep = "\n")
      
      linestime <- give.msg.time(time.1 = time1)
      writeLines(linestime, filelog)
    }
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
