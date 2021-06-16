#' Biomodelos 2 Routine
#'
#' @param occ  Occurrence data at least must have species name, latitude, longitude, and date columns
#' @param col_sp
#' @param col_lat
#' @param col_lon
#' @param do_clean
#' @param drop_out any, IQR, "freq", "IQR"
#' @param IQR_mtpl
#' @param clim_vars Which climatic data use, useful when you want to compare fit of different climatic data sets
#' @param dir_clim
#' @param dir_other
#' @param extension_vars
#' @param uniq1k_method
#' @param dist_uniq
#' @param MCP_buffer
#' @param polygon_select
#' @param points_Buffer
#' @param polygon_M Spatial data to construct M composed, must be inside project file
#' @param raster_M solo se "prende cuando drop out es freq"
#' @param dist_MOV Movement distance of the group
#' @param proj_models "M-M", # "M-G
#' @param area_G solo se prende cuando projection es M-G, MISSING puede ser raster o shape
#' @param compute_G
#' @param dir_G
#' @param use_bias Where is the bias file, in case of do.bias active
#' @param TGS_kernel
#' @param algos
#' @param beta_5.25
#' @param fc_5.25
#' @param beta_25
#' @param fc_25
#' @param extrapo
#' @param predic
#' @param do_future
#' @param crs_proyect
#' @param tipo
#' @param kept
#' @param E
#' @param mxnt.pckg
#' @param other.pckg
#' @param compute_F
#' @param dir_F
#' @param keep_files
#' @param write_intfiles
#' @param transf_biomo_ext
#'
#' @return
#' @export
#'
#' @examples
Bio2_routine <- function(occ, col_sp = NULL, col_lat = NULL, col_lon = NULL, do_clean = NULL,
                         drop_out = "any", IQR_mtpl = NULL, clim_vars, dir_clim = NULL, dir_other = NULL,
                         extension_vars = NULL, uniq1k_method = NULL, dist_uniq = NULL,
                         MCP_buffer = NULL, polygon_select = NULL, points_Buffer = NULL, polygon_M = NULL,
                         raster_M = NULL, dist_MOV = NULL, proj_models, area_G = NULL, compute_G = NULL,
                         dir_G = NULL, use_bias = NULL, TGS_kernel = NULL, algos = NULL,
                         beta_5.25 = NULL, fc_5.25 = NULL, beta_25 = NULL, fc_25 = NULL,
                         extrapo = NULL, predic = NULL, do_future = NULL, crs_proyect = NULL,
                         tipo = NULL, kept = NULL, E = NULL, mxnt.pckg = NULL, other.pckg = NULL,
                         compute_F = NULL, dir_F = NULL, keep_files = NULL, write_intfiles = NULL,
                         transf_biomo_ext = NULL) {

  # checking concatenated arguments and format of files

  if (!exists("occ")) {
    stop("Provide an occurrence data base as a data.frame at least with species name,
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
        if (!exists("ln") | !exists("lt")) {
          stop("Longitude or latitude coordinates does not find at database. Please check the column names and retry.")
        } else {
          rm(ln, lt)
        }
      }
    }
  }

  if (!is.null(uniq1k_method)) {
    if (uniq1k_method != "sqkm" | uniq1k_method != "spthin") {
      stop("Provide a valid method to thin the database, either be sqkm or spthin.")
    } else {
      if (!is.null(dist_uniq)) {
        if (!is.numeric(dist_uniq)) stop("Provide a distance to thin the database, only numeric.")
      }
    }
  }

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
  }

  if (!exists("drop_out")) {
    stop("Provide a way to dropping out 'outliers' records in the data.base.")
  } else {
    if (!is.null(drop_out)) {
      if (drop_out != "any") {
        if (drop_out != "freq") {
          if (drop_out != "IQR") {
            stop("Provide a valid method for dropping out 'outliers' either 'any', 'freq' or 'IQR'. For more details, refer to general documentation and vigenettes.")
          }
        } else {
          if (is.null(raster_M)) stop("Provide a rasterize layer (readable by the raster package) of a shapefile to select frequencies (path non an object).")
        }
      }
    }
  }

  if (!is.null(MCP_buffer) | !is.null(points_Buffer)) {
    if (!is.null(dist_MOV)) {
      if (!is.numeric(dist_MOV)) {
        stop("Provide a numeric distance to construct buffer of points_buffer or Minimun convex polygon.")
      }
    }
  }

  if (exists("polygon_select")) {
    if (isTRUE(polygon_select)) {
      if (is.null(polygon_M)) stop("Provide a shapefile ('.shp') to select polygons from in polygon_M argument (path non an object).")
    }
  }

  if (!exists("proj_models")) {
    stop("You need to provide a method to calibrate and project the models, either be M-M or M-G.")
  } else {
    if (proj_models == "M-G") {
      if (compute_G == TRUE) {
        if (is.null(area_G)) {
          stop("Provide a raster file of an area diferent to M in order to project the models.")
        } else {
          try(
            rtemp <- raster::raster(area_G)
          )
          if (!exists("rtemp")) stop("Provide a raster file supported by the raster package. See documentation.")
        }
        rm(rtemp)
      } else {
        if (is.null(dir_G)) {
          stop("Provide a path directory in which are stored the variables pre-processed using G as geographic extent.")
        } else {
          if (!dir.exists(dir_G)) {
            stop("Directory of G variables does not exist, please provide a valid one.")
          } else {
            dir_GFiles <- list.files(dir_G)
            if (length(length(dir_GFiles)) == 0) {
              stop("Any file inside dir_G path. Are the files located there?")
            }
          }
        }
      }
    }
  }

  if (!is.null(extrapo)) {
    if (extrapo != "all") {
      if (extrapo != "ext_clam") {
        if (extrapo != "ext") {
          if (extrapo != "no_ext") stop("Provide a valid type to make the projections.")
        }
      }
    }
  }

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

  # ellipsis arguments
  # occurrence arguments
  if (is.null(col_sp)) col_sp <- "acceptedNameUsage" # Which is the species name column
  if (is.null(col_lat)) col_lat <- "decimalLatitude" # Which is the latitude coordinate name column
  if (is.null(col_lon)) col_lon <- "decimalLongitude" # Which is the longitude coordinate name column
  if (is.null(do_clean)) do_clean <- FALSE
  if (is.null(drop_out)) drop_out <- "any"
  if (is.null(IQR_mtpl)) IQR_mtpl <- 5
  
  # Environmental variables 
  if (is.null(dir_clim)) dir_clim <- "Data/env_vars/"
  if (is.null(dir_clim)) dir_other <- "Data/env_vars/other/"
  if (is.null(extension_vars)) extension_vars <- "*.tif$" #### ?Solo aceptara tifs? como hacer para que lea solamente archivos que pueda usar raster
  
  # Bias management
  if (is.null(uniq1k_method)) uniq1k_method <- "sqkm" # "spthin" #MISSING user choose the grid
  if (is.null(dist_uniq)) dist_uniq <- 1
  if (is.null(use_bias)) use_bias <- FALSE
  
  # Accessible area
  if (is.null(MCP_buffer)) MCP_buffer <- FALSE
  if (is.null(polygon_select)) polygon_select <- FALSE
  if (is.null(points_Buffer)) points_Buffer <- TRUE
  
  # Projections
  if (is.null(compute_G)) compute_G <- FALSE
  if (is.null(do_future)) do_future <- FALSE
  if (is.null(compute_F)) compute_F <- FALSE
  
  #Algorithms
  if (is.null(algos)) algos <- c("MAXENT", "GBM", "ANN")
  if (is.null(beta_5.25)) beta_5.25 <- seq(0.5, 4, 0.5)
  if (is.null(fc_5.25)) fc_5.25 <- c("l", "q", "lq") # solo minusculas
  if (is.null(beta_25)) beta_25 <- seq(1, 6, 1)
  if (is.null(fc_25)) {
    fc_25 <- c(
      "lq", "lp", "lt", "lh", "qp", "qt", "qh", "pt", "ph", "th", "lqp",
      "lqt", "lqh", "lpt", "lph", "qpt", "qph", "qth", "pth", "lqpt",
      "lqph", "lqth", "lpth", "lqpth"
    )
  if (is.null(extrapo)) extrapo <- "no_ext"
  if (is.null(E)) E <- 5
  if (is.null(predic)) predic <- "kuenm" # dismo #missing maxnet
  
  # other arguments
  if (is.null(tipo)) tipo <- "" # optional, in case of experiment (it is attached to folder sp name created)*
  if (is.null(crs_proyect)) crs_proyect <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"
  if (is.null(kept)) kept <- FALSE # kuenm argument to clean competitor models
  if (is.null(keep_files)) keep_files <- "essential"
  if (is.null(write_intfiles)) write_intfiles <- FALSE
  if (is.null(transf_biomo_ext)) transf_biomo_ext <- TRUE
  
  # to reorganize
  # if (is.null(date_period)) date_period <- "1970-01-01" # "From" date to limit chronologically occurrence data "year-month-day"
  # if (is.null(event_date)) event_date <- "eventDate"
  
  # to develop
  #  if (is.null(mxnt.pckg)) mxnt.pckg <- "kuenm" # kuenm, enmeval, sdmtune [MISSING] develop an structure in which the user can choose the package needed, it can be made by create an intermediary function heading to each method and sourcing the needed functions
  #  if (is.null(other.pckg)) other.pckg <- "biomod" # biomod, sdmtune [MISSING]
  
  #--------------------------------------
  # 0. Setup
  #--------------------------------------

  print("Preparing folders and files")

  # 0.1 Calling individual functions

  source("R/1_clean_rawocc.R")
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
    "Cleaning occurrences (Clean coordinates)", do_clean, "\n",
    "Outliers manage by ", drop_out, "\n",
    "Method for unique records ", uniq1k_method, "\n",
    "Distance used for unique records ", dist_uniq, " km", "\n",
    "\n",
    "Accesible area constructed with:", "Buffer points ", points_Buffer,
    ", Minimun convex poligon ", MCP_buffer, ", Selecting polygons ", polygon_select, "\n",
    "Polygon shapefile for accesible area: ", polygon_M, "\n",
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
    "Store intermediate ASC files ", write_intfiles, "\n",
    "#############################################################################", "\n"
  )

  writeLines(text = linesmsg0, con = filelog, sep = "\n")

  # Raster setup

  dir.create(paste0(folder_sp, "/Temp"), showWarnings = FALSE)
  rasterOptions(tmpdir = paste0(folder_sp, "/Temp"))

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
      paste0(
        "Handling occurrences", "\n",
        "Clean occurrences (Clean coordinates): ", as.character(do_clean), "\n",
        "Number of not duplicated occurrences: ", nrow(occClean), "\n"
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
            "Not enough occurrences. Distribution estimated by rasterize a MCP,", "\n",
            "Stop", "\n"
          ), linestime),
          con = filelog, sep = "\n"
        )
        close(filelog)
        return("not enough occurrences")
      }
    }
  )

  #--------------------------------------
  # 2. Unique occurrences to x km
  #--------------------------------------
  print(paste0("Thining database to ", dist_uniq, "km, using  ", uniq1k_method))

  linesmsg2 <- tryCatch(
    expr = {
      occ_thin <- do.uniq1km(
        occ. = occClean, col.lon = col_lon, col.lat = col_lat, sp.col = col_sp,
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
      if (nrow(occ_thin) <= 5) {
        do.DE.MCP(
          occ. = occ_thin, col.lon = col_lon, col.lat = col_lat, folder.sp = folder_sp,
          dist.Mov = dist_MOV
        )
        linestime <- give.msg.time(time.1 = time1)
        #------- tracking file
        writeLines(
          text = paste(paste0(
            "Not enough occurrences. Distribution estimated by rasterize a MCP", "\n",
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
  # 3. Accesible Area.
  #--------------------------------------
  print("Constructing accesible area")

  linesmsg3 <- tryCatch(
    expr = {
      M_ <- M_area(
        polygon.M = polygon_M, raster.M = raster_M, occ. = occ_thin, col.lon = col_lon,
        col.lat = col_lat, folder.sp = folder_sp, dist.Mov = dist_MOV, drop.out = drop_out,
        MCPbuffer = MCP_buffer, polygon.select = polygon_select, pointsBuffer = points_Buffer
      )

      write.csv(M_$occurrences, paste0(folder_sp, "/occurrences/occ_jointID.csv"), row.names = F)
      paste("Accesible area: ok.", "\n")
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
  # 4. Processing environmental layers
  #--------------------------------------
  print("Processing environmental layers")

  linesmsg4 <- tryCatch( # missing at copy, use a vector of names instead of numbers to select what layers to copy
    expr = {
      envars <- process_env.current(
        clim.dataset = clim_vars, clim.dir = dir_clim, extension = extension_vars,
        crs.proyect = crs_proyect, area.M = M_$shape_M, area.G = area_G,
        env.other = dir_other, folder.sp = folder_sp, dofuture = do_future,
        proj.models = proj_models, compute.G = compute_G, compute.F = compute_F,
        dir.G = dir_G, dir.F = dir_F
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

  #--------------------------------------
  # 5. Bias file by species
  #--------------------------------------


  linesmsg5 <- tryCatch(
    expr = {
      if (use_bias == TRUE) {
        print("Procesing bias layer")
        BiasSp <- get_BiasSp(
          data. = M_$occurrences, TGS.kernel = TGS_kernel, shape.M = M_$shape_M, env.M = envars$M,
          ext = "*.asc", folder.sp = folder_sp, col.lon = col_lon, col.lat = col_lat,
          col.sp = col_sp
        )
        paste("Bias file development: ok", "\n")
      } else {
        BiasSp <- NULL
        paste("Bias file NO developed", "\n")
      }
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
            write.intfiles = write_intfiles
          )
          paste("\nPath A, number occ less or equal to 25\nSmall samples Maxent modelling: ok.")
        },
        error = function(error_message) {
          e1 <- conditionMessage(error_message)
          return(paste0("\nPath A, number occ less or equal to 25 \nSmall samples Maxent modelling fail.\nError R: ", e1))
        } ## MISSING FIXING DISMO PREDIC
      )

      writeLines(text = linesmsg6.1, con = filelog, sep = "\n")

      print("\nEnsembles")

      linesmsg6.2 <- tryCatch(
        expr = {
          enscurr <- currentEns_byAlg(
            ras.Stack = PathAMaxent$c_proj, data. = M_$occurrences,
            collon = col_lon, collat = col_lat, e = 10, algorithm = "MAXENT",
            foldersp = folder_sp, tim = "current", esc.nm = "",
            crs.proyect = crs_proyect, transf.biomo.ext = transf_biomo_ext
          )

          if (do_future == TRUE) {
            layersF <- futAuxiliar(PathAMaxent$f_proj)

            for (f in 1:length(layersF)) {
              currentEns_byAlg(
                ras.Stack = layersF[[f]], data. = M_$occurrences,
                collon = col_lon, collat = col_lat, e = 10, algorithm = "MAXENT",
                foldersp = folder_sp, tim = "future", esc.nm = names(layersF[f]),
                crs.proyect = crs_proyect, transf.biomo.ext = transf_biomo_ext
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
            maxent.path = getwd(), selection. = "OR_AICc", proj.models = proj_models,
            do.future = do_future, env.Mdir = paste0(folder_sp, "/M_variables"),
            env.Gdir = paste0(folder_sp, "/G_variables"),
            crs.proyect = crs_proyect, use.bias = use_bias, extrap = extrapo,
            write.intfiles = write_intfiles
            # MISSING for Unix and macOs the automated input of biasfile, ready for windows
          ) ####### MISSING
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

      print("\nEnsembles")

      linesmsg6.3 <- tryCatch(
        expr = {
          currentEns_byAlg(
            ras.Stack = PathBMaxent$c_proj, data. = M_$occurrences,
            collon = col_lon, collat = col_lat, e = 5, algorithm = "MAXENT",
            foldersp = folder_sp, tim = "current", esc.nm = "",
            crs.proyect = crs_proyect, transf.biomo.ext = transf_biomo_ext
          )

          if (do_future == TRUE) {
            layersF <- futAuxiliar(fut.list.ras = PathBMaxent$f_proj)

            for (f in 1:length(layersF)) {
              currentEns_byAlg(
                ras.Stack = layersF[[f]], data. = M_$occurrences,
                collon = col_lon, collat = col_lat, e = 5, algorithm = "MAXENT",
                foldersp = folder_sp, tim = "future", esc.nm = names(layersF[f]),
                crs.proyect = crs_proyect, transf.biomo.ext = transf_biomo_ext
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

      print("\nEnsembles")

      linesmsg6.5 <- tryCatch(
        expr = {
          for (i in 1:length(algos2)) {
            layersalgo_i <- grep(pattern = algos2[i], names(PathBOther$c_proj))
            if (length(layersalgo_i) != 0) {
              projBalgo_i <- PathBOther$c_proj[[layersalgo_i]]
              currentEns_byAlg(
                ras.Stack = projBalgo_i, data. = M_$occurrences, collon = col_lon, collat = col_lat,
                e = 5, algorithm = algos2[i], foldersp = folder_sp ############ MISSING let user choice e
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

  writeLines(linestime, filelog)

  close(filelog)

  return(c("ok", sp_name))
}
