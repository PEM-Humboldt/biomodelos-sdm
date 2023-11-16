#' fit_biomodelos automates the fitting of Species Distribution Models from occurrence and environmental data
#' 
#' @description  'fit_biomodelos' is a function that automates the process of fitting Species Distribution 
#' Models (SDMs) using occurrence and environmental data. The function follows a flexible and automated general 
#' routine to fit the SDMs. It formats occurrence data, constructs geographical areas, crops and masks environmental 
#' variables, trains SDMs using one or several algorithms, evaluates them quantitatively, and ensembles the best 
#' of each one. It also projects to different scenarios based on user input.
#' #' @param occ data frame containing occurrence data of the species of interest. Must have columns for longitude 
#' (col_lon), latitude (col_lat) and species name. Default 'acceptedNameUsage'
#' @param col_lon character string representing the name of the column in occ data.frame that contains the longitude data.
#' Default 'decimalLongitude'
#' @param col_lat character string representing the name of the column in occ data.frame that contains the latitude data.
#' Default 'decimalLatitude' if (is.null(col_sp)) col_sp <- "acceptedNameUsage"
#' @param clim_vars character string name of the folder which stores the environmental variables. It would be created using
#' the 'do.folder.structure' function. No default
#' @param dir_clim character string specifying the directory containing climate data for the current time period.
#' Default it takes the path 'env_vars/your_clim_vars/'
#' @param dir_other optional character string specifying the directory containing other environmental data. Default
#' it takes the path 'env_vars/other/'
#' @param file_xtension character string specifying the file extension of the data files to be processed. Default "*.tif"
#' @param remove_method character string indicating the method for removing duplicate occurrences. Two options:
#' "sqkm" uses clean_dup function from ntbox package by Luis Osorio 
#' https://github.com/luismurao/ntbox/blob/master/R/clean_dup.R or "spthin" uses thin function from package sp_thin
#' https://rdrr.io/cran/spThin/man/thin.html
#' @param remove_distance numeric value indicating the distance threshold in kilometers for considering occurrences 
#' as duplicates 
#' @param use_bias logical, calculates the bias of a set of species occurrences using the Target Group Sampling (TGS) 
#' method. Default is FALSE
#' @param TGS_kernel character string of the path to the raster file with the kernel for TGS. TGS estimates sampling 
#' by using the presence locations of taxonomically related species observes using the same techniques as the focal 
#' species, under the assumption that those surveys would have recorded the focal species had it occurred there. 
#' Default is NULL.
#' @param proj_models character string, it would be "M-M" or "M-G".The parameter specifies if the training area 
#' is the same as the projection area. M area can be define as the accessible area in which algorithm is trained, 
#' G area in which is going to be projected the trained model in current time or same as M.
#' @param area_M character string, file path to the raster or shape file defining the M area, in case of not
#' using any optional method, i.e an user pre-processed area to train ecological niche models. 
#' @param method_M character string, method to define the area to train ecological niche models. See details for options.
#' Default is set to 'points_buffer'.
#' @param dist_MOV numeric, maximum distance in kilometers to make buffer. It must be set following displacement 
#' behavior of the species to work. 
#' @param area_G character string, file path to the raster or shape file defining the M area, in case of not
#' using any optional method, i.e an user pre-processed area to train ecological niche models. It is used if compute_G 
#' is set to TRUE and proj_models is "M-G".
#' @param method_G character string, method to define the area to project with current climate. It is used if
#' compute_G is set to TRUE and proj_models is "M-G". 
#' @param compute_G logical value indicating whether to compute data for the "G" projection area. It can be useful
#' if the user pre-processed G environmental variables which are stored in a folder. 
#' @param dir_G character string specifying the directory containing environmental variables data for the "G" 
#' projection area. The quantity of layers and their names have to be equal to variables of "M" area.
#' @param do_future logical value indicating whether to process data for future scenarios
#' @param method_F character string, method to define the area to project with future climate. It is used if
#' do.future and compute.F are set to TRUE
#' @param area_F character string, file path to the raster or shape file defining the F area, in case of not
#' using any optional method, i.e an user pre-processed area to project ecological niche models in diferent temporal.
#' scenarios. It is used if compute_F is TRUE. 
#' is set to TRUE and proj_models is "M-G".
#' @param polygon_data character string, file path to shapefile in order to extract areas of interest. See details for
#' more information. Default is NULL.
#' @param compute_F logical value indicating whether to compute data for the "F" projection area. It can be useful
#' if the user pre-processed F environmental variables which are stored in a folder. 
#' @param dir_F character string specifying the directory containing data for the "F" future area. The quantity
#' of layers and their names have to be equal to variables of "M" area.
#' @param cor_eval logical value indicating whether to perform correlation analysis on the environmental variables
#' @param cor_method character string specifying the method to be used for correlation analysis (e.g. "VIF")
#' @param cor_detail numeric specifying details for correlation analysis, like the numeric value specifying 
#' the VIF threshold. Default is set to 10.
#' @param beta_small_sample numeric value representing the regularization multiplier to be used in Maxent model 
#' calibration when there is a small sample. Small sample for Maxent is considered here as less than 20 occurrences.
#' @param fc_small_sample character string indicating the feature class function to be used in Maxent model 
#' calibration (e.g. 'l', 'lq') when there is a small sample. Small sample for Maxent is considered here as less 
#' than 20 occurrences.
#' @param beta_large_sample numeric value representing the regularization multiplier to be used in Maxent model 
#' calibration when there is a large sample. Large sample for Maxent is considered here as equal or more than 20 
#' occurrences.
#' @param fc_large_sample character string indicating the feature class function to be used in Maxent model 
#' calibration (e.g. 'l', 'lq') when there is a large sample. Large sample for Maxent is considered here as 
#' equal or more than 20 occurrences.
#' @param E numeric threshold the percentage of training data omission error allowed. See 
#' @param extrapo character string extrapolation type of projections following kuenm_mod; can be: "all" (all three of the options 
#' listed), "ext_clam" (extrapolation with clamping), "ext" (free extrapolation), and "no_ext" (no extrapolation)
#' @param kept logical, if FALSE, all candidate models created with kuenm will be erased after evaluation. 
#' Default set as FALSE.
#' @param maxent_package character string, package to use to calibrate SDM's using large sample occurrence data. It
#' would be 'kuenm' or 'enmeval'. Default set as 'enmeval'
#' @param crs_proyect character string indicating which set of environmental layers to use for modeling. If 'M-M', 
#' only the M layers will be used. If 'M-G', both M and G layers will be used. Default set as 
#' "+proj=longlat +datum=WGS84 +no_defs +type=crs"
#' @param type character string for adding as a suffixes to species name folder. Default set as NULL 
#' @param erase_files character string representing if intermediate date and models are going to be erased. 
#' Three options are possible "none" (not erase any file or folder, if kept is TRUE it maintains calibrated models), 
#' "essential" (erase 'Temp' folder, calibrated models, RData files storing models, shaped occurrences for specific
#' packages, and processed environmental variables), "all" (erase final_model folder and information relate to evaluated
#' models, if used it will prevent the use of a redo model process) 
#' @param transf_biomo_ext logical if TRUE, resulted models will have extension of the biomodelos windows being a 
#' rectangular extension with left limit longitude: -83, right limit longitude: -60, upper limit latitude: 13, 
#' and lower limit latitude: -14, decimal coordinates - WGS84 reference system. Default set as TRUE.
#' @param redo logical if TRUE a redo model process is performed. This process use the best calibrated model in a 
#' previous run to be projected in different scenarios like a new area G or F. In order to be completely a replicated 
#' model you must to locate the same environmental variables in the other and climate folders.  
#' @param redo_path character string representing the path of the species folder that will be subjected to a model 
#' reprocessing.
#' @param outformat outformat, (character) the model output format; it can be: "raw", "logistic", "cloglog", or "cumulative"
#' default "cloglog".
#' @param Max_Bg = NULL, 
#' @param wr_Bin_Matrix = NULL, 
#' @param selection = NULL,
#' @param algo_enmeval = NULL
#' 
#' @details 
#' remove_method and remove_distance are used in the 'remove_spat_duplicates' function. The function offers two methods 
#' for removing duplicates: 'remove_method' (1) a spatial thinning approach using the spThin package 'spthin', and (2) 
#' a distance-based approach using a distance threshold. The function returns a data frame with unique occurrences 'sqkm', 
#' based on the chosen method and distance threshold.
#' 
#' The method.M, method.G and method.F are parameters defined by character string that specifies the method to 
#' define each interest area.  The method parameter can take the following values: 
#' "points_buffer": This method generates a buffer around the occurrence points (given by dist_Mov). 
#' "points_MCP": This method generates a minimum convex polygon around the occurrence points. 
#' "points_MCP_buffer": This method generates a minimum convex polygon around the occurrence points, applies a buffer
#' of a specified distance (given by dist_Mov) around the polygon, and then clips the polygon to the study area boundary.
#' "polygon_points": This method intersects a biogeographic area multi-polygon (given by polygon_data) with the 
#' occurrence points to create a new polygon that covers the study area and contains all occurrence points.
#' "polygon_buffer": This method intersects a biogeographic area multi-polygon (given by polygon_data) with the 
#' occurrence points and then create a buffer of a specified distance (given by dist.Mov) around the selected polygon
#' to create a new polygon that covers the study area and contains all occurrence points and a buffer.
#' "polygon_points_buffer": This method intersects a biogeographic area multi-polygon (given by polygon_data) with 
#' a buffer of a specified distance (given by dist_Mov) around the occurrence points, then clips the resulting polygon 
#' to the study area boundary. 
#' "polygon_MCP": This method intersects a study area polygon (given by polygon.data) with a minimum convex polygon 
#' around the occurrence points to create a new polygon that covers the study area and contains all occurrence points.


fit_biomodelos <- function(occ, col_sp = NULL, col_lat = NULL, col_lon = NULL, clim_vars, dir_clim = NULL, 
                           dir_other = NULL, file_extension = NULL, remove_method = NULL, remove_distance = NULL, 
                           use_bias = NULL, TGS_kernel = NULL, proj_models, area_M = NULL, method_M = NULL, 
                           dist_MOV = NULL, area_G = NULL, method_G = NULL, compute_G = NULL, dir_G = NULL,
                           do_future = NULL, method_F = NULL, area_F = NULL, polygon_data = NULL, compute_F = NULL, 
                           dir_F = NULL, cor_eval = NULL, cor_method = NULL, cor_detail = NULL, 
                           beta_small_sample = NULL, fc_small_sample = NULL, beta_large_sample = NULL, 
                           fc_large_sample = NULL, E = NULL, extrapo = NULL, kept = NULL, maxent_package = NULL, 
                           crs_proyect = NULL, type = NULL, erase_files = NULL, transf_biomo_ext = NULL, redo = NULL, 
                           redo_path = NULL, outformat = NULL, Max_Bg = NULL, wr_Bin_Matrix = NULL, selection = NULL,
                           algo_enmeval = NULL, sbg_file = NULL
                         # other.pckg = NULL, algos = NULL deprecated for problems with BIOMOD2
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
        } else if (!is.null(area_G)) {
          Gstr <- tail(unlist(strsplit(area_G, "\\.")), n = 1)
          if (Gstr == "shp") {
            rtemp <- terra::vect(area_G)
          }
          if (Gstr == "tif"){ # 
            rtemp <- terra::rast(area_G)
          } 

          if (!exists("rtemp")) {
            stop("Provide a raster or shapefile supported by the raster package. See documentation.")
          } else {
            rm(rtemp)
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
  
  if(!is.null(use_bias)){
    if(use_bias == TRUE){
      if (is.null(TGS_kernel)) {
        stop("Provide a path directory in which are stored the future variables pre-processed using M or G as geographic extent.")
      } else {
        if (!file.exists(TGS_kernel)) {
          stop("File of TGS_kernel does not exist, please provide a valid one.")
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
  if (is.null(file_extension)) file_extension <- "*.tif$"

  # Bias management
  if (is.null(use_bias)) use_bias <- FALSE

  # Areas and projections of interest

  ##
  if (is.null(method_M)) method_M <- NULL

  ## Projections
  if (is.null(compute_G)) compute_G <- FALSE
  if (is.null(do_future)) do_future <- FALSE
  if (is.null(compute_F)) compute_F <- FALSE

  #-----------------------------------
  # Algorithms
  #if (is.null(algos)) algos <- "MAXENT" #c("MAXENT", "GBM", "ANN")

  #----------------------------------- 
  # Maxent details
  
  if (is.null(beta_small_sample)) beta_small_sample <- seq(0.5, 4, 0.5)
  if (is.null(fc_small_sample)) fc_small_sample <- c("l", "q", "lq")
  if (is.null(beta_large_sample)) beta_large_sample <- seq(1, 6, 1)
  if (is.null(fc_large_sample)) {
    fc_large_sample <- c( "l", "q", "lq", "lp", "lqp", "qp")
  }
  if (is.null(extrapo)) extrapo <- "no_ext"
  if (is.null(E)) E <- 10
  if (is.null(maxent_package)) maxent_package <- "enmeval"
  if (is.null(outformat)) outformat <- "cloglog"
  if (is.null(Max_Bg)) Max_Bg <- 10000
  if (is.null(selection)) selection <- c("proc", "daic", "or")
  if (is.null(algo_enmeval)) algo_enmeval <-  "maxent.jar"
  
  
  #----------------------------------- 

  # Colineality
  if (is.null(cor_eval)) col_eval <- FALSE
  if (isTRUE(cor_eval)) { # as we have only one method now, get defaults
    if (is.null(cor_method)) col_method <- "VIF"
    if (is.null(cor_detail)) col_detail <- 10
  }

  # ensembles
  if (is.null(wr_Bin_Matrix)) wr_Bin_Matrix <- FALSE
  
  # other arguments
  if (is.null(type)) type <- ""
  if (is.null(crs_proyect)) crs_proyect <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"
  if (is.null(kept)) kept <- FALSE
  if (is.null(erase_files)) erase_files <- "essential"
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

  source("R/format_occ_data.R")
  source("R/remove_spat_duplicates.R")
  source("R/define_interest_areas.R")
  source("R/apply_vif_analysis.R")
  source("R/process_env_variables.R")
  source("R/estimate_sampling_bias.R")
  source("R/do_enmeval.R")
  source("R/make_split_occ.R")
  source("R/do_kuenm.R")
  source("R/do_biomod.R")
  source("R/apply_independent_evaluation.R")
  source("R/do_bioclim.R")
  source("R/make_ensembles.R")
  source("R/give.msg.time.R")

  # 0.2 Starting time

  time1 <- Sys.time()

  # 0.3 set and create species folder

  # extract the name of the species

  sp_name <- occ[1, col_sp] %>% gsub(pattern = " ", replacement = ".")

  if (type != "") {
    folder_sp <- paste0(sp_name, ".", type)
  } else {
    folder_sp <- sp_name
  }

  dir.create(folder_sp, showWarnings = F)

  # 0.4 writing occurrences data withouth processing, aka raw occurrences.

  dir.create(paste0(folder_sp, "/occurrences"), showWarnings = F)

  occ$occ.ID <- 1:nrow(occ)

  write.csv(occ, paste0(folder_sp, "/occurrences/raw_occ.csv"), row.names = F)


  # ----- tracking file

  filelog <- file(paste0(folder_sp, "/log_file.txt"), "w")

  linesmsg0 <- paste0(
    time1, "\n",
    "Species name: ", sp_name, "\n",
    "Number of raw occurrences ", nrow(occ), "\n",
    "Experimental type:", type, "\n",
    "\n",
    "Method for unique records ", remove_method, "\n",
    "Distance used for unique records ", remove_distance, " km", "\n",
    "\n",
    "Accesible area constructed with:", method_M, "\n",
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
    #"Algorithms used ", algos, "\n",
    "Bias layer used ", use_bias, "\n",
    "Path of bias layer", "bias_layer/aves_set16.tif", "\n",
    "\n",
    "Maxent detailed", "\n",
    "Maxent beta multiplier occurrences small sample ", paste0(beta_small_sample, collapse = ","), "\n",
    "Maxent beta multiplier occurrences large sample ", paste0(beta_large_sample, collapse = ","), "\n",
    "Maxent feature classes occurrences small sample ", paste0(fc_small_sample, collapse = ","), "\n",
    "Maxent feature classes occurrences large sample ", paste0(fc_large_sample, collapse = ","), "\n",
    "Maxent prediction settings ", extrapo, "\n",
    "\n",
    "Selection Method ", paste0(selection, collapse = ","), "\n",
    "\n",
    "Final data", "\n",
    "Store files ", erase_files, "\n",
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
      formated_occ <- format_occ_data(occ. = occ, col.lon = col_lon, col.lat = col_lat,
                                  spp.col = col_sp)
      write.csv(formated_occ, paste0(folder_sp, "/occurrences/formated_occ.csv"), row.names = F)
      paste0(
        "Handling occurrences", "\n",
        "Number of not duplicated occurrences: ", nrow(formated_occ), "\n"
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
  # 2. Remove spatial duplicate occurrences to x km
  #--------------------------------------
  
  message(paste0("Removing spatial duplicate occurrences database to ", remove_distance, "km, using ", remove_method))

  linesmsg2 <- tryCatch(
    expr = {
      removed_spat_occ <- remove_spat_duplicates(
        occ. = formated_occ, col.lon = col_lon, col.lat = col_lat, sp.col = col_sp,
        sp.name = sp_name, remove.method = remove_method, remove.distance = remove_distance
      )
      write.csv(removed_spat_occ, paste0(folder_sp, "/occurrences/removed_spat_occ.csv"), row.names = F)
      paste0(
        "Unique occurrences to ", remove_distance, "km, using  ", remove_method, "\n",
        "Unique occurrences: ok.", "\n",
        "Number of unique occurrences: ", nrow(removed_spat_occ), "\n"
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
      if (nrow(removed_spat_occ) <= 2) {
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
  message("Constructing interest areas")

  linesmsg3 <- tryCatch(
    expr = {
      interest_areas <- define_interest_areas(
        occ. = removed_spat_occ,  col.lon = col_lon, col.lat = col_lat, folder.sp = folder_sp, dist.Mov = dist_MOV,
        method.M = method_M, method.G = method_G, method.F = method_F, area.M = area_M,
        area.G = area_G, area.F = area_F, proj.models = proj_models,
        do.future = do_future, compute.F = compute_F, polygon.data = polygon_data
      )
      write.csv(interest_areas$occurrences, paste0(folder_sp, "/occurrences/jointID_occ.csv"), row.names = F)
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

  linesmsg4 <- tryCatch( # missing use a vector of names instead of numbers to select what layers to copy
    expr = {
      envars <- process_env_current(
        clim.dataset = clim_vars, clim.dir = dir_clim, file.extension = file_extension,
        compute.G = compute_G, compute.F = compute_F, dir.G = dir_G, dir.F = dir_F, 
        cor.eval = cor_eval, cor.method = cor_method, cor.detail = cor_detail,
        crs.proyect = crs_proyect, shape.M = interest_areas$shape_M, shape.G = interest_areas$shape_G, 
        shape.F = interest_areas$shape_F, env.other = dir_other, folder.sp = folder_sp, do.future = do_future, 
        proj.models = proj_models
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
        BiasSp <- estimate_sampling_bias(data. = interest_areas$occurrences, TGS.kernel = TGS_kernel,
                                         shape.M = interest_areas$shape_M, env.M = envars$M, 
                                         folder.sp = folder_sp, col.lon = col_lon, col.lat = col_lat,
                                         col.sp = col_sp)
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
  
  if (nrow(interest_areas$occurrences) < 6 ) {
    
      message("Calibrating and evaluating bioclim models: less than 6 occurrences")
      
      linesmsg6.1 <- tryCatch(
        expr = {
          calibrate_model <- do.bioclim(occ. = interest_areas$occurrences, env.Mdir = paste0(folder_sp, "/M_variables"),
                                    env.Gdir = paste0(folder_sp, "/G_variables"),
                                    folder.sp = folder_sp, col.lon = col_lon, col.lat = col_lat, 
                                    proj.models = proj_models)
          paste("\nPath, number occ less than 6\nVerySmall samples bioclim modelling: ok.")
        },
        error = function(error_message) {
          e1 <- conditionMessage(error_message)
          return(paste0("\nPath, number occ less than 6 \nVerySmall samples bioclim modelling fail.\nError R: ", e1))
        } 
      )
      
      writeLines(linestime, filelog)
      
      writeLines(text = linesmsg6.1, con = filelog, sep = "\n")
      linestime <- give.msg.time(time.1 = time1)
  }
  
  #pckg <- match.arg(use., c("Dismo", "kuenm", "ENMeval2", "SDMTune", "Biomod"))

  if (nrow(interest_areas$occurrences) >= 6 & nrow(interest_areas$occurrences) < 20) {
    
      ##########
      # Path A # Jackknife, enmeval maxent
      ##########

      message("Calibrating and evaluating Maxent models: less than 20 occurrences")

      linesmsg6.1 <- tryCatch(
        expr = {
          calibrate_model <- do_enmeval(
            occ. = interest_areas$occurrences, bias.file = BiasSp, beta.mult = beta_small_sample, f.clas = fc_small_sample,
            env.Mdir = paste0(folder_sp, "/M_variables"), env.Gdir = paste0(folder_sp, "/G_variables"),
            env.Fdir = paste0(folder_sp, "/G_variables"), do.future = do_future, folder.sp = folder_sp,
            col.lon = col_lon, col.lat = col_lat, proj.models = proj_models, partitionMethod = "checkerboard1", #"jackknife",
            use.bias = use_bias, crs.proyect = crs_proyect, extrap = extrapo,
            sp.name = sp_name, redo. = redo, redo.path = redo_path, E = E, outf = outformat,
            Max.Bg = Max_Bg, sel. = selection, algo.enmeval = algo_enmeval
          )
          paste("\nPath Maxent, number occ less than 20\nSmall samples Maxent modelling: ok.")
        },
        error = function(error_message) {
          e1 <- conditionMessage(error_message)
          return(paste0("\nPath Maxent, number occ less than 20 \nSmall samples Maxent modelling fail.\nError R: ", e1))
        } 
      )

      writeLines(text = linesmsg6.1, con = filelog, sep = "\n")
      linestime <- give.msg.time(time.1 = time1)
      writeLines(linestime, filelog)
  }

  if (nrow(interest_areas$occurrences) >= 20) {
    ##########
    # Path B # split in test and train, kuenm maxent, biomod GBM y ANN
    ##########
    
    message("Calibrating and evaluating Maxent models: equal or more than 20 occurrences")
    
    if(maxent_package == "enmeval"){
      linesmsg6.1 <- tryCatch(
        expr = {
          calibrate_model <- do_enmeval(
            occ. = interest_areas$occurrences, bias.file = BiasSp, beta.mult = beta_large_sample, 
            f.clas = fc_large_sample,
            env.Mdir = paste0(folder_sp, "/M_variables"), env.Gdir = paste0(folder_sp, "/G_variables"),
            env.Fdir = paste0(folder_sp, "/G_variables"), do.future = do_future, folder.sp = folder_sp,
            col.lon = col_lon, col.lat = col_lat, proj.models = proj_models, partitionMethod = "block",
            use.bias = use_bias, crs.proyect = crs_proyect, extrap = extrapo,
            sp.name = sp_name, redo. = redo, redo.path = redo_path, E = E, outf = outformat,
            Max.Bg = Max_Bg, sel. = selection, algo.enmeval = algo_enmeval, sbg.file = sbg_file
          )
          paste("\nPath Maxent, number occ greater than 20\nLarge sample Maxent modelling: ok.")
        },
        error = function(error_message) {
          e1 <- conditionMessage(error_message)
          return(paste0("\nPath Maxent, number occ greater or equal to 20 \nLarge samples Maxent modelling fail.\nError R: ", e1))
        } 
      )
      
      writeLines(linestime, filelog)
      
      writeLines(text = linesmsg6.1, con = filelog, sep = "\n")
      linestime <- give.msg.time(time.1 = time1)
      writeLines(linestime, filelog)
      
    }
    
    
    if(maxent_package == "kuenm"){
      
      linesmsg6.1 <- tryCatch(
        expr = {
          PathBOcc <- make_split_occ(
            occ. = interest_areas$occurrences, bias.file = BiasSp, folder.sp = folder_sp, col.lon = col_lon,
            col.lat = col_lat, use.bias = use_bias, env.M = envars$M, sbg.file = sbg_file, Max.Bg = Max_Bg
          )
          paste0("\nPath Maxent, number occ greater than 20\nOcc splited: ok (kuenm).")
        },
        error = function(error_message) {
          e1 <- conditionMessage(error_message)
          return(paste0("\nPath Maxent, number occ greater than 20\nOcc splited: fail (kuenm).\nError R: ", e1))
        }
      )
      
      writeLines(text = linesmsg6.1, con = filelog, sep = "\n")
      
      linesmsg6.2 <- tryCatch(
          expr = {
            calibrate_model <- do_kuenm(
              occ. = PathBOcc, sp.name = sp_name, folder.sp = folder_sp,
              biasfile = "BiasfileM.asc", beta.mult = beta_large_sample, fc.clas = fc_large_sample, 
              kept. = kept, maxent.path = getwd(), proj.models = proj_models, E = E,
              do.future = do_future, env.Mdir = paste0(folder_sp, "/M_variables"),
              env.Gdir = paste0(folder_sp, "/G_variables"),
              crs.proyect = crs_proyect, use.bias = use_bias, extrap = extrapo,
              write.intfiles = FALSE, redo. = redo, redo.path = redo_path,
              Max.Bg = Max_Bg
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
    #       calibrate_model <- do.biomod(
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
    #             ras.Stack = projBalgo_i, data. = interest_areas$occurrences, collon = col_lon, collat = col_lat,
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
  # 7. Ensemble calibrated and evaluated models
  #--------------------------------------
  
  message("\nCalculating ensembles")
  
  linesmsg7 <- tryCatch(
    expr = {
      enscurr <- currentEns_byAlg(
        rasM.Stack = calibrate_model$M_proj, rasG.Stack = calibrate_model$G_proj, 
        data. = interest_areas$occurrences, collon = col_lon, collat = col_lat, e = E, 
        algorithm = calibrate_model$algorithm, foldersp = folder_sp, 
        tim = "current", esc.nm = "",
        crs.proyect = crs_proyect, transf.biomo.ext = transf_biomo_ext,
        areas = interest_areas, proj.models = proj_models, bins = NULL, 
        wr.Bin.Matrix = wr_Bin_Matrix
      )
      
      if (do_future == TRUE) {
        layersF <- futAuxiliar(fut.list.ras = calibrate_model$f_proj)
        
        for (f in 1:length(layersF)) {
          currentEns_byAlg(
            rasM.Stack = calibrate_model$M_proj, rasF.Stack = layersF[[f]], 
            algorithm = calibrate_model $algorithm,
            foldersp = folder_sp, tim = "future", esc.nm = names(layersF[f]),
            crs.proyect = crs_proyect, transf.biomo.ext = transf_biomo_ext,
            proj.models = proj_models, bins = enscurr, e = E
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
  writeLines(text = linesmsg7, con = filelog, sep = "\n")
  
  linestime <- give.msg.time(time.1 = time1)
  writeLines(linestime, filelog)

  #--------------------------------------
  # End
  #--------------------------------------

  if (erase_files == "none") {
    erase <- ""
  }

  if (erase_files == "essential" | erase_files == "all") {
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
    if (erase_files == "all") {
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

  writeLines(paste0("erase_files: ", erase_files, "\n", linestime), filelog)

  close(filelog)

  return(paste0("Executed ", sp_name))
}
