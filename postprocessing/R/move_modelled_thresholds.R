#' Move Modelling Thresholds
#'
#' Function to move threshold layers and associated model records from one directory to another.
#'
#' This function moves model result files (threshold layers and optional occurrence records) from
#' one directory (`from`) to another (`to`). It searches for specific model outputs based on the `bin_threshold`
#' pattern and `model` algorithm within the `from` directory. If `move_occs` is TRUE, it also moves associated
#' occurrence files from the `from` directory.
#'
#' species, Character: vector specifying the species name.
#' from, Character: path to the directory containing the model result files.
#' bin_threshold, Character: specifying the bin threshold pattern to match in file names.
#' to, Character: path to the destination directory where files will be moved.
#' time, Character: specifying the time period ("current" or "future") of the models.
#' model, Character: specifying the model algorithm (default is "MAXENT").
#' move_occs, Logical: indicating whether to move occurrence files along with thresholds (default is TRUE).
#'
#' return A data frame indicating success or failure of moving the files for each species.

library(stringr)
library(dplyr)
library(data.table)

move_modelled_thresholds <- function(species, from, bin_threshold, to, time, model = "MAXENT",
                           move_occs = TRUE){
  
  if(file.exists(paste0(from, "/log_file.txt"))){
    x <- as.vector(read.delim(paste0(from, "/log_file.txt"))) %>% unlist()
    errindex <- which(str_detect(x, "Error") == TRUE)
    if(length(errindex) >  0){
      err <- TRUE
      moved <- FALSE
    } else {
      err <- FALSE
      
      dirs_in <- list.dirs(from, full.names = TRUE, recursive = FALSE)
      
      # Searching models
      index_ensembles <- grep(pattern = "ensembles", x = dirs_in)
      dir_ensamble <- dirs_in[index_ensembles]
      
      dirs_ens <- list.dirs(dir_ensamble)
      dir_time_model <- grep(pattern = paste0(time, "/", model), x = dirs_ens)
      
      models_file <- list.files(dirs_ens[dir_time_model], full.names = TRUE)
      
      bin_models <- models_file[grep(pattern = paste0(bin_threshold, "_", model), x = models_file )]
      
      index_occs <- grep(pattern = "occurrences", x = dirs_in)
      
      # Searching occurrences
      if(length(index_occs) > 0){
        if(time != "current"){
          to <- file.path(to, species)
          dir.create(to, showWarnings = FALSE)
        } 
        
        dir.create(to, showWarnings = FALSE, recursive = TRUE)
        file.copy(from = bin_models, to = paste0(to,"/"), overwrite = TRUE, recursive = FALSE)
        
        if(move_occs == TRUE){
          dir_occurrences <- dirs_in[index_occs]
          occs_files <- list.files(dir_occurrences, full.names = TRUE)
          
          occ_joint_file <- occs_files[grep("jointID", occs_files)]
          occ_joint <- read.csv(occ_joint_file)
          write.csv(occ_joint, paste0(to, "/", species, "_occ_jointID.csv"), row.names = FALSE)  
        }
      }
      moved = TRUE
    }
    
    data.move <- data.frame("species" = species, "Error"= err, "moved" = moved)
    return(data.move)
  }
  message(paste0(species, " log file was not found"))
}


# List of species to extract

target <- "folder_where_is_located_species_folders"
spsSmall <- list.dirs(file.path(getwd(), target), recursive = FALSE, full.names = FALSE) %>% 
  gsub(pattern = "\\.", replacement = "_")
sps <- list.dirs(file.path(getwd(), target), recursive = FALSE, full.names = TRUE)

# Exercise 1: Move only 10 percentile current threshold without moving occurrences

data_moved <- list() 
for(i in 1:length(spsSmall)){
  data_moved[[i]] <- move_modelling_thresholds(species = spsSmall[i], from = sps[i], bin_threshold = "_10",
                                    time = "current", to = file.path(getwd(), "thresholds_10"),
                                    move_occs = FALSE
  )
}
df_moved <- do.call(rbind, data_moved)

# Exercise 2: Move all future thresholds moving occurrences (note nested for loop)

thresholds <- c("_0", "10", "20", "30")

data_moved <- list() 
for(i in 1:length(spsSmall)){
  for(a in 1:length(thresholds)){
    data_moved[[i]] <- move_model_results(species = spsSmall[i], from = sps[i], 
                                      bin_threshold = thresholds[a], time = "future", 
                                      to = file.path(getwd()),
                                      move_occs = TRUE)  
  }
}
df_moved <- do.call(rbind, data_moved)