# March 2nd, 2022

# Libraries
library(stringr)
library(dplyr)
library(data.table)

# mov_thresholds: Function to move threshold layers

# species, character: name of the species to extract its threshold layers
# from, character: directory path where result files of the modeling process are located
# to, character: directory path to move threshold layers and records used for model calibration and testing
# time, character: time of the models, either "current" or "future"
# model, character: algorithm from which prediction layers are to be extracted
# move_occs, logical: whether to move occurrence files along with thresholds

mov_thresholds <- function(species, from, bin_threshold, to, time, model = "MAXENT",
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
      
      # searching models
      index_ensembles <- grep(pattern = "ensembles", x = dirs_in)
      dir_ensamble <- dirs_in[index_ensembles]
      
      dirs_ens <- list.dirs(dir_ensamble)
      dir_time_model <- grep(pattern = paste0(time, "/", model), x = dirs_ens)
      
      models_file <- list.files(dirs_ens[dir_time_model], full.names = TRUE)
      
      bin_models <- models_file[grep(pattern = paste0(bin_threshold, "_", model), x = models_file )]
      
      index_occs <- grep(pattern = "occurrences", x = dirs_in)
      
      # searching occurrences
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

# Directories and Species Run

target <- "bees_flowers/"
spsSmall <- list.dirs(file.path(getwd(), target), recursive = FALSE, full.names = FALSE) %>% 
  gsub(pattern = "\\.", replacement = "_")
sps <- list.dirs(file.path(getwd(), target), recursive = FALSE, full.names = TRUE)

# Exercise 1: Move thresholds of all species run

data_moved <- list() 
for(i in 1:length(spsSmall)){
  data_moved[[i]] <- mov_thresholds(species = spsSmall[i], from = sps[i], bin_threshold = "_10",
                                    time = "current", to = file.path(getwd(), "thresholds_10"),
                                    move_occs = FALSE
  )
}
df_moved <- do.call(rbind, data_moved)

write.csv(df_moved, "DM_13122021_mix_threshold_20.csv", row.names = FALSE)

# Exercise 2: Move thresholds of specific species run

## List of species to extract
invasive_species <- fread("species_layers/invasives_01032022/Exotic_List_Final.csv") %>% 
  as.data.frame() %>% dplyr::select(., acceptedNameUsage) %>% unlist() %>% 
  gsub(pattern = " ", replacement = "_")

index <- which(spsSmall %in% invasive_species == TRUE)

sps <- sps[index]
spsSmall <- spsSmall[index]

thresholds <- c("_0", "10", "20", "30")

data_moved <- list() 
for(i in 1:length(spsSmall)){
  for(a in 1:length(thresholds)){
    data_moved[[i]] <- mov_thresholds(species = spsSmall[i], from = sps[i], 
                                      bin_threshold = thresholds[a], time = "future", 
                                      to = file.path(getwd(), "species_layers/invasives_01032022/"),
                                      move_occs = TRUE)  
  }
}
df_moved <- do.call(rbind, data_moved)