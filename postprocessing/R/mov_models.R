# creado
# Segundo semestre de 2021
# actulización de codigo
# 2 de marzo de 2022

# Librerias

library(stringr)
library(dplyr)
library(data.table)

# mov_umbrales: Funcion para mover las capas de umbrales

# species, character: nombre de la especie a extraer sus umbrales
# from, character: dirección del directorio en donde se encuentran los archivos resultados del 
# proceso de modeloado
# to, character: direccion del directorio a donde se van a mover las capas de umbrales y registros
# usados para la calibración y prueba de los modelos
# time, character: umbrales de modelos en el presente "current" o el futuro "future" 
# model, character: algoritmo del cual se quiere extraer las capas de prediccion

mov_umbrales <- function(species, from, bin_threshold, to, time, model = "MAXENT",
                             move_occs = T){
  
  if(file.exists(paste0(from, "/log_file.txt"))){
    x <- as.vector(read.delim(paste0(from, "/log_file.txt"))) %>% unlist()
    errindex <- which(str_detect(x, "Error") == TRUE)
    if(length(errindex) >  0){
      err <- TRUE
      moved <- FALSE
    }else{
      err <- FALSE
      
      dirs_in <- list.dirs(from, full.names = T, recursive = F)
      
      # searching models
      index_ensembles <- grep(pattern = "ensembles", x = dirs_in)
      dir_ensamble <- dirs_in[index_ensembles]
      
      dirs_ens <- list.dirs(dir_ensamble)
      dir_time_model <- grep(pattern = paste0(time, "/", model), x = dirs_ens)
      
      models_file <- list.files(dirs_ens[dir_time_model], full.names = T)
      
      bin_models <- models_file[grep(pattern = paste0(bin_threshold, "_", model), x = models_file )]
      
      index_occs <- grep(pattern = "occurrences", x = dirs_in)
      # searching occs
      
      if(length(index_occs) > 0){
        
      
      if(time != "current"){
        to <- file.path(to, species)
        dir.create(to, showWarnings = F)
      } 
      
      dir.create(to, showWarnings = F, recursive = T)
      file.copy(from = bin_models, to = paste0(to,"/"), overwrite = T, recursive = F)
      
      if(move_occs == T){
        dir_occurrences <- dirs_in[index_occs]
        
        occs_files <- list.files(dir_occurrences, full.names = T)
        
        occ_joint_file <- occs_files[grep("jointID", occs_files)]
        
        occ_joint <- read.csv(occ_joint_file)
        
        write.csv(occ_joint, paste0(to, "/", species, "_occ_jointID.csv"), row.names = F)  
      }
      
        
    }
      moved = TRUE
    }
    
    data.move <- data.frame("species" = species, "Error"= err, "moved" = moved)
    
    return(data.move)
  }
  message(paste0(species, " log file was not found"))
}

# directorios y especies corridas

target <- "abejas_vflorales/"
spsSmall <- list.dirs(file.path(getwd(), target), recursive = F, full.names = F) %>% 
  gsub(pattern = "\\.", replacement = "_")
sps <- list.dirs(file.path(getwd(), target), recursive = F, full.names = T)

# Ejercicio 1: mover los umbrales de todas las especies corridas

data_moved <- list() 
for(i in 1:length(spsSmall)){
  data_moved[[i]] <- mov_umbrales(species = spsSmall[i], from = sps[i], bin_threshold = "_10",
                                      time = "current", to = file.path(getwd(), "umbrales_10"),
                                      move_occs = F
                                      )
}
df_moved <- do.call(rbind, data_moved)

write.csv(df_moved, "DM_13122021_mix_umbral_20.csv", row.names = F)


# Ejercicio 2: mover los umbrales de algunas especies corridas
## lista de especies a extraer
spp_invasoras <- fread("capasx_especies/invasoras_01032022/Lista_exoticas_final.csv") %>% 
  as.data.frame() %>% dplyr::select(., acceptedNameUsage) %>% unlist() %>% 
  gsub(pattern = " ", replacement = "_")

index <- which(spsSmall %in% spp_invasoras == T)

sps <- sps[index]
spsSmall <- spsSmall[index]

umbrales <- c("_0", "10", "20", "30")

data_moved <- list() 
for(i in 1:length(spsSmall)){
  for(a in 1:length(umbrales)){
    data_moved[[i]] <- mov_umbrales(species = spsSmall[i], from = sps[i], 
                                          bin_threshold = umbrales[a], time = "future", 
                                          to = file.path(getwd(), "capasx_especies/invasoras_01032022/"),
                                          move_occs = T)  
  }
  
}
df_moved <- do.call(rbind, data_moved)


#----------------------------------------
dirs <- list.files("coleopteros_vflorales_extBioModelos/", ".tif", full.names = T, recursive = F)

dirs <- dirs[grepl("_10_", dirs)]

for (a in 1:length(dirs)) {
  file.copy(
    from = dirs[a],
    to = paste0("upload_05052023/"),
    overwrite = T, recursive = T
  )
}

