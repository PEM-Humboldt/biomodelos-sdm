# November 15 2022
# Carlos Jair Munoz Rodriguez
#
# This script checks if a process of modelling construction created with the modelling routines
# stored in "https://github.com/PEM-Humboldt/biomodelos-sdm/tree/master/Modelling" is completed.
# The script can catch from the log file which issues were presented.
# 
# Return: data.frame #----- (finalizar)

# Preamble: you need to store all the species folder run inside only one. For example: if
# you run 5 species move those folders inside only one and run the next script.

# names and folder path of species run
sps_nm <- list.dirs("JBM/", recursive = F, full.names = F) %>% gsub(pattern = "\\.", replacement = " ")
sps_folder <- list.dirs("JBM/", recursive = F, full.names = T)

# empty data.frame to fill

df <- data.frame("species" = as.character(), "eval"= as.character(), "final"= as.character(), 
                "ensembles"= as.character(), "time"= as.character(), 
                "Error"= as.character())

#--------------------------------------

for(i in 1:length(sps_folder)){
  require(stringr)

  #especie
  df[i, "species"] <- sps_nm[i]
  
  folders <- list.dirs(sps_folder[i], recursive = F, full.names = T)
  
  #generalizacion?
  gener <- list.dirs(folders, full.names = F, recursive = T) 
  if(length(which(str_detect(gener, "generalizacion") == TRUE)) > 0){
    df[i,"eval"] <- NA
    df[i,"final"] <- NA
    df[i,"ensembles"] <- "generalizacion"
    next()
  }
  
  # hizo evaluaciones?
  evalindex <- which(str_detect(folders, "eval_results") == TRUE)
  if(length(evalindex) > 0 ) df[i,"eval"] <- "x"
  
  # hizo modelos finales?
  finalindex <- which(str_detect(folders, "final_models") == TRUE)
  if(length(finalindex) > 0 ) df[i,"final"] <- "x" 
  
  # hizo ensambles de futuro?
  ensindex <- which(str_detect(folders, "ensembles") == TRUE)
  if(length(ensindex) >  0){
    ensfol <- folders[ensindex]
    ensfols <- list.dirs(ensfol, recursive = F, full.names = T)
    enscurindex <- which(str_detect(ensfols, "current") == TRUE)
    enscur <- ensfols[enscurindex]
    enscurs <- list.files(enscur, recursive = T, full.names = T)
    if(length(enscurs) > 5 ) df[i,"ensembles"] <- "x"
    
    #ensfutindex <- which(str_detect(ensfols, "future") == TRUE)
    #ensfut <- ensfols[ensfutindex]
    #ensfuts <- list.files(ensfut, recursive = T, full.names = T)
    #if(length(ensfuts) > 100 ) df[i,"ensembles"] <- "x" 
  }
  
  # si hubo errores, cual fue el primero y en que proceso?
  #x <- as.vector(read.delim(paste0(sps_folder[i], "/log_file.txt"))) %>% unlist()
  x <- read.delim(paste0(sps_folder[i], "/log_file.txt"))
  errindex <- which(str_detect(x[,1], "Error") == TRUE)
  if(length(errindex) >  0){
    err <- paste(x[(errindex[1]-1), 1],"\n", x[errindex[1], 1])
    df[i,"Error"] <- err
  }
  
  # cuanto se demoro?
  timeindex <- which(str_detect(x[,1], "mins") == TRUE)
  timedata <- x[timeindex[length(timeindex)], 1]
  timenums <- strsplit(timedata, "Completed in  ")
  df[i,"time"] <- timenums[[1]][2]
}

#escribir el data.frame
write.csv(df, file =  "revAuto_JBM.csv", row.names = F )
