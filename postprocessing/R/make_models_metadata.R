# Automated Metadata Generation for Species Modeling Results

# METADATA BETA VERSION 4
# October 2022 to XXX 2023

# Load Libraries
# Required libraries for executing the script.
library(raster)
library(dplyr)
library(hms)
library(xlsx)

# Get Prerequisites

# Load a reference map in case the model is not projected to the biomodelos extent.
biomodelos_template <- raster::raster("data/raster_template.tif")

# Load the metadata template
meta_template <- read.csv("data/metada_template.csv")

# Define the taxon for metadata generation
taxon <- "JBM"

# Define the directory containing the folders from the bio2_routine
dirmodels <- taxon

# Create a directory for storing metadata and moved model tifs
tmp <- paste0(taxon, "_2")
dir.create(tmp)
dirtowrite <- tmp

# Define the current date
dates <- Sys.Date() %>% as.character()

# Function to Automate Metadata Generation
auto_metadata <- function(dirmodels, dirtowrite, meta_template, algos, fut_proj = T, 
                          dates, transf_ext = FALSE, ext_template = NULL, 
                          crs_project = "+init=epsg:4326") {
  
  # each directory must represent a species
  sps <- list.dirs(paste0(dirmodels, "/"), full.names = F, recursive = F)
  
  # list to save template filled by tif mentioned above
  info <- list()
  
  # Iterate through species folders
  for (i in 1:length(sps)) {
    #i <- 4
    message(sps[i])
    
    dirs_inside <- list.dirs(paste0(dirmodels, "/", sps[i]), full.names = T, recursive = F)
    
    ensembles_dir <- dirs_inside[grep("ensembles", dirs_inside)]
    
    if (length(ensembles_dir) != 0) {
      
      for(a in 1:length(algos)){
        
        ensembles_inside <- list.dirs(ensembles_dir)
        
        find_alg1 <- ensembles_inside %>% grep(pattern = algos[a], x = .) 
        
        if(length(find_alg1) != 0){
          
          alg1 <- algos[a]
          path_alg <- ensembles_inside[find_alg1]
          break()
        }
      }
      
      pathforsp <- paste0(path_alg, "/")  
      
      path_tifs <- list.files(path = pathforsp, pattern = "*.tif$", full.names = T, 
                              recursive = F)
      path_tifs_short <- list.files(path = pathforsp, pattern = "*.tif$", full.names = F, 
                                    recursive = F)
      
      if (length(path_tifs) !=0) {
        
        # find records and count
        occ.forsp <- paste0(dirmodels, "/", sps[i], "/occurrences/occ_jointID.csv")
        rec_data <- read.csv(occ.forsp)
        records <- nrow(rec_data)
        
        # writing records data in dir.write folder
        write.csv(rec_data, paste0(dirtowrite, "/", sps[i], "_records.csv"), row.names = F)
        
        if(alg1 == "generalizacion"){
          
          # copy tif
          file.copy(
            from = path_tifs,
            to = paste0(dirtowrite, "/"),
            overwrite = T, recursive = T
          )
          records_model <- records
          thresholdValue <- NA
          thresholdType <- NA
          validationType <- NA
          perfStatType <- NA
          pvalue <- NA
          best1 <- NA
          bes2 <- NA
          consensus <- NA
          or_organize <- NA
          n <- 1
          
        }else{
          # Selecting strings of binaries 0,10,20,30 and continuos
          continuos <- sps[i] %>% gsub(x = ., pattern = "[.]", replacement = "_") %>% 
            paste0("*",., "_", alg1,".tif$") %>% grep(pattern = ., path_tifs)
          
          strings <- path_tifs[c(1, 2, 3, 4, continuos)]
          strings_short <- path_tifs_short[c(1, 2, 3, 4, continuos)]
          
          if (transf_ext) {
            "MISSING"
          } else {
            for (a in 1:length(strings)) {
              file.copy(
                from = strings[a],
                to = paste0(dirtowrite, "/"),
                overwrite = T, recursive = T
              )
            }
          }
          
          # binaries: 0, 10p, 20p, 30p
          # find thrsholds and get names
          bin.forsp <- paste0(dirmodels, "/", sps[i], "/ensembles/current/", alg1, "/binValues_", alg1, ".csv")
          bins <- read.csv(bin.forsp)
          records_model <- rep(records, each = 5)
          thresholdValue <- bins[1, -1]
          thresholdValue$cont <- NA
          thresholdType <- c("0", "10", "20", "30", "Continuous")
          
          if(alg1 == "MAXENT"){
            # performance evaluation
            if (records <= 25) {
              validationType <- "Jackknife"
              perfStatType <- "AUC_test"
              indexbest <- grep(pattern = "best_models.csv", list.files(paste0(dirmodels, "/", sps[i]), recursive = T))
              best <- read.csv(list.files(paste0(dirmodels, "/", sps[i]), recursive = T, full.names = T)[indexbest])
              
              pvalue <- NA
              best1 <- median(best$auc.train)
              bes2 <- sd(best$auc.train)
            } else {
              validationType <- "Crossvalidate-Block"
              perfStatType <- "pROC"
              dirsAll <- list.files(paste0(dirmodels, "/", sps[i]), recursive = T)
              indexbest <- grep(pattern = "best_", dirsAll)[1]
              best <- read.csv(list.files(paste0(dirmodels, "/", sps[i]), recursive = T, full.names = T)[indexbest])
              
              indexpckg <- grep(pattern = "enmeval", dirsAll)
              
              if(length(indexpckg) != 0){
                #enmeval
                best1 <- median(best$proc_auc_ratio.avg)
                bes2 <- sd(best$proc_auc_ratio.avg)
                pvalue <- mean(best$proc_pval.avg)
              }else{
                #kuenm
                best1 <- median(best$Mean_AUC_ratio)
                bes2 <- sd(best$Mean_AUC_ratio)
                pvalue <- mean(best$pval_pROC)
              }
              
            }
          }
          
          
          if(alg1 == "bioclim"){
            validationType <- "Crossvalidate-Block"
            perfStatType <- "AUC_train"
            indexbest <- grep(pattern = "best_models", list.files(paste0(dirmodels, "/", sps[i]), recursive = T))
            best <- read.csv(list.files(paste0(dirmodels, "/", sps[i]), recursive = T, full.names = T)[indexbest])
            best1 <- median(best$auc.mean)
            bes2 <- sd(best$auc.mean)
            pvalue <- mean(best$pbinomial)
          }
          
          
          if(is.na(bes2)){
            consensus <- NA
          }else{
            consensus <- "Median"
          }
          
          # omission
          rec_data <- rec_data[, c(3:4)]
          
          if(transf_ext == FALSE){
            ras <- raster::stack(strings)
            names(ras) <- gsub(pattern = ".tif", replacement = "", strings_short)
          }
          
          data_ras <- raster::extract(ras[[grep(pattern = "_0_|_10_|_20_|_30_", x = names(ras))]], rec_data)
          or <- 1 - (colSums(data_ras, na.rm = T) / records_model[1])
          or_organize <- c(or[1], or[2], or[3], or[4], NA)
          
          n <- length(thresholdType)
          
        }# maxent and bioclim and other algortithms
        
        # dates
        
        date_split <- strsplit(dates, "-") %>% unlist()
        
        # filling meta data template
        
        meta_template[1:n, "acceptedNameUsage"] <- sps[i] %>% gsub(x = ., pattern = "[.]", replacement = " ")
        meta_template[1:n, "modelingMethod"] <- alg1
        meta_template[1:n, "thresholdType"] <- thresholdType
        meta_template[1:n, "validationType"] <- validationType
        meta_template[1:n, "perfStatType"] <- perfStatType
        meta_template[1:n, "perfStatValue"] <- best1
        meta_template[1:n, "perfStatSD"] <- bes2
        meta_template[1:n, "pValue"] <- pvalue
        meta_template[1:n, "consensusMethod"] <- consensus
        meta_template[1:n, "thresholdValue"] <- t(thresholdValue)
        meta_template[1:n, "omission"] <- or_organize
        meta_template[1:n, "recsUsed"] <- records_model
        meta_template[1:n, "modelLevel"] <- 1
        meta_template[1:n, "isActive"] <- "true"
        meta_template[1:n, "modelStatus"] <- "Developing"
        meta_template[1:n, "modelAuthors"] <- "Instituto Humboldt"
        meta_template[1:n, "yyyy"] <- date_split[1]
        meta_template[1:n, "mm"] <- date_split[2]
        meta_template[1:n, "dd"] <- date_split[3]
        meta_template[1:n, "published"] <- "false"
        meta_template[1:n, "license"] <- "by-nc-sa"
        meta_template[1:n, "modelSeason"] <- "resident"
        meta_template[1:n, "modelOrigin"] <- "native"
        meta_template[1:n, "modelGeoExtent"] <- "national"
        meta_template[1:n, "modelEpoch"] <- "present" #MISSING COMPLETELY MISSED
        
        info[[i]] <- meta_template
      }
    }
  }
  info <- info[lengths(info) != 0]
  infoall <- do.call(rbind.data.frame, info)
}

# Generate metadata using the function
a <- auto_metadata(dirmodels = dirmodels, dirtowrite = dirtowrite,
                   meta_template = meta_template, algos = c("MAXENT", "bioclim", 
                                                            "generalizacion"), 
                   fut_proj = FALSE, dates = dates, 
                   transf_ext = FALSE, ext_template = NULL, crs_project = NULL)

# Write metadata to an xlsx file
# write.xlsx(a, file = paste0(dirtowrite, "/_metadata_", dirmodels, "_", dates, ".xlsx"),
#            sheetName= paste0("_metadata_", dirmodels, "_", dates), append= FALSE,
#            showNA = FALSE, row.names = FALSE)