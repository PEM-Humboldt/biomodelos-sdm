#' Constructs a Geoserver-ready data frame from metadata and raster files.
#'
#' This function takes as input the path to a metadata file in Excel (.xlsx) or CSV (.csv) format,
#' the path to the folder containing raster files in ".tif" format, and the location where the resulting
#' data frame in CSV format should be saved. It organizes the information and creates a CSV file with
#' the data frame for use on geospatial platforms such as Geoserver.
#'
#' @param path.file.metadata Path to the metadata file in Excel (.xlsx) or CSV (.csv) format.
#' @param path.folder.raster Path to the folder containing raster files in ".tif" format.
#' @param path.folder.save Path where the resulting CSV file will be saved.
#'
#' @import data.table
#' @import openxlsx
#'
#' @return A CSV file with organized information for Geoserver.
#'
#' @examples
#' construct_df_geoserver("metadata.xlsx", "raster_folder/", "output/")
#'

construct_df_geoserver <- function(path.file.metadata, path.folder.raster, path.folder.save){
  
  # Determine the format of the metadata file
  finalstr <- tail(unlist(strsplit(path.file.metadata, "\\.")), n = 1)
  
  # Read metadata
  if (finalstr == "xlsx"|finalstr == "xls") {
    require(openxlsx)
    metadata <- openxlsx::read.xlsx(path.file.metadata, sheet = 1)
  } else {
    require(data.table)
    metadata <- data.table::fread(path.file.metadata) |> 
      as.data.frame()
  }
  
  # Get the list of raster files
  rasters <- list.files(path.folder.raster, pattern = ".tif$", full.names = F, recursive = F) |>
    sort()
  
  # Sort metadata
  index_order <- metadata$acceptedNameUsage |> order()
  
  tax_id <- metadata$taxID[index_order]
  model_id <- metadata$modelID[index_order]
  
  # Create a Geoserver-ready data frame
  df_geoserver <- data.frame("tax_id" = tax_id, "model_id" = model_id, "model_file" = rasters)
  
  # Generate a date to include in the resulting file name
  date <- gsub("-", "", Sys.Date())
  
  # Write the data frame to a CSV file
  write.csv(df_geoserver, paste0(path.folder.save, "df_geoserver_", date, ".csv"), row.names = F)  
  
}

construct_df_geoserver(path.file.metadata = "A", path.folder.raster = "B", path.folder.save = "C")
# Example1:
# construct_df_geoserver(path.file.metadata = ".../peces_invemar_modelos/metadata_Modelos_20231206.xlsx",
#                        path.folder.raster = ".../peces_invemar_modelos/",
#                        path.folder.save = ".../peces_invemar_modelos/")

# Example 2:
construct_df_geoserver(path.file.metadata = "D:/humboldt/bm_ediciones_upload/actualizacion_carnivoros/carnivora_metadata_2024-06-20_geoserver.csv", 
                      path.folder.raster = "D:/humboldt/bm_ediciones_upload/actualizacion_carnivoros/flujo_geoserver/", 
                     path.folder.save = "D:/humboldt/bm_ediciones_upload/actualizacion_carnivoros/flujo_geoserver/")
