# Bio2_routine
## Going deep inside BioModelos 2

### Description

`Bio2_routine` automates the fitting of Species Distribution Models from occurrence and environmental data.

### Details

This is a function to automate the fitting of Species Distribution Models (SDM) from databases gathered and managed by the Instituto de Investigación de Recursos Biológicos Alexander von Humboldt (IAvH). It is attached to BioModelos initiative from the IAvH. "BioModelos 2" follows an automatized SDM general routine. First, it cleans occurrence data and constructs the accessible area when is necesary. Second, it crops and masks current and future environmental variables. Third, it creates SDM's using one or several algorithms and then ensemble the best of each one. Fourth, it projects to different scenarios at user discretion.

### Usage

```
Bio2_routine(

occ, col_sp = NULL, col_lat = NULL, col_lon = NULL, do_clean = NULL, drop_out = "any", IQR_mtpl = NULL, clim_vars, dir_clim = NULL, dir_other = NULL, extension_vars = NULL, uniq1k_method = NULL, dist_uniq = NULL, use_bias = NULL, TGS_kernel = NULL, MCP_buffer = NULL, polygon_select = NULL, points_Buffer = NULL, dist_MOV = NULL, polygon_M = NULL, raster_M = NULL, proj_models, area_G = NULL, compute_G = NULL, dir_G = NULL, do_future = NULL, compute_F = NULL, dir_F = NULL, algos = NULL, beta_5.25 = NULL, fc_5.25 = NULL, beta_25 = NULL, fc_25 = NULL, E = NULL, extrapo = NULL, predic = NULL, crs_proyect = NULL, tipo = NULL, kept = NULL, keep_files = NULL, write_intfiles = NULL, transf_biomo_ext = NULL

)
```
### Parameters

Parameters will be presented in groups.

#### Managing species occurrences

*occ* data frame: occurrence data base of a single species. As a minimum, the data base must have species name, latitude, longitude, and date columns
*col_sp* vector character: containing the species name column
*col_lat* vector character: containing the latitude coordinate name column
*col_lon* vector character: containing the longitude coordinate name column
*do_clean* logical:
*drop_out* vector character: method for dropping outliers out from occurrence database. Methods are "any", "IQR", ""freq".
*IQR_mtpl*

#### Environmental varialbles

*clim_vars* Which climatic data use, useful when you want to compare fit of different climatic data sets
#' @param dir_clim
#' @param dir_other
#' @param extension_vars
#' @param uniq1k_method "sqkm", "spthin"
#' @param dist_uniq
#' @param use_bias
#' @param TGS_kernel
#' @param MCP_buffer
#' @param polygon_select
#' @param points_Buffer
#' @param dist_MOV
#' @param polygon_M Spatial data to construct M composed, must be inside project file
#' @param raster_M
#' @param proj_models
#' @param area_G
#' @param compute_G
#' @param dir_G
#' @param do_future
#' @param compute_F
#' @param dir_F
#' @param algos
#' @param beta_5.25
#' @param fc_5.25
#' @param beta_25
#' @param fc_25
#' @param E
#' @param extrapo
#' @param predic
#' @param crs_proyect
#' @param tipo
#' @param kept
#' @param keep_files
#' @param write_intfiles
#' @param transf_biomo_ext
