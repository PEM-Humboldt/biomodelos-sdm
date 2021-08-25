# Bio2_routine
## Going deep inside BioModelos 2

### Description

`Bio2_routine` automates the fitting of Species Distribution Models from occurrence and environmental data.

### Details

This is a function to automate the fitting of Species Distribution Models (SDM) from databases gathered and managed by the Instituto de Investigación de Recursos Biológicos Alexander von Humboldt (IAvH). It is attached to BioModelos initiative from the IAvH. "BioModelos 2" follows an automatized SDM general routine. First, it cleans occurrence data and constructs the accessible area when is necesary. Second, it crops and masks current and future environmental variables. Third, it creates SDM's using one or several algorithms and then ensemble the best of each one. Fourth, it projects to different scenarios at user discretion.

### Usage

```
Bio2_routine <- function(occ, col_sp = NULL, col_lat = NULL, col_lon = NULL, do_clean = NULL, drop_out = NULL, 
IQR_mtpl = NULL, clim_vars, dir_clim = NULL, dir_other = NULL, extension_vars = NULL, uniq1k_method = NULL, 
dist_uniq = NULL, use_bias = NULL, TGS_kernel = NULL, method_M = NULL, dist_MOV = NULL, proj_models, 
method_G = NULL, area_G = NULL, compute_G = NULL, dir_G = NULL, do_future = NULL, method_F = NULL, area_F = NULL, 
compute_F = NULL, dir_F = NULL, polygon_data = NULL, raster_data = NULL, freq_percent = NULL, algos = NULL, 
beta_5.25 = NULL, fc_5.25 = NULL, beta_25 = NULL, fc_25 = NULL, E = NULL, extrapo = NULL, predic = NULL, 
crs_proyect = NULL, 
tipo = NULL, kept = NULL, keep_files = NULL, write_intfiles = NULL, transf_biomo_ext = NULL 
# mxnt.pckg = NULL, other.pckg = NULL Future develop
)
```
### Arguments

Arguments will be presented in groups.

#### Managing species occurrences

* **occ** data frame: occurrence data base of a single species. As a minimum, the data base must have species name, latitude, longitude, and date columns. It does not matter the order of the database or if there are companion columns. No Default defined.
Example:

|acceptedNameUsage       | ...   |decimalLatitude|decimalLongitude      | ...   |
|-                       | -     |-              |-                     |-      |
|Anisognathus melanogenys| ...   | 11.1085       | -74.0612             | ...   |
|Anisognathus melanogenys| ...   | 11.1041       | -74.0695             | ...   |
|Anisognathus melanogenys| ...   | 11.1113       | -74.0549             | ...   |
|Anisognathus melanogenys| ...   | 11.1096       | -74.0449             | ...   |
|Anisognathus melanogenys| ...   | 11.1073       | -74.0489             | ...   |
|Anisognathus melanogenys| ...   | 11.1024       | -74.0616             | ...   |
| ...                    | ...   | ...           | ...                  | ...   |

* **col_sp** character: containing the species name column. Default: "acceptedNameUsage". 
* **col_lat** character: containing the latitude coordinate name column. Default: "decimalLatitude".
* **col_lon** character: containing the longitude coordinate name column. Default: "decimalLongitude".
* **do_clean** logical: occurrence data cleaning. If FALSE only a searching and removing for strange characters ("@[>!¿<#?&/\\]") and duplicated data is performed on latitude and longitude columns. If TRUE, apart from the last, records falling in capitals, gbif headquarters or research institutions coordinates, regional or national centroids, having equal latitude and longitude or zero-zero at both, as well as seas data. Here is uded the function [clean_coordinates](https://github.com/ropensci/CoordinateCleaner/blob/master/R/clean_coordinates.R) from [CoordinateCleaner](https://cran.r-project.org/web/packages/CoordinateCleaner/index.html) Default: FALSE.
* **drop_out** character: method for dropping geographical outliers out from occurrence database. Valid strings are "any", "IQR", ""freq". For no action, "any" string. The "IQR" method stands by outlier detection based on an interquantile range test using the function [cc_outl](https://github.com/ropensci/CoordinateCleaner/blob/master/R/cc_outl.R) from [CoordinateCleaner](https://cran.r-project.org/web/packages/CoordinateCleaner/index.html). The "freq" method removes occurrence records from bio-geographical polygons under represented in the distribution of the species. It calculates the frequency of registers in each region extracting the region information by each record. Next, the regions with a relative frequency below an user threshold percentage are removed. The method uses a rasterized layer created from a polygon file and geographical records. Default: "any".
* **IQR_mtpl** numeric: value of the interquantile range to use with "IQR" dropping outlier method. Default: NULL.
* **freq_percent** numeric: value of relative frequency threshold to use with "freq" dropping outlier method. Default: NULL.

#### Environmental varialbles

* **clim_vars** character: filename of climatic data set to use. It is useful when you want to compare fit of different climatic data sets. No Default.
* **dir_clim** character: path in where is stored the climatic data set specified. Default: "Data/env_vars/".
* **dir_other**  character: path in where is stored the other environmental variables. Default: "Data/env_vars/other/".
* **extension_vars** character: regular expression to find the environmental raster layers to load. Supported file types are the 'native' raster
package format and those that can be read via rgdal (see [raster formats](https://www.rdocumentation.org/packages/raster/versions/3.4-10/topics/writeFormats)) Default: "*.tif$".

#### Bias management

* **uniq1k_method** character: Spatial thinning of species occurrence records can help address problems associated with spatial sampling biases. Ideally, thinning removes the fewest records necessary to substantially reduce the effects of sampling bias, while simultaneously retaining the greatest amount of useful information. Two methods are available: "sqkm" and "spthin". The former divides the geographical extent in squares of an user select distance and let one (1) occurrence record by each of those squares, it uses the function [clean_dup](https://github.com/luismurao/nichetoolbox/blob/master/R/clean_dup.R) from the package [nichetoolbox](https://www.google.com/search?q=nichetoolbox&rlz=1C1CHBF_esCO935CO935&oq=nichetoolbox&aqs=chrome..69i57j0i19i30j69i60l2j69i61.2307j1j4&sourceid=chrome&ie=UTF-8). The latter uses a randomization algorithm to create a data set in which each record is at least an user select distance apart, it uses the function [thin](https://github.com/cran/spThin/blob/master/R/thin.R) from the [spThin](https://cran.r-project.org/web/packages/spThin/spThin.pdf) package. Default: "sqkm".    
* **dist_uniq** 
* **use_bias**
* **TGS_kernel**

#### Accessible Area or M Area

* **MCP_buffer**
* **polygon_select**
* **points_Buffer**
* **dist_MOV**
* **polygon_M** Spatial data to construct M composed, must be inside project file
* **raster_M**

* **proj_models**
* **area_G**
* **compute_G**
* **dir_G**
* **do_future**
* **compute_F**
* **dir_F**

#### Algorithms

* **algos**
* **beta_5.25**
* **fc_5.25**
* **beta_25**
* **fc_25**
* **E**
* **extrapo**
* **predic**

#### Miscellaneous

* **crs_proyect**
* **tipo**
* **kept**
* **keep_files**
* **write_intfiles**
* **transf_biomo_ext**


#' @param occ 
#' @param col_sp 
#' @param col_lat 
#' @param col_lon 
#' @param do_clean 
#' @param drop_out 
#' @param IQR_mtpl 
#' @param clim_vars 
#' @param dir_clim 
#' @param dir_other 
#' @param extension_vars 
#' @param uniq1k_method 
#' @param dist_uniq 
#' @param use_bias 
#' @param TGS_kernel 
#' @param method_M 
#' @param dist_MOV 
#' @param proj_models 
#' @param method_G 
#' @param area_G 
#' @param compute_G 
#' @param dir_G 
#' @param do_future 
#' @param method_F 
#' @param area_F 
#' @param compute_F 
#' @param dir_F 
#' @param polygon_data 
#' @param raster_data 
#' @param freq_percent 
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