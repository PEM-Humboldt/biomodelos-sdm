# Bio2_routine
## Going deep inside of BioModelos

### Description

`Bio2_routine` is a wrapper function that allows an automatized and flexible SDM general routine. that automates the fitting of Species Distribution Models from occurrence and environmental data.

### Usage

```
Bio2_routine <- function(occ, col_sp = NULL, col_lat = NULL, col_lon = NULL, do_clean = NULL, drop_out = NULL, 
IQR_mtpl = NULL, clim_vars, dir_clim = NULL, dir_other = NULL, extension_vars = NULL, uniq1k_method = NULL, 
dist_uniq = NULL, use_bias = NULL, TGS_kernel = NULL, method_M = NULL, dist_MOV = NULL, proj_models, 
method_G = NULL, area_G = NULL, compute_G = NULL, dir_G = NULL, do_future = NULL, method_F = NULL, area_F = NULL, 
compute_F = NULL, dir_F = NULL, polygon_data = NULL, raster_data = NULL, freq_percent = NULL, algos = NULL, 
beta_5.25 = NULL, fc_5.25 = NULL, beta_25 = NULL, fc_25 = NULL, E = NULL, extrapo = NULL, predic = NULL, 
crs_proyect = NULL, tipo = NULL, kept = NULL, keep_files = NULL, transf_biomo_ext = NULL 
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
* **do_clean** logical: occurrence data cleaning. If FALSE only searching and removing for strange characters ("@[>!¿<#?&/\\]") and duplicated data is performed on latitude and longitude columns from occurrence data frame. If TRUE, aditionally records falling in capitals, gbif headquarters or research institutions coordinates, regional or national centroids, having equal latitude and longitude or zero-zero at both, as well as seas data are removed. Here is used the function [clean_coordinates](https://github.com/ropensci/CoordinateCleaner/blob/master/R/clean_coordinates.R) from [CoordinateCleaner](https://cran.r-project.org/web/packages/CoordinateCleaner/index.html) Default: FALSE.
* **drop_out** character: method for dropping geographical outliers out from occurrence database. Valid strings are "any", "IQR", ""freq". For no action, "any" string. The "IQR" method stands by outlier detection based on an interquantile range test using the function [cc_outl](https://github.com/ropensci/CoordinateCleaner/blob/master/R/cc_outl.R) from [CoordinateCleaner](https://cran.r-project.org/web/packages/CoordinateCleaner/index.html). The "freq" method removes occurrence records from bio-geographical polygons under represented in the distribution of the species. It calculates the frequency of registers in each region extracting the region information by each record. Next, the regions with a relative frequency below an user threshold percentage are removed. The method uses a rasterized layer created from a polygon file and geographical records. Default: "any".
* **IQR_mtpl** numeric: value of the interquantile range to use with "IQR" dropping outlier method. Default: NULL.
* **freq_percent** numeric: value of relative frequency threshold to use with "freq" dropping outlier method. Default: NULL.

#### Environmental variables

Environmental variables characterizes the environment asociated to distribution records. This description can be made because those variables are stored as raster images. Those raster are geo-referenced matrix structures containing information by each cell. The information can be extracted using Geographical Information Tools (Petersonet al., 2011).

* **clim_vars** character: filename of climatic data set to use. It is useful when you want to compare fit of different climatic data sets. No Default assigned. More information in the [Bio2routine readme](https://github.com/cmunozr/16kproject_IAVH)
* **dir_clim** character: path in where is stored the climatic data set specified. Default: "Data/env_vars/".
* **dir_other**  character: path in where is stored the other environmental variables. Default: "Data/env_vars/other/".
* **extension_vars** character: regular expression to find the environmental raster layers to load. Supported file types are the 'native' raster
package format and those that can be read via rgdal (see [raster formats](https://www.rdocumentation.org/packages/raster/versions/3.4-10/topics/writeFormats)) Default: "*.tif$". 

#### Bias management

Spatial bias usually leads to environmental bias because of the over-representation of certain environmental features of the more accessible and extensively surveyed areas. Sampling bias can be addressed by: spatial thinning or the inclusion of so-called bias files. Spatial thinning of species occurrence records can help address problems associated with spatial sampling biases. Ideally, thinning removes the fewest records necessary to substantially reduce the effects of sampling bias, while simultaneously retaining the greatest amount of useful information. Bias files allow the user to choose background data with the same bias as occurrence data.

* **uniq1k_method** character: two methods are available: "sqkm" and "spthin". The former divides the geographical extent in squares of an user select distance and let one (1) occurrence record by each of those squares, it uses the function [clean_dup](https://github.com/luismurao/nichetoolbox/blob/master/R/clean_dup.R) from the package [nichetoolbox](https://www.google.com/search?q=nichetoolbox&rlz=1C1CHBF_esCO935CO935&oq=nichetoolbox&aqs=chrome..69i57j0i19i30j69i60l2j69i61.2307j1j4&sourceid=chrome&ie=UTF-8). The latter uses a randomization algorithm to create a data set in which each record is at least an user select distance apart, it uses the function [thin](https://github.com/cran/spThin/blob/master/R/thin.R) from the [spThin](https://cran.r-project.org/web/packages/spThin/spThin.pdf) package. Default: "sqkm".
* **dist_uniq** numeric: distance in kilometers to be used as threshold in the spatial thinning process.
* **use_bias** logical: correcting bias using a bias layer enable or disabled. Default: FALSE.

* **TGS_kernel** character: path to pre-processed bias layer file. Target Group Sampling (TGS; Ponder etal. 2001, Phillips etal. 2009). TGS uses the presence locations of taxonomically related species observed using the same techniques as the focal species (usually from the same database) to estimate sampling, under the assumption that those surveys would have recorded the focal species had it occurred there (Phillips etal. 2009). For a begginer guide see the [Scott Rinann's blog](https://scottrinnan.wordpress.com/2015/08/31/how-to-construct-a-bias-file-with-r-for-use-in-maxent-modeling/) 

#### Interest areas

Methods to construct geographical areas to calibrate and project the models. Three areas are defined: M stands for calibration area, G and F where is projected the model as it would be needed. Each of those areas have spatial methods to be constructed.


* **method_M** character:
* **dist_MOV** numeric: distance in kilometers of 
* **proj_models** character
* **method_G** character
* **area_G** character
* **compute_G** logical
* **dir_G** character
* **do_future** logical
* **method_F** character
* **area_F** character
* **compute_F** logical
* **dir_F** character
* **polygon_data** character, path to spatial data to construct interest areas .
* **raster_data** character, path

#### Algorithms

* **algos**
* **beta_5.25**
* **fc_5.25**
* **beta_25**
* **fc_25**
* **E**
* **extrapo**
* **predic**
* **mxnt.pckg = NULL** Future develop
* **other.pckg = NULL** Future develop

#### Miscellaneous

* **crs_proyect** character: final output spatial reference. Default: "+proj=longlat +datum=WGS84 +no_defs +type=crs"
* **tipo** character: adds a suffix to the species main folder in, useful in experimental or comparative settings. Default: ""
* **kept** logical: removes the candidate models folder and files created by the package kuenm. Default: TRUE.
* **keep_files** character: certain files from the species main folder are kept in order to reduce disk usage. Three methods allowed. "all" keeps every file and folder creates in the process of setting SDM. "essential" keeps evaluation, final, ensembles, processed occurrences and geographical areas constructed folders and files. "none" keeps only ensembles, processed occurrences and geographical areas constructed.     
* **transf_biomo_ext** logical: if TRUE extends the model to the BioModelos extension with NA data. It is usful to make operations between SDMs. Default: TRUE.