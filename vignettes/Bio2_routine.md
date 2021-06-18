# Bio2_routine
## Going deep inside BioModelos 2

### Description

`Bio2_routine` automates the fitting of Species Distribution Models from occurrence and environmental data.

### Details

This is a function to automate the fitting of Species Distribution Models (SDM) from databases gathered and managed by the Instituto de Investigación de Recursos Biológicos Alexander von Humboldt (IAvH). It is attached to BioModelos initiative from the IAvH. "BioModelos 2" follows an automatized SDM general routine. First, it cleans occurrence data and constructs the accessible area when is necesary. Second, it crops and masks current and future environmental variables. Third, it creates SDM's using one or several algorithms and then ensemble the best of each one. Fourth, it projects to different scenarios at user discretion.

### Usage

```
Bio2_routine(
  occ, col_sp = NULL, col_lat = NULL, col_lon = NULL, do_clean = NULL, drop_out = "any",
  IQR_mtpl = NULL, clim_vars, dir_clim = NULL, dir_other = NULL, extension_vars = NULL,
  uniq1k_method = NULL, dist_uniq = NULL, use_bias = NULL, TGS_kernel = NULL, 
  MCP_buffer = NULL, polygon_select = NULL, points_Buffer = NULL, dist_MOV = NULL, 
  polygon_M = NULL, raster_M = NULL, proj_models, area_G = NULL, compute_G = NULL, 
  dir_G = NULL, do_future = NULL, compute_F = NULL, dir_F = NULL, algos = NULL, 
  beta_5.25 = NULL, fc_5.25 = NULL, beta_25 = NULL, fc_25 = NULL, E = NULL, extrapo = NULL,
  predic = NULL, crs_proyect = NULL, tipo = NULL, kept = NULL, keep_files = NULL,
  write_intfiles = NULL, transf_biomo_ext = NULL
)
```
### Arguments

Arguments will be presented in groups.

#### Managing species occurrences

* **occ** data frame: occurrence data base of a single species. As a minimum, the data base must have species name, latitude, longitude, and date columns. Example:

|Species|     decimalLatitude|      decimalLongitude| ...|
|-|-|-|-
|Anisognathus melanogenys| 11.1085| -74.0612| ...|
|Anisognathus melanogenys| 11.1041| -74.0695| ...|
|Anisognathus melanogenys| 11.1113| -74.0549| ...|
|Anisognathus melanogenys| 11.1096| -74.0449| ...|
|Anisognathus melanogenys| 11.1073| -74.0489| ...|
|Anisognathus melanogenys| 11.1024| -74.0616| ...|

* **col_sp** vector character: containing the species name column
* **col_lat** vector character: containing the latitude coordinate name column
* **col_lon** vector character: containing the longitude coordinate name column
* **do_clean** logical:
* **drop_out** vector character: method for dropping outliers out from occurrence database. Methods are "any", "IQR", ""freq".
* **IQR_mtpl**

#### Environmental varialbles

* **clim_vars** Which climatic data use, useful when you want to compare fit of different climatic data sets
* **dir_clim**
* **dir_other**
* **extension_vars**
* **uniq1k_method** "sqkm", "spthin"
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
* *kept*
* **keep_files**
* **write_intfiles**
* **transf_biomo_ext**
