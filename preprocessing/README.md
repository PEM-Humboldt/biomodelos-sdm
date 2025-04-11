# Preprocessing


## Prerequisites

### Dependencies and files

* [R](https://cran.r-project.org/mirrors.html) 4.1.1 o upper
* [RStudio](https://www.rstudio.com/products/rstudio/download/#download) Optional

### Libraries

Required Libraries and Their Versions for Each Indicator. Ensure you have the exact versions of each package and they are compatible with the R version.

1. Download occurrences from BioModelos MongoDB

```
"mongolite"
"plyr"
"dplyr"
"devtools"
"gtools"
"openxlsx"
```

2. Spatial Bias Layer Generation from Target Group Sampling (TGS) Data

```
library(dplyr)
library(sf)
library(raster)
```

3. Spatial Relocation of Species Occurrences Around Aquatic Basins Using Raster Analysis

```
library(data.table)  
library(sf)          
library(terra)      
library(dplyr)  
```

## How to Run

### Independent Codes

 - [download_occurrences_biomodelos.R](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/preprocessing/R/download_occurrences_biomodelos.R)
 - [bias_layer_generation_from_TGS.R](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/preprocessing/R/bias_layer_generation_from_TGS.R)
 - [species_occurrence_river_relocation.R](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/preprocessing/R/species_occurrence_river_relocation.R)
 

You can obtain the results of each code independently by following these steps:

1. Open the R folder.
2. Open the file corresponding to the code you wish to run.
3. Follow the execution instructions provided within the code file itself. This might include configuring specific parameters or loading required data.
4. Run the code.

## Description

#### [Download occurrences from BioModelos MongoDB](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/R/check_models_constructed.R)

This R script is designed to download and process species records from the BioModelos MongoDB database. It utilizes several R packages specialized in data manipulation and database connections. The process begins with loading these packages and reading a list of species from a CSV file. Subsequently, connections are established to the species and records collections in BioModelos MongoDB. A function (RetrieveSpRecords) is defined to retrieve occurrence records of the species from the database. The script then iterates over the species list, using this function to obtain the records and store them in a list. The complete records are combined into a single DataFrame and saved in a CSV file. Finally, the data is filtered to remove records identified as erroneous or problematic by BioModelos experts, and the filtered result is saved in another CSV file. This process ensures efficient collection and cleaning of species occurrence data from a structured data source.

#### [Spatial Bias Layer Generation from Target Group Sampling (TGS) Data](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/preprocessing/R/bias_layer_generation_from_TGS.R)

This code is developed to create a bias layer from a Target Group Sampling (TGS) dataset, a common approach in ecological modeling and species distribution analysis. The script leverages spatial and statistical techniques to integrate occurrence data with ecological regions and environmental variables. In MaxEnt modeling using R, integrating a bias layer is a method to mitigate the effects of sampling biases inherent in species occurrence data. This layer enables adjustments that account for uneven sampling effort across landscapes, ensuring that model predictions more accurately reflect true species distributions. By correcting spatial biases and incorporating additional environmental context such as habitat suitability or anthropogenic influences, the bias layer enhances the robustness and reliability of MaxEnt models.

#### [Spatial Relocation of Species Occurrences Around Aquatic Basins Using Raster Analysis](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/preprocessing/R/species_occurrence_river_relocation.R)

The workflow facilitates the spatial adjustment of species occurrence records around aquatic basins using raster analysis techniques in R. This script is designed to integrate CSV data with raster templates, enabling the precise relocation of species points based on specified buffer distances from water bodies. Such spatial refinement is crucial for ecological studies aiming to accurately map species distributions and understand their environmental associations. By ensuring that species occurrences align with their ecological contexts, the script supports rigorous scientific research into distribution biodiversity patterns and habitat suitability assessments.



