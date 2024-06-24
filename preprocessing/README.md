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

## How to Run

### Independent Codes

 - [download_occurrences_biomodelos.R](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/preprocessing/R/download_occurrences_biomodelos.R)
 - [bias_layer.R](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/preprocessing/R/bias_layer.R)
 - [move_river_occurrences.R](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/preprocessing/R/move_river_occurrences.R)

You can obtain the results of each code independently by following these steps:

1. Open the R folder.
2. Open the file corresponding to the code you wish to run.
3. Follow the execution instructions provided within the code file itself. This might include configuring specific parameters or loading required data.
4. Run the code.

## Description

#### [Download occurrences from BioModelos MongoDB](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/R/check_models_constructed.R)

This R script is designed to download and process species records from the BioModelos MongoDB database. It utilizes several R packages specialized in data manipulation and database connections. The process begins with loading these packages and reading a list of species from a CSV file. Subsequently, connections are established to the species and records collections in BioModelos MongoDB. A function (RetrieveSpRecords) is defined to retrieve occurrence records of the species from the database. The script then iterates over the species list, using this function to obtain the records and store them in a list. The complete records are combined into a single DataFrame and saved in a CSV file. Finally, the data is filtered to remove records identified as erroneous or problematic by BioModelos experts, and the filtered result is saved in another CSV file. This process ensures efficient collection and cleaning of species occurrence data from a structured data source.

