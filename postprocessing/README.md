# Postprocessing

This section plays a crucial role in biomodelos-sdm project pipeline, where we refine and enhance the results obtained from our modeling and preprocessing efforts. The postprocessing phase is where we take the output of our models and apply a series of carefully designed steps to extract valuable insights, improve the quality of our data, and present our findings in a clear and meaningful manner. In this folder, you will find a collection of scripts, tools, and documentation dedicated to the postprocessing stage of our project. Our team has put in significant effort to develop methodologies that help us distill raw model outputs into refined outcomes that are ready for analysis, visualization, and further interpretation.

## Prerequisites

### Dependencies and files

* [R](https://cran.r-project.org/mirrors.html) 4.1.1 o upper
* [RStudio](https://www.rstudio.com/products/rstudio/download/#download) Optional

### Libraries

Required Libraries and Their Versions for Each Indicator. Ensure you have the exact versions of each package and they are compatible with the R version.

1. Checking Completion Status of Modelling Construction Process

```
"stringr" 1.4.0
"dplyr" 1.0.7
```
2. Automated Metadata Generation for Species Modeling Results

```
"raster" 3.6-3
"dplyr" 1.0.9
"hms" 1.1.1
"xlsx" 0.6.5
```

4. Progressive Accumulation of Raster Data for Total Richness Calculation

```
dplyr
terra
stringr
```

## How to Run

### Independent Codes

 - [check_models_constructed.R](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/R/check_models_constructed.R)
 - [make_models_metadata.R](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/R/make_models_metadata.R)
 - [mov_models.R](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/R/mov_models.R)
 - [calculate_simple_alpha_richness.R](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/R/calculate_simple_alpha_richness.R)

You can obtain the results of each code independently by following these steps:

1. Open the R folder.
2. Open the file corresponding to the code you wish to run.
3. Follow the execution instructions provided within the code file itself. This might include configuring specific parameters or loading required data.
4. Run the code.

## Description

#### [Checking Completion Status of Modelling Construction Process](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/R/check_models_constructed.R)

The provided script is designed to automate the assessment of the completion status of a SDM modeling construction process. The script operates on a collection of species folders, each representing a separate modeling run. By examining the contents and log files of these folders, the script systematically determines the progression of each modeling run. It identifies key stages, such as evaluation, final model creation, and the generation of ensembles for future predictions. Additionally, the script captures any encountered errors and records the time taken for each modeling run. Through a series of organized checks and evaluations, the script compiles this information into a structured data frame. This data frame provides a comprehensive overview of the modeling process, including species-specific details on progress, errors, and time allocation. By automating this analysis, the script streamlines the process of tracking the status of multiple modeling runs, enhancing efficiency and facilitating effective project management.

#### [Automated Metadata Generation for Species Modeling Results](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/R/make_models_metadata.R)

This script simplifies the process of creating metadata for species modeling outcomes. By analyzing modeling results stored in specific directories, the script automatically extracts essential information like model parameters and evaluation metrics. This eliminates manual effort and enhances accuracy. The script can handle various modeling algorithms and scenarios, producing organized metadata files in XLSX format. This efficient approach enhances the documentation and management of species modeling results, making research processes smoother.

#### [Progressive Accumulation of Raster Data for Total Richness Calculation]()

This R script performs the progressive accumulation of raster data from TIFF files in a specified directory. During the loop, the cell values of each file are loaded and summed, accumulating them into a total raster. Every 100 iterations or at the last iteration, the accumulated raster is saved to a new TIFF file, memory is cleaned, and the accumulated result is reloaded as the new base raster. At the end of the loop, a graph of the accumulated raster is generated and the final result is saved in a TIFF file with a specific name. This approach aims to calculate the total richness of raster data incrementally, facilitating the processing of large spatial datasets on **computers with low processing power and massive number of raster surfaces**.

### Interconnected Codes

#### Group 1: future refugees from ensembles of predicted species distribution models 

Codes that are interlinked to produce comprehensive results. Follow these steps to execute them together:

 - [future_ensemble_binario.R](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/R/future_ensemble_binario.R)
 - [future_ensemble_continuo.R](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/R/future_ensemble_continuo.R)
 - [calculate_future_refugees.R](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/R/calculate_future_refugees.R)
 - [calculate_simple_alpha_richness](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/R/calculate_simple_alpha_richness.R)

1. Open the R folder.
2. Open the files containing both interconnected codes.
3. First, execute the `future_ensemble_binario.R` code following the provided instructions.
4. Once `future_ensemble_binario.R` has completed its execution, proceed to run `calculate_future_refugees.R` by following its corresponding instructions.

#### Group 2: model statistics for BioModelos platform and Atlas  



