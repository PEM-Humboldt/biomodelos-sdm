# Postprocessing

This section plays a crucial role in biomodelos-sdm project pipeline, where we refine and enhance the results obtained from our modeling and preprocessing efforts. The postprocessing phase is where we take the output of our models and apply a series of carefully designed steps to extract valuable insights, improve the quality of our data, and present our findings in a clear and meaningful manner. In this folder, you will find a collection of scripts, tools, and documentation dedicated to the postprocessing stage of our project. Our team has put in significant effort to develop methodologies that help us distill raw model outputs into refined outcomes that are ready for analysis, visualization, and further interpretation.

## Prerequisites

### Dependencies and files

* [R](https://cran.r-project.org/mirrors.html) 4.1.1 o upper
* [RStudio](https://www.rstudio.com/products/rstudio/download/#download) Optional

### Libraries

Required Libraries and Their Versions for Each Indicator. Ensure you have the exact versions of each package and they are compatible with the R version.

- Checking Completion Status of Modelling Construction Process

```
"stringr" 1.4.0
"dplyr" 1.0.7
```
- Automated Metadata Generation for Species Modeling Results

```
"raster" 3.6-3
"dplyr" 1.0.9
"hms" 1.1.1
"xlsx" 0.6.5
```

- move_modelled_thresholds.R

```
stringr
dplyr
data.table
```

- Progressive Accumulation of Raster Data for Total Richness Calculation

```
dplyr
terra
stringr
```

## How to Run

### Independent Codes

 - [check_models_constructed.R](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/R/check_models_constructed.R)
 - [make_models_metadata.R](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/R/make_models_metadata.R)
 - [move_modelled_thresholds.R](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/R/move_modelled_thresholds.R)
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

#### [move_modelled_thresholds.R](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/R/calculate_simple_alpha_richness.R)

This function moves model result files (threshold layers and optional occurrence records) from one directory (`from`) to another (`to`). It searches for specific model outputs based on the `bin_threshold` pattern and `model` algorithm within the `from` directory. If `move_occs` is TRUE, it also moves associated occurrence files from the `from` directory.

#### [Progressive Accumulation of Raster Data for Total Richness Calculation](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/R/calculate_simple_alpha_richness.R)

This R script performs the progressive accumulation of raster data from TIFF files in a specified directory. During the loop, the cell values of each file are loaded and summed, accumulating them into a total raster. Every 100 iterations or at the last iteration, the accumulated raster is saved to a new TIFF file, memory is cleaned, and the accumulated result is reloaded as the new base raster. At the end of the loop, a graph of the accumulated raster is generated and the final result is saved in a TIFF file with a specific name. This approach aims to calculate the total richness of raster data incrementally, facilitating the processing of large spatial datasets on **computers with low processing power and a massive number of raster surfaces**. This function must be used using one raster per species.

### Interconnected Codes

#### Group 1: future refugia from ensembles of predicted species distribution models 

By running group 1 scripts you will be able to find the areas of climatic stability or refugia according to the theoretical framework of Type 1, as outlined by Brambilla et al. (2022). Type 1 refugia are defined as locations that meet the criteria of being suitable across all projected time periods (eg., present, 2050, and 2070), indicating significant temporal persistence and being found exclusively in areas of climatic stability (in situ). Climatic refugia are sectors of species distribution that can be considered resistant to change and play a fundamental role in the survival of populations. These areas are currently suitable and are expected to remain suitable in the future. Therefore, they are being considered the most important places for species conservation, regardless of the period and future conditions (Brambilla et al. 2022).

Codes that are interlinked to produce comprehensive results. Follow these steps to execute them together:

 - [future_ensemble.R](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/R/future_ensemble.R)
 - [calculate_future_refugia.R](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/R/calculate_future_refugia.R)
 - [future_ensemble_continuo.R](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/R/future_ensemble_continuo.R)

1. Open the R folder.
2. Open the files containing both interconnected codes.
3. First, execute the `future_ensemble.R` code following the provided instructions using the `ensemble_method = "binary"`.
4. Once `future_ensemble_binario.R` has completed its execution, proceed to run `calculate_future_refugees.R` by following its corresponding instructions.

Alternatively, instead of run `future_ensemble` using the `ensemble_method = "binary"` you can use `ensemble_method = "continuos"` Tip: In case of having a limited sample of species distribution scenarios, use the thresholded (binarized) method. Otherwise, continuos ensambling method.

#### [Future Ensemble Processing](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/R/future_ensemble.R)

Ensembling climate change scenarios involves aggregating multiple projections derived from climate models to provide a comprehensive view of potential ecological impacts. This function processes raster data representing future climate conditions, utilizing either binary or continuous methods to derive insights. The binary method categorizes suitable habitats based on threshold values, while the continuous method calculates medians and standard deviations to assess ecological suitability. By integrating these approaches, the function addresses uncertainty and variability in climate projections.


#### [Calculate Future Climatic Refugia](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/R/calculate_future_refugia.R)

#### Group 2: model statistics for BioModelos platform and Atlas  



