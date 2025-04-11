# Postprocessing

This section plays a crucial role in biomodelos-sdm project pipeline, where we refine and enhance the results obtained from our modeling and preprocessing efforts. In the postprocessing phase, we take the output of our models and apply a series of carefully designed steps to extract valuable insights, improve the quality of our data, and present our findings clearly and meaningfully. In this folder, you will find a collection of scripts, tools, and documentation dedicated to the postprocessing stage of our project. Our team has made significant efforts to develop methodologies that help us distill raw model outputs into refined outcomes ready for analysis, visualization, and further interpretation.

## Prerequisites

### Dependencies and files

* [R](https://cran.r-project.org/mirrors.html) 4.1.1 o upper
* [RStudio](https://www.rstudio.com/products/rstudio/download/#download) Optional

## How to Run

You can obtain the results of these codes by following these steps:

1. Open the R folder.
2. Open the file corresponding to the code you wish to run.
3. Follow the execution instructions provided within the code file itself. This might include configuring specific parameters or loading required data.
4. Run the code.

## Code to reduce the overprediction in the species distribution models

### [Filter habitat By Presences](https://[github.com/PEM-Humboldt/biomodelos-sdm/blob/master/preprocessing/R/species_occurrence_river_relocation.R](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/R/FilterHabitatByPresence))

This R routine, implemented using the terra package, aims to retain only those habitat patches within a binary raster where the records support species presence. The function, named "FilterHabitatByPresence", first transforms the input raster into a binary mask by treating all NA values as absent. It then identifies contiguous habitat patches using the patches function (with 8-cell neighborhood connectivity). The presence points provided are used to extract which patches they intersect. Only patches containing at least one presence point are retained, while all others are masked. The final output maintains the original values of the retained patches and is saved as a new .tif raster file. The latter part of the script includes simulated examples to test the function. The first example randomly generates a habitat raster and a set of presence points (some of which may fall outside the habitat), demonstrating how the function filters out non-relevant patches. The second example ensures the presence points fall within valid habitat cells, showing a successful case of patch selection. This routine is beneficial in ecological niche modeling or species distribution modeling when it is necessary to restrict predictions to areas with confirmed presence, thereby reducing overestimation and increasing the spatial accuracy of habitat suitability maps.

## Species richness and refugia maps 

### [Richness calculation from multi-stack species distribution models](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/R/calculate_simple_alpha_richness.R)

This R script performs the progressive accumulation of raster data from TIFF files in a specified directory. During the loop, the cell values of each file are loaded and summed, accumulating them into a total raster. Every 100 iterations or at the last iteration, the accumulated raster is saved to a new TIFF file, memory is cleaned, and the accumulated result is reloaded as the new base raster. At the end of the loop, a graph of the accumulated raster is generated, and the final result is saved in a TIFF file with a specific name. This approach aims to calculate the total richness of raster data incrementally, facilitating the processing of large spatial datasets on **computers with low processing power and a massive number of raster surfaces to process**. This function must be used using one raster per species.

### Future refugia from ensembles of predicted species distribution models 

By running these scripts, you can find the areas of climatic stability or refugia according to the theoretical framework of Type 1, as outlined by Brambilla et al. (2022). Type 1 refugia are defined as locations that meet the criteria of being suitable across all projected time periods (eg., present, 2050, and 2070), indicating significant temporal persistence and being found exclusively in areas of climatic stability (in situ). Climatic refugia are sectors of species distribution that can be considered resistant to change and play a fundamental role in the survival of populations. These areas are currently suitable and are expected to remain suitable in the future. Therefore, they are considered the most important places for species conservation, regardless of the period and future conditions (Brambilla et al., 2022).

#### [Future Ensemble Processing](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/R/future_ensemble.R)

Ensembling climate change scenarios involves aggregating multiple projections from climate models to provide a comprehensive view of potential ecological impacts. This function processes raster data representing future climate conditions, utilizing either binary or continuous methods to derive insights. The binary method categorizes suitable habitats based on threshold values, while the continuous method calculates medians and standard deviations to assess ecological suitability. The function addresses uncertainty and variability in climate projections by integrating these approaches. As it only works in biomodelos-sdm modeling results. *As it only works in biomodelos-sdm modeling results*.


#### [Calculate Future Climatic Refugia](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/R/calculate_future_refugia.R)

This code focuses on constructing climate refugia, which are areas where environmental conditions are projected to be suitable in both current and future climate scenarios. The function accesses climate model ensembles for different future periods, generating raster maps that depict projected environmental conditions. These maps are compared with current data to pinpoint areas where environmental conditions consistently meet suitability criteria across present and future climate scenarios. This binary approach aids in categorizing areas as potential climate refugia based on specific thresholds of environmental suitability. *As it only works in biomodelos-sdm modelling results*.

These codes are interlinked to produce comprehensive results. Follow these steps to execute them together:

1. Open the R folder.
2. Open the files containing both interconnected codes.
3. First, execute the `future_ensemble.R` code following the provided instructions using the `ensemble_method = "binary"`.
4. Once `future_ensemble_binario.R` has completed its execution, proceed to run `calculate_future_refugees.R` by following its corresponding instructions.

Alternatively, instead of run `future_ensemble` using the `ensemble_method = "binary"` you can use `ensemble_method = "continuos"` Tip: In case of having a limited sample of species distribution scenarios, use the thresholded (binarized) method. Otherwise, continuos ensambling method.


## Code to produce metadata, auxiliar files and data for BioModelos platform and Atlas

### Information for statistics models developed with biomodelos-sdm modelling tool

#### [Checking Completion Status of Modelling Construction Process](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/Ediciones_BioModelos/Rutinas/check_models_constructed.R)

The provided script is designed to automate the assessment of the completion status of a SDM modeling construction process. The script operates on a collection of species folders, each representing a separate modeling run. By examining the contents and log files of these folders, the script systematically determines the progression of each modeling run. It identifies key stages, such as evaluation, final model creation, and the generation of ensembles for future predictions. Additionally, the script captures any encountered errors and records the time taken for each modeling run. Through a series of organized checks and evaluations, the script compiles this information into a structured data frame. This data frame provides a comprehensive overview of the modeling process, including species-specific details on progress, errors, and time allocation. By automating this analysis, the script streamlines the process of tracking the status of multiple modeling runs, enhancing efficiency and facilitating effective project management. *As it only works in biomodelos-sdm modelling results*.

#### [Automated Metadata Generation for Species Modeling Results](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/R/Ediciones_BioModelos/Rutinas/make_models_metadata.R)

This script simplifies the process of creating metadata for species modeling outcomes. The script loads the [make_metadata_functions](https://github.com/PEM-Humboldt/biomodelos-sdm/tree/master/postprocessing/Ediciones_BioModelos/Rutinas/Funciones/make_metadata_functions.R). By analyzing modeling results stored in specific directories, the script automatically extracts essential information like model parameters and evaluation metrics. This eliminates manual effort and enhances accuracy. The script can handle various modeling algorithms and scenarios, producing organized metadata files in XLSX format. This efficient approach enhances the documentation and management of species modeling results, making research processes smoother. *As it only works in biomodelos-sdm modelling results*.

#### [Move Modelling Thresholds](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/Ediciones_BioModelos/Rutinas/calculate_simple_alpha_richness.R)

This function moves model result files (threshold layers and optional occurrence records) from one directory (`from`) to another (`to`). It searches for specific model outputs based on the `bin_threshold` pattern and `model` algorithm within the `from` directory. If `move_occs` is TRUE, it also moves associated occurrence files from the `from` directory. *As it only works in biomodelos-sdm modelling results*.

### Information for consensus (potential) and remanent species distribution models

#### [Analysis of potential distribution and remnant distribution of species by departments and Regional Autonomous Corporations (CAR).](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/Ediciones_BioModelos/Rutinas/Estadisticas_BioModelos.R)

The code performs an analysis of the potential distribution of species and remnant areas in Colombia. It uses raster and vector data to assess the presence of species in different departments and areas under the jurisdiction of the Regional Autonomous Corporations. Initially, it creates storage directories and loads species model data from specific locations. It then uses administrative polygons and areas of interest to clip and mask species models, determining whether species are present or absent in each area. The results are recorded in CSV files separated by department and area type, facilitating additional analysis on potential distribution and species conservation within the Colombian context. *This information is used to construct data for Atlas*.

#### [Calculation of statistics for BioModelos](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/Ediciones_BioModelos/Rutinas/Estadisticas_BioModelos.R)

The workflow is designed to calculate various statistics from remanent species distribution models. The script loads the [stats_SDM_V2withCLC](https://github.com/PEM-Humboldt/biomodelos-sdm/tree/master/postprocessing/Ediciones_BioModelos/Rutinas/Funciones/stats_SDM_V2withCLC.R) functions. It generates two main CSV files: the first includes statistics such as habitat coverage (using Level 3 land cover types from 2018), forest occurrence percentages across different years (1990, 2000, 2005, 2010, 2012), projections for three scenarios for 2030, percentage of distribution area within protected areas, estimated area using Minimum Convex Polygon (MCP) based on occurrence pixels and records, among others. The second CSV contains information on threats including human footprint, road networks, and mining titles. The script iterates through species models stored as raster files, calculating various metrics for each species such as range size, habitat coverage, historical forest loss, future scenarios, and impact of protected areas and threats. Results are aggregated into two comprehensive CSV files (stats_level2.csv and stats_threats_level2.csv) for analysis and reporting. It also notes adjustments made due to package updates and provides insights on ongoing improvements in SDM methodologies using packages like ConR for future integration. *This information is used to construct metadata for BioModelos platform*.

On the other hand, this script is designed to calculate various statistics from potential species distribution models. This code snippet reads a CSV file containing metadata indices and processes raster files corresponding to species potential distribution models. For each species raster file, it calculates various statistics including the species' range size, Minimum Convex Polygon (MCP) area, and area of occupancy (using the ConR package). The results are compiled into a data frame and written to a CSV file. The script ensures that projections are consistent and prepares the data for further analysis by storing it in a structured format, facilitating the evaluation of species distributions and potential habitats. *This information is used to construct data for Atlas*.

### Auxiliar files to upload models in BioModelos platform

#### [Converting TIFF files to PNG images and creating additional files for uploading in BioModelos](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/Ediciones_BioModelos/Rutinas/Extent%26convert2PNG.R)

This script prepares species distribution models data for BioModelos by converting TIFF files to PNG images and creating additional visualization files. It starts by loading necessary libraries, setting the working directory, and loading a reference map along with the [convert2PNG](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/Ediciones_BioModelos/Rutinas/Funciones/convert2PNG.R) function and [parameters](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/Ediciones_BioModelos/Rutinas/Funciones/params.RData). It then processes continuous and thresholded models by adjusting projections, defining color palettes, reclassifying raster data, and converting them to PNG images. The script also handles the conversion of Level 1 (N1) and Level 2 (N2) models by applying the convert2PNG function to these models stored in specified folders, generating PNG images with specific color palettes. This script is linked to the model uploading through image workflow,. It is worth noting that although any type of model could be uploaded through this flow, currently only continuous and threshold models are uploaded to the platform through this procedure.

#### [Creating zip from metadata table for uploading images workflow](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/Ediciones_BioModelos/Rutinas/create_zip_from_metadata_table.R)

This code processes metadata from a specified CSV file, extracts key data for each model, and generates additional information like thumbnails path, PNG images path, and create ZIP files. It organizes these files into a designated output folder. Initially, the script reads metadata and creates a directory to store images and zip files. It defines the path for the raster TIFF files and processes each model by extracting and formatting necessary details, such as species name and modeling method. Depending on the metadata, it constructs names for the models and updates the metadata with filenames for the thumbnails, PNGs, and ZIP files. It then saves individual metadata to CSV files, copies the corresponding raster TIFF files to the zip folder, and zips the files. Finally, it deletes the individual files after zipping and closes all open connections. This script is linked to the model uploading through image workflow,. It is worth noting that although any type of model could be uploaded through this flow, currently only continuous and threshold models are uploaded to the platform through this procedure.

#### [Geoserver-Ready Data Frame Construction from Metadata and Raster Files](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/postprocessing/Ediciones_BioModelos/Rutinas/construct_df_geoserver.R)

The code is designed to create a formatted data frame suitable for use with Geoserver, a geospatial platform. It takes three inputs: the path to a metadata file in either Excel (.xlsx) or CSV (.csv) format, the path to a folder containing raster files in ".tif" format, and the directory where the resulting CSV file should be saved. The function reads the metadata file, retrieves a list of raster files from the specified folder, sorts the metadata based on accepted names, and constructs a data frame (df_geoserver) with columns for taxonomy ID (tax_id), model ID (model_id), and corresponding raster file names (model_file). It then generates a CSV file named df_geoserver_<date>.csv in the specified output folder, where <date> is the current date in the format YYYYMMDD. This function ensures that the metadata and raster files are organized and ready for deployment on Geoserver.
