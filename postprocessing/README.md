# Postprocessing

This section plays a crucial role in biomodelos-sdm project pipeline, where we refine and enhance the results obtained from our modeling and preprocessing efforts. The postprocessing phase is where we take the output of our models and apply a series of carefully designed steps to extract valuable insights, improve the quality of our data, and present our findings in a clear and meaningful manner. In this folder, you will find a collection of scripts, tools, and documentation dedicated to the postprocessing stage of our project. Our team has put in significant effort to develop methodologies that help us distill raw model outputs into refined outcomes that are ready for analysis, visualization, and further interpretation.

## Prerequisitos

### Dependencias y archivos

* [R](https://cran.r-project.org/mirrors.html) 4.1.1 o upper
* [RStudio](https://www.rstudio.com/products/rstudio/download/#download) Optional

### Librerias

Required Libraries and Their Versions for Each Indicator. Ensure you have the exact versions of each package and they are compatible with the R version.

1. Checking Completion Status of Modelling Construction Process

```
"stringr" 1.4.0
"dplyr" 1.0.7
```

## How to run

Se sugiere correr las rutinas paso por paso, siguiendo el orden de cada script. Sin embargo, usted puede obtener los resultados de cada indicador independientemente. Las capas base de ejemplo deben ser descromprimidas dentro del directorio de trabajo asignado por el usuario.


## Description

#### [Checking Completion Status of Modelling Construction Process](...)

The provided script is designed to automate the assessment of the completion status of a SDM modeling construction process. The script operates on a collection of species folders, each representing a separate modeling run. By examining the contents and log files of these folders, the script systematically determines the progression of each modeling run. It identifies key stages, such as evaluation, final model creation, and the generation of ensembles for future predictions. Additionally, the script captures any encountered errors and records the time taken for each modeling run. Through a series of organized checks and evaluations, the script compiles this information into a structured data frame. This data frame provides a comprehensive overview of the modeling process, including species-specific details on progress, errors, and time allocation. By automating this analysis, the script streamlines the process of tracking the status of multiple modeling runs, enhancing efficiency and facilitating effective project management.
