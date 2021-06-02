# Biomodelos 2

Biomodelos 2 is an effort to construct thousands of Species Distribution Models from databases gathered and managed by the Instituto de Investigacion de Recursos Biologicos Alexander von Humboldt (IAvH). It is attached to Biomodelos initiative from the IAvH. "Biomodelos 2" follows an automatized SDM general routine. First, it cleans occurrence data and constructs the accessible area when is necesary. Second, it crops and masks current and future environmental variables. Third, it creates SDM's using several machine learning algorithms and then ensembles them. Fourth, it projects to future scenarios and evaluates extrapolation.

Current state: in development. Version 1.0.0

## Prerequisites

### Dependencies and files

Dependencies to install, choose the version depending on your operating system and version. For example, a windows 10 terminal with more than 4 gigabytes on memory RAM almost always has a 64 bit version of that version. Surf on the web in case of more information.

* [R](https://cran.r-project.org/mirrors.html)
* [RStudio](https://www.rstudio.com/products/rstudio/download/#download)
* [Rtools](https://cran.r-project.org/bin/windows/Rtools/)
* [Java Development Kit](https://www.oracle.com/java/technologies/javase/javase-jdk8-downloads.html)
* [Maxent](https://drive.google.com/file/d/1a-0QPZyqk9DFWEm7rSreBTYiuTJDgABR/view)
* This repository

### Libraries
Libraries required and their versions

```
"dplyr" version 1.04
"plyr" version 1.8.6
"rJava" version 0.9-13
"automap" version 0.9-13
"PresenceAbsence" version 1.1.9
"devtools" version 2.3.2
"CoordinateCleaner" version 2.0-18
"sf" version 0.9-7
"spThin" version 0.2.0
"raster" version 3.4-5
"dismo" version 1.3-3
"biomod2" version 3.4.6
"ENMeval" version 0.3.1
"rgdal" version 1.5-23
"kuenm" version 1.1.6
```

## How to run

1. Create a folder and move (uncompressed) the content of this repository and "maxent.jar" file (downloaded previously) there. For better results choose a root directory like "C" or "D" in windows to create the folder (working directory).
2. Open RStudio and create a new project using the folder created as working directory. It can be achieving doing the next. First, click on tool bar "File" (upper left of the RStudio window). Second, "New Project". In the opened window, click on "Existing Directory". After that, browse into the computer folder structure until reach the folder created in the step 1. Last, get click on "Create Project". *This step is comparative to setup a working directory with* `setwd()`
3.  Create in RStudio a new script. It can be achieve going to "File" tool bar, "New File" and then "R Script". It may well be used the icon "New file" right under the tool bar "File" or using the keyboard shortcut "Ctrl+Shift+N" in windows.
4. 


7. Cargue y corra las funciones de configuración de las funciones de biomodelos2:
Para cargar las funciones, situado dentro del editor de script de R, porción superior de la pantalla, escriba el siguiente comando, source("setup.R"). Luego ejecute o corra haciendo click en el icono image.png "Run" o use la combinación de teclas "Ctrl+Enter" en windows. Despues de cargar las funciones de configuración encontrara en el ambiente ("Environment") de RStudio, situado en la porción superior derecha de la pantalla, 4 objetos nuevos: `vector.packages`   ( paquetes de R necesarios),  `do.install` (instalación automatica de los paquetes),  `do.load`   (carga automatica de los paquetes ) y  `do.folder.structure` (creación de la estructura de archivos).
En el editor, escriba el siguiente comando, do.install(vector.packages). R comenzara a descargar e instalar los paquetes necesarios para la rutina, si se abre una ventana en la pantalla que le pregunta sobre instalar dependencias que necesitan compilación, click en Si ("Yes"). El proceso suele ser mas demorado que en una instalación normal, pero este proceso ayuda a que existan menos errores. Una vez instalados los paquetes, lo mejor seria bloquear el comando de instalación, situand un simbolo # antes del comando de instalación, dejando la linea del script como #  do.install(vector.packages).
Cargue los paquetes con la función do.install(), siguiendo este comando, do.load(vector.packages).
Instale el paquete "kuenm". Escriba y corra el siguiente comando, devtools::install_github("marlonecobos/kuenm"). Espere a instalar el paquete.
Cree la estructura de carpetas necesarias al escribir y correr el siguiente comando, do.folder.structure("worldclim"). El caracter "worldclim" no supone una descarga de variables, sino la creación de la estructura donde deben ser almacenadas las variables.
Cargue la rutina creada, "Bio2_routine", corriendo el comando, source("R/Bio2_routine.R")



### Authomatic installation of libraries



Please explain the order to run each script using bullets and identation, showing step by step series of examples that tell you how to get your scripts running, which one to run first and how to call the others

Say what the step will be

```
Give the example
```

And repeat

```
until finished
```

**End with an example of getting some data out of the system or using it for a little demo**

## Authors and contact

* **Billie Thompson** - *Initial work* - [PurpleBooth](https://github.com/PurpleBooth)

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## [Optional] Acknowledgments

* Hat tip to anyone whose code was used
* Inspiration
* etc

## [Optional] Contributing

It usually has its own file: [CONTRIBUTING.md](https://gist.github.com/PurpleBooth/b24679402957c63ec426), which details the code of conduct, and the process for submitting pull requests to us.
