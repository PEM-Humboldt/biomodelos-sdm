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

4. Load setup functions of **Biomodelos 2**. In the script editor type 

```
source(setup.R)
``` 

5. Then run it using the icon "Run" or the keyboard shortcut "Ctrl+Enter" in windows. You will find four new objects in the environment (upper left portion of the RStudio window)
+ *vector.packages* vector character that stores the name of each package necessary to run **Biomodelos 2**
+ *do.folder.structure* function to create folders to organize work process in the workind directory
+ *do.install* automatic installation of needed packages
+ *do.load* 

6. Run the automatic installation in the editor script. The process will install the packages stored in the 'vector.packages' object. In case of showing a compilation window procedure, it is normally better to accept as it diminish likely of installation errors. Warning: if you have a version of ENMeval package upper to 0.3.1 it will be replace.
```
do.install(vector.packages)
``` 

7. You only need to install the packages once, so, it is better to block this command line typing a '#' character in the forefront of the line just before of the first run, like this `# do.install(vector.packages)`. If you need to troubleshoot on installing the packages, please refer to the vignete **Manual Installation of Packages** MISSING.

8. Load the installed packages with

```
do.load(vector.packages)
``` 
9. Create the structure of folders typing and run. The use of the character 'worldclim' inside the function does not refer to retrieve the data from the repository. It is only a way to create an organized framework inside the working directory in which you may store the variables downloaded manually or using automatized tools. 

```
do.folder.structure("worldclim")
``` 

10. Load the wrapper function "Bio2_routine". This function follows the basic structure of a "Ecological Niche Modelling" (ENM) process (Peterson et al, 2011). It calls several subroutines to achieve this with a few inputs and having a wide range of customization. Also, it is useful for users not familiarized with ENM's or R. Please refer to **Structure and Functions** and **More deep in Bio2_routine** vignettes to find more information about. 

```
source("R/Bio2_routine.R")
``` 

Now you are ready to run ENM models.

## Example

Having done

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
