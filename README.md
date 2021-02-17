# 16k project - Biomodelos 2

"16k project" is an effort to construct thousands of Species Distribution Models from databases gathered and managed by the Instituto de Investigacion de Recursos Biologicos Alexander von Humboldt (IAvH). It is attached to Biomodelos initiative from the IAvH. "16k project" follows an automatized SDM general routine. First, it cleans occurrence data and constructs the accessible area. Second, it crops and masks current and future environmental variables. Third, it creates SDM's using several machine learning algorithms and then ensembles them. Fourth, it projects to future scenarios and evaluates extrapolation.

Current state: in development. Version 1.0.0

## Prerequisites

Libraries required and their versions

```
"dplyr" version 1.04
"plyr" version 1.8.6
"rJava" version 0.9-13
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

### Required files

URLs to download required files or templates to download before to run each script.

```
Give the example, as dataset names and where to download them
```

## How to run

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
