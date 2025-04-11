# BioModelos: Species Distribution Maps from Colombian species

BioModelos is a collaborative online system to map species distributions (Velásquez-Tibata et al, 2019). This repository stores functions to automate the construction of Species Distribution Models (SDM) from databases gathered, managed and curated by The Alexander Von Humboldt Institute and a network of experts in order to be evaluated and refined.

Current state: in development.

## Folders

### Pre-processing
This folder includes routines that produce layers and data necessaries to run ecological niche models. The first implementation correspond to the obtention of mask in GIS format to cut environmental layers.

### Modelling
This folder includes functions and routines necessary to run ecological niche models using Maxent. These routines represent the BioModelos backbone used to infer spatial distribution models.

### Post-Processing
This folder includes functions and routines proposed to clean ecological niche models and to obtain products based on the BioModelos, such as species richness maps, vulnerability index, and others that could be used as indicators.

### Study-cases
This folder includes functions and routines (Pre-processing, modeling, post-processing) modified to run models to specific cases or biological groups. Other examples in Spanish and English are available online through the [Biodiversity and Climate Change website](https://proyectos.humboldt.org.co/cambioclimatico/en/developing-an-integrated-species-distribution-modelling-system-to-identify-complementary-conservation-areas-in-colombia/) 

## Authors and contact

* **Carlos Jair Muñoz Rodriguez, [contacto institucional](mailto:cmunoz@humboldt.org.co), [contacto personal](mailto:cmunozbiol@gmail.com)**
* **Elkin Alexi Noguera Urbano, [contacto institucional](mailto:enoguera@humboldt.org.co), [contacto personal](mailto:elkalexno@gmail.com)**

## Collaborators

* **María Helena Olaya, [contacto institucional](mailto:molaya@humboldt.org.co ), [contacto personal](mailto:olaya42@gmail.com)**

* **Gabriel Alejandro Perilla Suarez, [contacto institucional](mailto:gperilla@humboldt.org.co), [contacto personal](mailto:thealejandroperilla@gmail.com)**

* **Héctor Manuel Arango Martínez, [contacto institucional](mailto:harango@humboldt.org.co), [contacto personal](hma9327@gmail.com)**

* **Cristian Alexander Cruz Rodriguez, [contacto institucional](mailto:ccruz@humboldt.org.co), [contacto personal](cruzrodriguezcristian@gmail.com)**

* **Luis Hernando Romero Jiménez, [contacto institucional](mailto:lromero@humboldt.org.co), [contacto personal](mailto:lhromeroj@gmail.com)**

* **Andrés Felipe Suárez Castro, [contacto personal](mailto:felipesuarezca@gmail.com)**

## Acknowledgment

*This development is supported by National Geographic Society Grant #NGS-86896T-21, project "Developing an integrated species distribution modelling system to identify complementary conservation areas in Colombia". [Project information](https://explorer-directory.nationalgeographic.org/elkin-a-noguera-urbano)**
* Initial phases of the project were supported by Natural map.

## License

This project is licensed under the MIT License - see the [License.md](License.md) file for details
