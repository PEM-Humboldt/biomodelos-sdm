# BioModelos: Species Distribution Maps from Colombian species

BioModelos is a collaborative online system to map species distributions (Velásquez-Tibata et al, 2019). This repository stores functions to automate the construction of Species Distribution Models (SDM) from databases gathered, managed and curated by The Alexander Von Humboldt Institute and a network of experts in order to be evaluated and refined.

Current state: in development.

## Folders

Describe each one

### Pre-processing
This repository include routines that produce layers and data necessaries to run ecological niche models. The first implementation correspond to the obtention of mask in GIS format to cut environmental layers.

### Modelling
This repository include functions and routines necessaries to run ecological niche models using Maxent. This routines represent the BioModelos backbone used to infer spatial distribution models.

### Post-Processing
This repository include functions and routines proposed to clean ecological niche models and to obtain products based on the BioModelos such as spcies richness maps, vulnerability index and another which could be used as indicators. 

### Study-cases
This repository include functions and routines (Pre-processing, modelling, post-processing) modified to run models to specific cases or biological groups.

## Authors and contact

* **Carlos Jair Muñoz Rodriguez, [contacto institucional](mailto:cmunoz@humboldt.org.co), [contacto personal](mailto:cmunozbiol@gmail.com)**

* **María Helena Olaya, [contacto institucional](mailto:molaya@humboldt.org.co ), [contacto personal](mailto:olaya42@gmail.com)**

* **Gabriel Alejandro Perilla Suarez, [contacto institucional](mailto:gperilla@humboldt.org.co), [contacto personal](mailto:thealejandroperilla@gmail.com)**

* **Héctor Manuel Arango Martínez, [contacto institucional](mailto:harango@humboldt.org.co), [contacto personal](hma9327@gmail.com)**

* **Cristian Alexander Cruz Rodriguez, [contacto institucional](mailto:ccruz@humboldt.org.co), [contacto personal](cruzrodriguezcristian@gmail.com)**

* **Luis Hernando Romero Jiménez, [contacto institucional](mailto:lromero@humboldt.org.co), [contacto personal](mailto:lhromeroj@gmail.com)**

* **Andrés Felipe Suárez Castro, [contacto personal](mailto:felipesuarezca@gmail.com)**

* **Elkin Alexi Noguera Urbano, [contacto institucional](mailto:enoguera@humboldt.org.co), [contacto personal](mailto:elkalexno@gmail.com)**

## Acknowledgment

This development is supported by National Geographic Society Grant #NGS-86896T-21.
This development was supported by Natural map in its first phase.

## License

This project is licensed under the MIT License - see the [License.md](License.md) file for details
