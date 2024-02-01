# Aves endémicas

Este documento tiene como objetivo presentar el código utilizado en la construcción de los modelos de distribución de aves endémicas publicados en el atlas. Este código emplea una serie de rutinas automatizadas desarrolladas dentro del Instituto Humboldt, las cuales son de acceso público y están disponibles para su uso. Los mapas resultantes de la ejecución de este código fueron revisados, seleccionados y editados de manera consensuada por expertos, siguiendo la metodología presentada en el atlas.

## Archivos necesarios

1. Registros de distribución curados por expertos: plataforma [BioModelos](http://biomodelos.humboldt.org.co/) Debido a la privacidad de algunos registros y al nivel de riesgo para estas especies, las solicitudes de registros se gestionan individualmente. Para más información, comunicarse al correo **biomodelos@gmail.com**.
2. Variables ambientales: [enlace](https://www.worldclim.org/data/worldclim21.html) recortar usando [esta](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/vignettes/grandes_roedores/extension/) extensión.
3. Capa de sesgo: [enlace](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/vignettes/aves_endemicas/aves.tif)
4. Repositorio de rutinas biomodelos-sdm: [version 1.0.0](https://github.com/PEM-Humboldt/biomodelos-sdm/tree/1.0.0). Instalar siguiendo las indicaciones que aparecen en e [readme](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/1.0.0/README.md).
5. Capa de ecorregiones [Dinerstein et al., 2017](https://ecoregions.appspot.com/)

## Codigo

Una vez descargados los archivos necesarios, ejecutar:

```
# configuración de paquetes
source("setup.R")
do.install(vector.packages)
do.check(vector.packages)
do.load(vector.packages)

# configuración de carpetas
do.folder.structure("worldclim")
# una vez creada la estructura de archivos mover los archivos climaticos y de sesgo
```

Modelos

```
aves_endemicas_atlas <- read.csv("Occurrences/aves_endemicas_atlas.csv") # path for sp data

list <- split(aves_endemicas_atlas, f = aves_endemicas_atlas$acceptedNameUsage)

# cargar codigo de rutina, recuerde descargar la version 1.0.0
source("R/Bio2_routine.R")

for(i in 1:length(list)){
  for(a in 1:length){
    Bio2_routine(occ = list[[i]],
                 col_lat = "decimalLatitude",
                 col_lon = "decimalLongitude",
                 drop_out = "any"
                 polygon_data = NULL,
                 proj_models = "M-M",
                 dist_MOV = 74,
                 clim_vars = "worldclim",
                 dir_clim = ".../env_vars/", # ubicación variables ambientales
                 dir_other = ".../env_vars/other/", # ubicación variables ambientales no relacionadas al clima
                 TGS_kernel = ".../aves.tif", ubicación archivo de sesgo
                 do_clean = FALSE,
                 uniq1k_method = "sqkm",
                 dist_uniq = 1,
                 method_M = "polygon_points_buffer",
                 algos = "MAXENT",
                 beta_5.25 = seq(0.5, 4, 0.5),
                 fc_5.25 = c("l", "q", "lq"),
                 beta_25 = seq(1, 6, 1),
                 #fc_25 = c(), # default
                 use_bias = T,
                 keep_files = "essential",
                 transf_biomo_ext = TRUE, #FALSE,
                 polygon_data = ".../ecorregiones.shp
                 )

                 closeAllConnections()
                 gc()
  }
}  


```

Dentro de la carpeta de trabajo, se generarán los modelos. Posteriormente, estos modelos se editaron en colaboración con expertos, utilizando las herramientas disponibles en la plataforma BioModelos y los códigos ubicados en la carpeta de  [procesamiento](https://github.com/PEM-Humboldt/biomodelos-sdm/tree/master/study-cases/aves_endemicas/procesamiento_aportes_expertos).
