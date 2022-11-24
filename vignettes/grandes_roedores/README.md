# Grandes Roedores

Este documento tiene por objetivo mostrar el código utilizado en la construcción de los modelos de distribución de Grandes Roedores publicados en el atlas. Este código trabaja con una serie de rutinas automatizadas creadas dentro del Instituto Humboldt las cuales son de público acceso y uso. Los mapas resultados de ejecutar este codigo fueron revisados, seleccionados y editados en consenso por los expertos de acuerdo con la metodología presentada en el atlas.

## Archivos necesarios

1. Registros de distribución: plataforma [BioModelos](http://biomodelos.humboldt.org.co/) (se esta trabajando para poner a disposición del público los registros de manera masiva).
2. Variables ambientales: [vinculo](https://www.worldclim.org/data/worldclim21.html) recortar usando [esta](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/vignettes/grandes_roedores/extension/) extensión.
3. Capa de sesgo: [vinculo](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/vignettes/grandes_roedores/roedores_bias_file.tif)
4. Repositorio de rutinas biomodelos-sdm: [version 1.0.0](https://github.com/PEM-Humboldt/biomodelos-sdm/tree/1.0.0), instalar siguiendo las indicaciones que aparecen en el [readme](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/1.0.0/README.md).

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
load("Occurrences/grandes_roedores_atlas.csv") # path for sp data

list <- split(grandes_roedores_atlas, f = grandes_roedores_atlas$acceptedNameUsage)

# cargar codigo de rutina, recuerde descargar la version 1.0.0
source("R/Bio2_routine.R")

sesgo <- c(TRUE, FALSE)
tipos <- c("con_sesgo", "sin_sesgo")

for(i in 1:length(list)){
  for(a in 1:length){
    Bio2_routine(occ = list[[i]],
                 col_lat = "decimalLatitude",
                 col_lon = "decimalLongitude",
                 drop_out = "any"
                 polygon_data = NULL,
                 proj_models = "M-G",
                 dist_MOV = 222,
                 clim_vars = "worldclim",
                 dir_clim = ".../env_vars/", # ubicación variables ambientales
                 dir_other = ".../env_vars/other/", # ubicación variables ambientales no relacionadas al clima
                 TGS_kernel = ".../roedores_bias_file.tif", ubicación archivo de sesgo
                 do_clean = FALSE,
                 uniq1k_method = "sqkm",
                 dist_uniq = 1,
                 method_M = "points_buffer",
                 area_G = "/env_vars/worldclim/bio1.tif", # area para proyectar
                 algos = "MAXENT",
                 beta_5.25 = seq(0.5, 4, 0.5),
                 fc_5.25 = c("l", "q", "lq"),
                 beta_25 = seq(1, 6, 1),
                 #fc_25 = c(), # default
                 use_bias = sesgo[a],
                 tipo = tipo[a],
                 keep_files = "essential",
                 transf_biomo_ext = TRUE, #FALSE,
                 extrapo = "no_ext"
                 )

                 closeAllConnections()
                 gc()
    }
  }  
}

```
Dentro de su carpeta de trabajo apareceran los modelos.
