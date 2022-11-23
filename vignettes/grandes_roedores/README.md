# Grandes Roedores

## Archivos necesarios

1. Registros de distribución (en proceso de disposición al público).
2. Variables ambientales: [vinculo](https://www.worldclim.org/) recortar usando esta extensión.
3. Capa de sesgo: [vinculo](https://github.com/PEM-Humboldt/biomodelos-sdm/blob/master/R_BioMScripts/grandes_roedores/roedores_bias_file.tif)
4. Repositorio de rutinas biomodelos-sdm: version 1.0.0, instalar siguiendo las indicaciones que aparecen en el readme.

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
tipos <- c(con_sesgo, sin_sesgo)

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
