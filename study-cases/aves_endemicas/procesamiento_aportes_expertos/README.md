# Procesamiento modelos aves endémicas

## Archivos necesarios

1. Archivos de ediciones generadas por expertos, usar utilidad bmdbutils para hacer conexión con la base de datos usando el `comando ecovars`
2. Tabla de coberturas seleccionadas por expertos, usar utilidad bmdbutils para hacer conexión con la base de datos usando el `editions`

Debido a la privacidad del api de Biomodelos, las solicitudes de ediciones y variables se gestionan individualmente. Para más información, comunicarse al correo **biomodelos@gmail.com**.

## Rutinas

- 1_edicion_aves_endemicas.R: editar modelos con ediciones allegadas desde 2021 a 2022.
- 2_edicion_aves_baja_evaluacion_N1.R: editar modelos con ediciones allegadas desde enero de 2023 a mayo de 2023.
- 3_edicion_aves_media_evaluacion_N1.R: editarmodelos con ediciones allegadas desde junio de 2023 a septiembre de 2023.
- 4_edicion_aves_vireos_N1.R: generación de modelos de experto vara los vireos de las islas de San Andres y Providencia.
- 5_refinamiento_aves_endemicas_N1_to_N2.R: refinamiento de modelos nivel 1 (modelo de distribución potencial con ediciones de experto) a modelos nivel 2 usando las coberturas seleccionadas por expertos.

