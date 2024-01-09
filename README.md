# ARGENTADATA
## argendata-etl

Este proyecto busca sistematizar en scripts de R o Python los procesos de generación de los outputs utilizados en argendata de la forma más reproducible posible.

## estructura del proyecto

### scripts

1) Scripts generales que aplican a todo el proyecto:
- main.R: script inicial para la ejecucion de scripts por subtopicos
- aux_functions.R: funciones auxiliares ad_hoc
- consolidar_metadata.R: script de lectura y consolidacion de metadata para analisis y unificacion de fuentes
- insumos.R: descarga general de insumos crudos para reunir en data/_INSUMOS/raw
- crear_directorios.R: replica la esctructura de directorios de Drive de argendata

2) /subtopicos: directorio con un un subdirectorio por subtopico con los scripts particulares de genereción de outputs (1 script x 1 output)


### data

En /data se alojan los datos crudos (raw) y los datos procesados para los graficos (outputs) replicando la esctructura del drive de argendata.

- directorios '/{subtopicos}': cada directorio contiene un directorio '/datasets', el cual contiene dos directorios /outputs y /raw.
- /_INSUMOS reune todos los raw de diferentes subtopicos


