# ARGENTADATA
## argendata-etl

Este proyecto busca sistematizar en scripts de R o Python los procesos de generación de los outputs utilizados en argendata de la forma más reproducible posible.

## estructura del proyecto

- .Renviron con las variables de entorno del proyecto

### /scripts

Scripts generales que aplican a todo el proyecto:
- main.R: script inicial para la ejecucion de scripts por subtopicos
- aux_functions.R: funciones auxiliares ad_hoc
- descarga_metadata.R: script de lectura y consolidacion de metadata para analisis y unificacion de fuentes
- crear_directorios.R: replica la esctructura de directorios de Drive de argendata
- /insumos:
-- descarga_insumos.R: compilación de descargas o links a apis usadas en el proyecto
-- limpieza_*: scripts de limpieza de un insumo en particular
- /subtopicos:
-- '/{subtopicos}': cada directorio contiene un directorio contiene los scripts de generación de outputs, cada script genera 1 solo output.


### /data

En /data se alojan los datos crudos (raw) y los datos procesados para los graficos (outputs) replicando la esctructura del drive de argendata.

- directorios '/{subtopicos}': cada directorio contiene un directorio '/datasets', el cual contiene dos directorios /outputs y /raw.
- /_INSUMOS reune todas fuentes de datos en versión raw de diferentes subtopicos y una versión tidy de cada uno de ellos también.


