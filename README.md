# argendata-etl

Este proyecto busca sistematizar en scripts de R o Python los procesos de generación de los outputs utilizados en argendata de la forma más reproducible posible.

## Flujo de trabajo

### main.R

Este script es el punto de entrada del proyecto. Por el momento solo realiza la carga de librerías indispensables para  el proyecto y la autneticación con el drive de Argendata.

El flujo de trabajo requiere que estén definidas dos variables de entorno en el archivo .Renviron de la siguiente manera:

```
USER_GMAIL="mail de fundar"
ARGENDATA_DRIVE="id del drive de argendata
```



### Fuentes

*Fuentes Raw*

Fuentes raw son las fuentes en su formato original tal cual se descargan de su origen. Cada fuente nueva que se incorpora al proyecto debe tener un script de descarga en /scripts/descarga_fuentes/.

Los scripts de descarga deben guardar el archivo de la fuente raw en `/data/_FUENTES/raw/`. Cada script de descarga debe incluir un llamado a `argendatar::agregar_fuente_raw()` que se ejecuta por única vez la primera vez que se descarga la fuente, y un llamado a `argendatar::actualizar_fuente_raw()` que se ejecuta cada vez que se descarga neuvamente la fuente ya registrada. Estas funciones registran o actualizan los datos en una googlesheet de fuentes raw (consultar con `argendatar::fuentes_raw()`) y además suben el archivo al drive de argendata en `BASES DE DATOS/Fuentes/raw/`. 

Es posible descargar fuentes raw ya registradas en el drive de Argendata con `argendatar::descargar_fuente_raw()` usando su número de id como figura en `argendatar::fuentes_raw()`.

*Fuentes Clean*

Fuentes clean son las fuentes ya preprocesadas para el uso en análisis y generación de outputs. En general deberían ser csv, en UTF-8, con la data en formato long y nombres de columna limpios (sin espacios, caracteres extraños, en minuscula). A partir de un mismo archivo de fuente raw se pueden crear multiples fuentes clean. Por ej.: a partir del archivo "serie_cgi_01_24.xls" (serie de VAB e insumo de mano de obra por sector) de INDEC se debería crear un csv con la versión clean de cada una de las hojas que contiene ese xlsx para facilitar su integración en proceso. Cada fuente clean nueva que se incorpora al proyecto debe tener un script de creación en /scripts/limpieza_fuentes/.

Los scripts de limpieza deben guardar el archivo de la fuente clean en `/data/_FUENTES/clean/`. Cada script debe incluir un llamado a `argendatar::agregar_fuente_raw()` para registrar cada fuente clean -al igual que en el caso anterior esta funcion se ejecuta una unica vez al registrar por primera vez esta fuente clean-, y un llamado a `argendatar::actualizar_fuente_clean()` que se ejecuta cada vez que se actualiza nuevamente la fuente ya registrada. Estas funciones registran o actualizan los datos en una googlesheet de fuentes clean (consultar con `argendatar::fuentes_clean()`) y además suben el archivo al drive de argendata en `BASES DE DATOS/Fuentes/clean/`. 

Es posible descargar fuentes clean ya registradas en el drive de Argendata con `argendatar::descargar_fuente_clean()` usando su número de id como figura en `argendatar::fuentes_clean()`.


### Outputs

Cada subtopico debe tener una carpeta dentro de `scripts/subtopicos` con el nombre de 6 letras en mayuscula que le corresponde (ver [Estructura](#estructura-del-proyecto)). Dentro de esa carpeta se deben guardar los scripts de generación de outputs. Cada script debe llevar el nombre del output que genera.

Los scripts deben usar fuentes clean ya registradas en el drive de Argendata, el output debe cumplir con los lineamientos de para Argendata. Se espera que cada script de output pueda ser ejecutado con independencia de los demás.

Cada script debe guardar el output en formato en una carpeta con el nombre del subtopico, por ej.: `/data/ACECON/`. Para guardar el output el script debe utilizar la función `argendatar::write_output()` usando el parametro `exportar = T`, esta función escribe un json que contiene la data y la metadata del output, y además escribe un csv usando el estándar de Argendata.

Los json y csv generados como outputs deben ser pusheados al repositorio hasta su validación para carga directa en el drive de Argendata.


## Estructura

```
/scripts 
|-- /utils
|-- /descarga_fuentes
  |-- descarga_weo_imf.R
|-- /limpieza_fuentes
  |-- limpieza_weo_imf.R
|-- /subtopicos
  |-- /ACECON
    |-- output1.R
  |-- /SUBTOP
|-- main.R
/data
|-- /_FUENTES
  |-- /raw
  |-- /clean
|-- /ACECON
  |-- output1.csv
|-- /SUBTOP
```

Para replicar la estructura de carpetas de subtopicos del drive dentro de /data y /scripts programaticamente se puede usar el script `crear_directorios.R` que aprovecha la función `argendatar::subtopicos()`.

