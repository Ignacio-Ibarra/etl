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
    |-- fuentes_acecon.R
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

## Pasos para trabajar en un subtópico

Al momento de empezar a trabajar en Argendata debemos configurar primero las variables de entorno. Se pueden configurar usando `usethis::edit_r_environ()`

```
USER_GMAIL="mail de fundar"
ARGENDATA_DRIVE="id del drive de argendata"
```

Al inicio de cada sesión de trabajo ejecutar el script "main.R"

```r
source("scripts/main.R")
```

El script carga las librerías mínimas requeridas, define la autenticación de las APIs de google drive y google sheet a partir de la variable de entorno y si es necesario crea la estructura de carpetas para data y scripts.

Luego el trabajo con un subtópico dado se puede definir en 2 partes. Por un lado está la captura y preparación de fuentes de datos, y por el otro lado está la generación de los outputs propiamente dichos.

### Captura - Fuentes Raw

Llamamos fuentes raw a los archivos insumo tal cual se descargan de su origen sin ningún procesamiento por parte nuestra. Cada fuente a utilizar debería tener script en `scripts/descarga_fuentes` que permita la descarga de la misma, dentro de su script de descarga tiene que haber un llamado a la función `agregar_fuente_raw()` que cargue el archivo de la fuente descargada en el drive y la metadata de la misma en la  sheet de fuentes_raw. Prestar especial atención a la información que cargamos al agregar la fuente, en particular al `nombre` y `path_raw` que asignamos. Ambos deben ser únicos pero además hay que tomar la desición si incluir en ellos alguna referencia a la fecha de corte de la fuente o no. 


El estándar es que aquellas fuentes que reciben actualizaciones periodicas regulares (por ej., series trimestrales o anuales de INDEC u otros organismos) no lleven indicación del año en el nombre ni el path. Por ejemplo, se prefiere como path "serie_cgi.xls" en vez de "serie_cgi_01_24.xls" y se prefiere "World Economic Outlook database" en vez de "World Economic Outlook database Octubre 2023". Esto es para que futuras descargas de la misma fuente puedan sobreescribir el mismo archivo en el drive sin romper el flujo definido en scripts. La indicación de fecha en el path o nombre de la fuente solo debería usarse cuando indican que esos datos para esa fecha no son actualizables ni deben ser sobreescritos. Por ejemplo podría ser el caso si cargaramos como fuente "Censo Nacional 2010", como tal la fuente es única y no debería reemplazarse con el censo 2022 que debería cargarse también como "Censo Nacional 2022".

Siempre verificar antes que la fuente no estuviera previamente cargada, puede ser que otro usuario ya haya creado el script de descarga y cargado la fuente en el drive. Se pueden explorar las fuentes cargadas con la función `fuentes_raw()`. 

### Preprocesamiento - Fuentes clean

En general las fuentes deben ser preprocesadas para su uso, de ese procesamiento salen lo que llamamos fuentes clean. Cada fuente clean debe ser generada con un script en `scripts/limpieza_fuentes`. La fuente clean debería cumplir al menos:

- nombres de columnas estén en minusculas, sin espacios ni caracteres especiales. Pueden usar la función `janitor::clean_names()` a tal fin. 
- limpieza de columnas o filas enteramente vacías, o que solo tienen aclaraciones o notas provenientes de la importación desde excel
- ser exportada en csv utf-8
- en lo posible formato long preferido

Se espera que cada fuente clean consista en una sola tabla cuando se trate de un dato tabular. Por ej., a partir de una fuente raw como "serie_cgi" que es un excel con múltiples hojas se espera que pasemos a una fuente clean para cada hoja que se requiera de "serie_cgi".

En los casos de fuentes clean para las que tenemos la expectativa de que serán actualizadas con regularidad (al menos anual) es importante que el código tenga la flexibilidad para ejecutarse sin grandes ajustes con la próxima versión de la fuente raw que se actualice.

Por ejemplo, si tenemos un pequeño dataset de copas del mundo con formato wide que seguirá agregando datos a lo ancho, con una nueva columna para cada nuevo mundial :


```r
datos <- tibble(
       "iso" = c("ARG", "FRA"),
       "2006" = c(2,1),
       "2010" = c(2,1),
       "2014" = c(2,1),
       "2018" = c(2,2)
       )

```

Una de las cosas que deberíamos hacer pivotear a lo largo esta fuente de datos, pero para ello se prefiere el código:

```r
datos %>% 
  pivot_longer(cols = -iso,
               values_to = "copas", names_to = "anio")
```

En vez del código:

````r
datos %>% 
  pivot_longer(cols = c(`2006`, `2010`, `2014`, `2018`), 
               values_to = "copas", names_to = "anio")

```

Ya que el primer caso es agnóstico respecto a las columnas que refieren a los años y que irán variando con el tiempo y aún servirá cuando la fuente sea actualizada:

```r
datos <- tibble(
  "iso" = c("ARG", "FRA"),
  "2006" = c(2,1),
  "2010" = c(2,1),
  "2014" = c(2,1),
  "2018" = c(2,2),
  "2022" = c(3,2)
)

datos %>% 
  pivot_longer(cols = -iso,
               values_to = "copas", names_to = "anio")
               
```

### Trabajo con Subtópicos

Cada subtópico tiene su propia carpeta en `/scripts`. La estructura de carpetas se crea con la rutina de `scripts/utils/crear_directorios.R` que se ejecuta al correr `main.R`. Dentro de cada carpeta de un subtópico en `/scripts` debe existir un archivo `fuentes_{subtopico}.R` (por ej.: 'fuentes_acecon.R') y uno o más scripts de creación de outputs.

El archivo `fuentes_{subtopico}.R` debe listar las descargas de fuentes desde el drive de Argendata que son necesarias para el subtópico. Por ej. para ACECON sería:

```r
descarga_fuentes_clean(id_fuente = 1)
descarga_fuentes_clean(id_fuente = 2)
descarga_fuentes_clean(id_fuente = 4)
descarga_fuentes_clean(id_fuente = 6)

```

Para descargar una fuente raw o fuente clean se debería incluir siempre un llamado a las funciones `descargar_fuente_clean()` y `descargar_fuente_raw()`. Por eso todas las fuentes usadas deben estar registradas como describió más arriba.

La descarga de fuentes se plantea así para evitar volver a descargar la misma fuente en cada script particular de creación de outputs pero poder realizarla en bloque para cada ejecución completa de un subtópico.

Para los scripts de creación de outputs se debe generar un script para cada output dentro de su carpeta de subtopico. Los scripts se guardan en `scripts/subtopicos/{nombre del subtopico}` y deberían estar nombrados de forma que haga facil reconocer qué output generan. Se puede usar la función `argendataR::script_subtopico` para crear un archivo .R con un estructura básica de script de creación de outputs. A esta función hay que pasarle el path completo de donde queremos crear el archivo y su nombre.

Breve esquema de un script de generacion de output

```r
# dataset:
# librerias ---------

# lectura de datos ---------
# los datos utilizados deben estar cargados como fuentes del proyecto en fuentes_raw() o fuentes_clean()
# todos los insumos a utilizar deben leerse de /data/_FUENTES/clean o /data/_FUENTES/raw

# parametros generales ---------
# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

# procesamiento ---------

# guardar output ---------
# usar write_output con exportar = T para generar la salida
# write_output()

```

*Consideraciones generales*

En el caso de que sea necesario más de un script para crear un output dado, los scripts deberían tener definida la secuencia lógica de forma inherente, es decir, si se necesita que se ejecuten primero el script A y luego el script B para generar el output 1, es preciso que el propio script B llame la secuencia del script A. Esto se puede hacer mediante el uso de `source()` o mejor aún definiendo los procedimientos como funciones. 

Dado que se espera poder actualizar los outputs sin grandes modificaciones del código se requiere que el procesamiento sea en la mayor medida posible agnóstico respecto de nombres de columnas que puedan variar al actualizar las fuentes (ver ejemplo de codigo anterior).

Las variables que sirvan para parametrizar el procedimiento de generación del output (por ej.: fechas de corte para filtros, o listas de codigos para filtros de paises de interés, etc.) deberían aparecer definidas al principio del script.







