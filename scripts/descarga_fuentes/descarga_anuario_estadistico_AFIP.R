#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

source("scripts/utils/afip_anuario_estadistico_scraper.R")


# Define la URL base y la URL de la página a consultar
url_base <- "https://contenidos.afip.gob.ar/institucional/estudios/archivos/estadisticasTributarias/"
page_url <- "https://www.afip.gob.ar/estudios/anuario-estadisticas-tributarias/"

anios_elegidos <- 2009:2022

# Extraigo links y almaceno datos en df
datos <- afip_anuario_estadistico.extraer_links_afip(url_base = url_base, page_url = page_url)

# Descargo archivos zip y almaceno ruta de descarga
datos$ruta_zip = pmap_chr(list(datos$anio, datos$url_name), ~ afip_anuario_estadistico.descargar_zip(..1, ..2))

# Descomprimo zip y almaceno ruta descarga de folder
datos$unzipped_folder = pmap_chr(list(datos$anio, datos$ruta_zip), ~ afip_anuario_estadistico.unzip_to_folder(..1, ..2))

# Busco en la carpeta el archivo que coincide con los formatos posibles. 
datos$archivo_path = pmap_chr(list(datos$anio, datos$unzipped_folder), ~ afip_anuario_estadistico.search_file(..1, ..2, formatos_archivos = c("2.1.1.4.xls", "2.1.1.4.htm", "2.1.1.4.xlsx")))


# Función para agregar o actualizar

tematica_archivo <- "Impuesto al Valor Agregado, presentaciones, ventas y exportaciones por actividad económica"

afip_anuario_estadistico.a_fuente_raw(datos = datos, code_name = code_name, tematica_archivo = tematica_archivo, actualizar = T)
