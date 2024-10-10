#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

source("scripts/utils/afip_anuario_estadistico_scraper.R")


# Define la URL base y la URL de la página a consultar
url_base <- "https://contenidos.afip.gob.ar/institucional/estudios/archivos/estadisticasTributarias/"
page_url <- "https://www.afip.gob.ar/estudios/anuario-estadisticas-tributarias/"


# Extraigo links y almaceno datos en df
datos <- afip_anuario_estadistico.extraer_links_afip(url_base = url_base, page_url = page_url)

# Descargo archivos zip y almaceno ruta de descarga
datos$ruta_zip = pmap_chr(list(datos$anio, datos$url_name), ~ afip_anuario_estadistico.descargar_zip(..1, ..2))

# Descomprimo zip y almaceno ruta descarga de folder
datos$unzipped_folder = pmap_chr(list(datos$anio, datos$ruta_zip), ~ afip_anuario_estadistico.unzip_to_folder(..1, ..2))

# Busco en la carpeta el archivo que coincide con los formatos posibles. 
datos$archivo_path = pmap_chr(list(datos$anio, datos$unzipped_folder), ~ afip_anuario_estadistico.search_file(..1, ..2))

