# Wrapper de OECD API 
# Docs https://gitlab.algobank.oecd.org/public-documentation/dotstat-migration/-/raw/main/OECD_Data_API_documentation.pdf

# Hay que entrar al Data Explorer https://data-explorer.oecd.org/ 
# buscar lo que se quiere descargar y extraer la URL de la solapa Devloper API

# el siguiente codigo toma la URL y descarga el archivo como csv


library(httr)
library(readr)



oecd_api.download_data_from_url <- function(url, output_path){
  
  
  # Realiza la petición HTTP solicitando los datos en formato SDMX-CSV v1
  response <- GET(
    url,
    add_headers(
      Accept = "application/vnd.sdmx.data+csv; charset=utf-8; labels=both"
    )
  )
  
  # Verifica si la petición fue exitosa
  if (status_code(response) == 200) {
    # Especifica el nombre del archivo CSV donde se guardarán los datos
    
    # Guarda el contenido de la respuesta en un archivo CSV
    writeBin(content(response, "raw"), output_path)
    
    # Lee el archivo CSV en un dataframe
    datos <- read_delim(output_path, delim = ";")
    
  } else {
    # Si hubo un error, imprime el código de estado
    print(paste("Error:", status_code(response)))
  }
  
  
  
}





