library(rvest)
library(httr)


afip_anuario_estadistico.extraer_links_afip = function(url_base, page_url){
  
  # Obtiene el contenido de la página web
  web_content <- read_html(page_url)
  
  # Me quedo con los href
  links <- web_content %>% 
    html_nodes("a") %>% 
    html_attr("href")
  
  # Filtra los links que contienen la URL base
  urls <- links[grepl(url_base, links)]
  
  # Extrae los años de los links
  anios <- as.numeric(sub(".*(\\d{4})\\.zip$", "\\1", urls))
  
  
  # Me quedo con 2014 nomás. 
  datos <- data.frame(anio = anios, url_name = urls)
  
  return(datos)
}


afip_anuario_estadistico.descargar_zip = function(anio, url){
  
  
  # Armo destfile
  carpeta_unzip <- glue::glue("estadisticasTributarias{anio}")
  archivo_zip <- glue::glue("{carpeta_unzip}.zip")
  destfile <- glue::glue("{tempdir()}/{archivo_zip}")
  
  
  tryCatch(
    {
      # Intentamos ejecutar descarga
      download.file(url = url, destfile = destfile)
      cat_str <- glue::glue("Descargó el archivo del año {anio}\n\n")
      cat(cat_str)
      return(destfile)
    },
    error = function(e) {
      # Si ocurre un error, lo capturamos y mostramos un mensaje
      message("Error: ", conditionMessage(e))
      return(NA)
    }
  )
  
  
}

afip_anuario_estadistico.unzip_to_folder = function(anio, destfile){
  
  carpeta_unzip <- glue::glue("estadisticasTributarias{anio}")
  archivo_zip <- glue::glue("{carpeta_unzip}.zip")
  exdir <- glue::glue("{tempdir()}/{carpeta_unzip}")
  
  tryCatch(
    {
      # Intentamos ejecutar descarga
      unzip(destfile, exdir = exdir)
      cat_str <- glue::glue("Descrompirmió archivo zip del año: {anio}\n\n")
      cat(cat_str)
      return(exdir)
      
    },
    error = function(e) {
      # Si ocurre un error, lo capturamos y mostramos un mensaje
      message("Error: ", conditionMessage(e))
      return(NA)
    }
  )
  
}

afip_anuario_estadistico.search_file = function(anio, unzipped_folder, formatos_archivos =c("2.1.1.4.xls", "2.1.1.4.htm", "2.1.1.4.xlsx")){
  
  list_archivos <- list.files(unzipped_folder, recursive = T, full.names = TRUE)
  
  # Filtra los archivos que coincidan con los nombres definidos
  archivo_path <- list_archivos[basename(list_archivos) %in% formatos_archivos]
  archivo_path <- ifelse(length(archivo_path) == 0, NA, archivo_path[1])
  cat_str <- ifelse(!is.na(archivo_path), glue::glue("Descrompirmió archivo {anio}\n\n"), glue::glue("No se encontró archivo con los formatos declarados para el año {anio}\n\n"))
  
  cat(cat_str)
  
  return(archivo_path)  
  
}