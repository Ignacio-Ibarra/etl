download_file_from_url <- function(url, filename, destdir ){
  
  destfile <- glue::glue("{destdir}/{filename}")
  
  # Desactivo la verificacion de SSL
  options(download.file.method="curl", download.file.extra="-k -L")
  
  download.file(url, destfile = destfile, mode = "wb")
}


subir.cedlas <- function(links.df, fuentes_raw.df , directorio_descarga, code_name ){
  
  for (i in 1:nrow(links.df)){
    url <- links.df$url_path[[i]]
    filename <- links.df$archivo_pagina[[i]]
    nombre <- links.df$titulo[[i]]
    
    download_file_from_url(url = url, filename = filename, destdir = directorio_descarga)
    
    
    id_fuente_raw <- fuentes_raw.df %>% 
      dplyr::filter(path_raw == filename) %>% select(id_fuente) %>% pull()
    
    if (length(id_fuente_raw)>0){
      
      actualizar_fuente_raw(id_fuente = id_fuente_raw, 
                            dir = directorio_descarga, 
                            nombre = glue::glue("Indicadores Sociales de Argentina: {nombre}"))
      
    }else{
      
      
      agregar_fuente_raw(url = url,
                         nombre = glue::glue("Indicadores Sociales de Argentina: {nombre}"),
                         institucion = "Centro de Estudios Distributivos, Laborales y Sociales (CEDLAS)",
                         actualizable = T,
                         fecha_descarga = Sys.Date(),
                         directorio = directorio_descarga,
                         path_raw = filename,
                         script = code_name
      )
      cat(filename, "\n")
    }
    
  }
  
}