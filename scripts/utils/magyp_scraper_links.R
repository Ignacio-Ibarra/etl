require(httr)
library(urltools)

corregir_url <- function(url) {
  
  # Separa la URL en sus componentes para trabajar con ella
  componentes <- url_parse(url)
  
  # Codifica caracteres especiales solo en el path
  componentes$path <- gsub(" ", "%20", componentes$path)  # Codifica espacios
  componentes$path <- URLencode(componentes$path, reserved = TRUE)
  
  # Reconstruye la URL a partir de sus componentes
  url_corregida <- url_compose(componentes)
  
  return(url_corregida)
}


MAGYP.extraer_links_informes_bovinos <- function(pattern){
  
  # URL de la página que quieres scrapear
  base_url <- "https://www.magyp.gob.ar/sitio/areas/bovinos/informacion_interes/informes"
  
  
  url <- paste0(base_url, "/index.php")
  
  web_content <- rvest::read_html(url)
  
  # Extraer los nodos <a> con href que coincidan con el patrón
  target_a_nodes <- web_content %>% 
    html_nodes('a') %>% 
    keep(~ grepl(pattern, html_attr(., "href"), ignore.case = TRUE))
  
  planillas <- target_a_nodes %>% 
    map_df(~ {
      href <- html_attr(., "href")
      texto <- html_text(.)
      data.frame(
        texto = texto,
        link = file.path(base_url, href) %>% corregir_url(.),
        stringsAsFactors = FALSE
      )
    }) %>% 
    dplyr::filter(texto!="")
  
  if (nrow(planillas) == 0){
    stop("No se han encontrado planillas de cálculo para descargar en la página")
  }
  
  return(planillas)
  
}


MAGYP.extraer_links_datos_abiertos <- function(page_suffix, h3_target ){
  
  url_base <- "https://datos.magyp.gob.ar/dataset/"
  
  page_url <- paste0(url_base,page_suffix)
  
  response <- GET(page_url, 
                  config = config(ssl_verifypeer = FALSE),
                  add_headers(
                    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36",
                    `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
                    `Accept-Language` = "en-US,en;q=0.5",
                    `Accept-Encoding` = "gzip, deflate, br",
                    `Connection` = "keep-alive",
                    `Upgrade-Insecure-Requests` = "1",
                    `Sec-Fetch-Dest` = "document",
                    `Sec-Fetch-Mode` = "navigate",
                    `Sec-Fetch-Site` = "none",
                    `Sec-Fetch-User` = "?1"
                  )
                )
  
  # Verificar el contenido de la respuesta
  web_content <- httr::content(response, "text") %>% 
    rvest::read_html(.)
  
  # Extraer todos los div con clase "pkg-container"
  pkg_containers <- web_content %>%
    html_nodes(".pkg-container")
  
  # Extraer los enlaces "DESCARGAR" y sus respectivos h3
  links_df <- pkg_containers %>%
    # Iterar sobre cada div "pkg-container"
    purrr::map_df(~ {
      # Extraer los enlaces "DESCARGAR" dentro del div.pkg-actions
      href <- .x %>%
        html_nodes(".pkg-actions a") %>%
        purrr::keep(~ html_text(.x, trim = TRUE) == "DESCARGAR") %>%
        html_attr("href")
      
      # Extraer el título h3 dentro del div.package-info
      h3_text <- .x %>%
        html_nodes(".package-info h3") %>%
        html_text(trim = TRUE)
      
      paragraph_text <- .x %>%
        html_nodes(".package-info p") %>%
        html_text(trim = TRUE)
      
      # Devolver los valores en un data.frame
      tibble(
        href = href,
        h3 = h3_text,
        paragraph = paragraph_text
        )
    })
  
  filtered_links <- links_df %>% 
    dplyr::filter(h3 == h3_target)
  
  # Verificar que 'url' sea un vector de longitud 1 y no vacío
  if (!(nrow(filtered_links) == 1)) {
    stop("Error: Verificar los parámetros 'page_suffix' o 'h3_target' no se han pasado correctamente")
  }
  
  # Devuelve una lista con la URL y el texto asociado
  result <- list(url = filtered_links$href, title = filtered_links$h3, text = filtered_links$paragraph)
  
  return(result)
}


MAGYP.extraer_links_pesca_maritima_desembarques <- function(pattern = ".*(\\.zip$|\\.rar$)"){
  
  url_consulta <- "https://www.magyp.gob.ar/sitio/areas/pesca_maritima/desembarques"
  
  # Obtiene el contenido de la página web
  web_content <- read_html(url_consulta)
  
  # Obtiene los href y el texto de cada enlace
  links <- web_content %>%
    html_nodes("a") %>%
    html_attr("href")
  
  text_links <- web_content %>%
    html_nodes("a") %>%
    html_text(trim = TRUE)
  
  # Filtra los links que contienen la URL base y aplican el pattern
  filtered_links <- links[grepl(pattern, links)] %>% 
    gsub("//", "/", .) %>% 
    file.path(url_consulta,.) %>% str_replace_all(., " ","%20")
  filtered_texts <- text_links[grepl(pattern, links)]
  
  resultado <- data.frame(nombre = filtered_texts, url = filtered_links) %>% dplyr::filter(nombre != "") %>% 
    mutate( anio = as.integer(str_extract(nombre, ".*(\\d{4}).*", group = 1)))
  
  return(resultado)
  
}


MAGYP.obtener_links_desembarques_especificos <- function(patron_especifico = NULL){
  
  
  all_links_df <- MAGYP.extraer_links_pesca_maritima_desembarques(pattern = "(.\\.zip$|\\.rar$)")
  
  
  links_especificos <- all_links_df %>% dplyr::filter(grepl(patron_especifico, url))
  
  
  if (nrow(links_especificos)>0){
    return(links_especificos)
  }else{
      stop("El patrón ingresado no matchea con ninguno de los links disponibles")
    }
}


MAGYP.descargar_archivo <- function(url, filename = NULL){
  
  if (is.null(filename)){
  
  valid_years <- 1989:year(Sys.Date())
  base <- basename(url)
  ext <- tools::file_ext(base)
  anio <- base %>% str_replace_all(., "\\%20", "_") %>% str_extract(., ".*_(\\d{4}).*", group = 1) %>% purrr::keep(., ~(as.integer(.x) %in% valid_years))
  
  if (is.na(anio) || length(anio) == 0){
    stop("No se logró detectar el año")
  }
  filename <- glue::glue("archivo_{anio}.{ext}")
  }
  
  
  destfile <- file.path(tempdir(), filename)
  
  download.file(url, destfile = destfile, mode = "wb")
  cat(glue::glue("Se descargó {filename}\n\n"))
  
  return(destfile)
  
  }


MAGYP.descomprimir_archivo <- function(zipfilepath, patron_archivo = NULL, exdir = tempdir()){
  
  files_df <- archive::archive(zipfilepath)
  
  if (is.null(patron_archivo)){
    inner_file <- files_df %>% arrange(-size) %>% slice_head(n = 1) %>% pull(path)
  
  }else{
    
    inner_file <- files_df %>% dplyr::filter(grepl(patron_archivo, path)) %>% pull(path)  
    
  }
    
  archive::archive_extract(zipfilepath, files = inner_file, dir = exdir)
  
  curr_path <- glue::glue("{exdir}/{inner_file}")
  base <- basename(curr_path)
  ext <- tools::file_ext(base)
  anio <- str_extract(base, "\\d{4}(?=(\\D*$|$))")
  
  new_path <- glue::glue("{exdir}/inner_file_{anio}.{ext}")
  
  file.rename(curr_path, new_path)
  
  return(new_path)
}


MAGYP.leer_pestanias <- function(path, patron_pestania = NULL){
  
  
  sheet_names <- readxl::excel_sheets(path)
  
  if (!is.null(patron_pestania)){
    
    sheet_names <- sheet_names %>% purrr::keep(., ~all(grepl(patron_pestania, .x)))
  
    }
  
  if (!is.null(patron_pestania) && length(sheet_names) == 0){
   
   str_err <- glue::glue("No se encontró una pestaña con el patrón '{patron_pestania}' en el archivo {basename(path)}")
   stop(str_err)
  }
  
 worksheet_data <- list()
 
 for (sh in sheet_names){
   
   worksheet_data[[sh]] <- readxl::read_excel(path, sheet = sh, col_names = F)
   
 } 

  
 return(worksheet_data)  
  
}


MAGYP.obtener_datos <- function(target = "desembarque", anios = NULL, patron_pestania = ".*", patron_archivo = NULL){
  
  all_links <- MAGYP.extraer_links_pesca_maritima_desembarques()
  
  valid_years <- 1989:year(Sys.Date())
  
  if (is.null(anios)){
    anios =  valid_years
  }else{ 
      anios = intersect(valid_years, anios)
      }
  
  targets_posibles <- c("desembarque","cmp", "puerto_flota_especie_mes")
  
  if (!(target %in% targets_posibles)){
    
    str_err <- paste0(targets_posibles, collapse = ", ") %>% glue::glue("Target inválido, elegir entre '{.}'")
    
    stop(str_err)
  }

  if (target == "desembarque"){
    
    
    links_especificos <- MAGYP.obtener_links_desembarques_especificos(patron_especifico = ".*(archivos.*|.*Desembarques.*)")
    
    
  }
  
  
  if (target == "puerto_flota_especie_mes"){
    
    links_especificos <- MAGYP.obtener_links_desembarques_especificos(patron_especifico = ".*uerto.*lota.*specie.*mes.*\\.zip")
  }
  
  
  if (target == "cmp"){
    
    links_especificos <- MAGYP.obtener_links_desembarques_especificos(patron_especifico = ".*apturas.*ximas.*ermisible.*\\.zip")
    
  }
  
  # filtro los anios requeridos
  links_descarga <- data.frame(anio = anios) %>% 
    left_join(links_especificos, join_by(anio)) 
  
  
  if (sum(is.na(links_descarga$nombre))>0){stop("Hay años que no están disponibles para dicho target, seleccionar años correctos")}
  
  
  links_descarga <- links_descarga %>% pull(url)
  
  
  descargas <- purrr::map_chr(links_descarga, function(url){
    MAGYP.descargar_archivo(url = url)
    })
  
  archivos_descomprimidos <- purrr::map_chr(descargas, function(zipfilepath){
    MAGYP.descomprimir_archivo(zipfilepath = zipfilepath, 
                               patron_archivo = patron_archivo)
    })
  
  
  lista_data <- list()
  
  for (i in 1:length(archivos_descomprimidos)){
    
    f <- archivos_descomprimidos[i]
    
    id <- basename(f)
    
    lista_data[[id]] <- MAGYP.leer_pestanias(path = f, patron_pestania = patron_pestania)
    
    cat("ok, file: ",id, "\n\n")
    
  }
  
  
  
  
  return(lista_data)
  
}


# Desde 1989 a 2012 cada rar viene con un solo archivo y 
# solo hay data agregada, por eso hay que elegir pestaña. 
# eg Para especie por mes "esp_mes.*"
# Solo hay target desembarque. 
# lista_raw_2012 <- MAGYP.obtener_datos(target = "desembarque", anios = 1989:2012, patron_archivo = NULL, patron_pestania = "esp_mes.*" )


# Desde 2013 a 2021 cada zip viene con dos archivos adentros.
# Existe la data desagregada por puerto flota especie mes, por eso hay que elegir patron_archivo
# En caso de elegir la data desagregada no hace falta pasarle patron_pestania. 
# Ademas hay data data agregada, por eso hay que elegir patron_pestania
# eg Para especie por mes "esp_mes.*"
# Solo hay target desembarque. 
# lista_raw_2021 <- MAGYP.obtener_datos(target = "desembarque", anios = 2013:2021, patron_archivo = ".*Por puerto.*", patron_pestania = NULL)


# Desde 2022 en adelante hay más de un tarquet
# Para data agregada elegir target == 'desembarque',
# Para data desagregada elegir target == "puerto_flota_especie_mes",
# Para data sobre Capacidad Máxima Permisible target == "cmp",
# En ese caso no es necesario pasar patron_archivo y el patron_pestania tampoco. 
# lista_raw_2024 <- MAGYP.obtener_datos(target = "puerto_flota_especie_mes", anios = 2022:2024, patron_archivo = NULL, patron_pestania = NULL)

