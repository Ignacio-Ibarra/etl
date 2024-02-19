#' agregar nueva fuente
#'
#' @param url 
#' @param nombre 
#' @param institucion 
#' @param actualizable 
#' @param fecha_descarga 
#' @param fecha_actualizar 
#' @param path_raw
#' @param script 
#'
#' @return
#' @export
#'
#' @examples
#' 

agregar_fuente <- function(url,
                           nombre,
                           institucion,
                           actualizable,
                           fecha_descarga,
                           fecha_actualizar,
                           path_raw,
                           script) {
  
  
  inputs <- list(
    "url" = url ,
    "nombre" = nombre ,
    "institucion" = institucion,
    "actualizable" = actualizable ,
    "fecha_descarga" = fecha_descarga ,
    "fecha_actualizar" =  fecha_actualizar ,
    "path_raw" = path_raw,
    "script" = script
  )
  
  inputs$fecha_descarga <- as.Date(inputs$fecha_descarga)
  
  inputs$fecha_actualizar <- as.Date(inputs$fecha_actualizar)
  

  stopifnot("No se admiten parametros nulos" = !any(sapply(inputs, is.null)))
  
  stopifnot("No se admiten parametros con NAs" = !any(sapply(inputs, is.na)))
  
  stopifnot("No se admiten parametros con string vacios. Eg: ''" = !any(sapply(inputs, function(x) {as.character(x) == ''})))
  
  stopifnot("param 'actualizable' debe ser logico" = is.logical(inputs$actualizable))
  
  stopifnot("param 'fecha_descarga' debe ser fecha" = !is.na(inputs$fecha_descarga))
  
  stopifnot("param 'fecha_actualizar' debe ser fecha" = !is.na(inputs$fecha_actualizar))
  
  stopifnot("param 'url' debe ser una url valida" =  grepl("^(https|http)://",inputs$url))
  
  
  df <- read_csv("data/_FUENTES/fuentes_lista.csv", col_types = cols("numeric", 
                                                             "character",
                                                             "character",
                                                             "character",
                                                             "logical",
                                                             "Date",
                                                             "Date",
                                                             "character",
                                                             "character"
                                                             ))

  if (nrow(df[df$nombre == inputs$nombre & df$url == inputs$url & df$institucion == inputs$institucion,]) != 0) {
    stop("Ya existe esa combinacion nombre, institucion y url. Verificar si es una posible duplicacion o cambiar de nombre, institucion o url")
  }
  
  if (!file.exists(paste0("data/_FUENTES/raw/", inputs$path_raw))) {
    stop("No se encontro el archivo raw en data/_FUENTES/raw. Guardarlo en la ubicacion antes de continuar")
  }
  
  if (!file.exists(paste0("scripts/fuentes/", inputs$script))) {
    stop("No se encontro el archivo script en scripts/fuentes/. Guardarlo en la ubicacion antes de continuar")
  }
  
  last_id <- last(df$id_fuente)
  
  if (is.na(last_id)) {
    next_id <- 1
  } else {
    next_id <- last_id+1
    
  }
  
  
  inputs$id_fuente <- next_id
  
  print(paste("La fuente quedara registrada con el id:", inputs$id_fuente))
  
  print(df %>% 
          add_row(!!!inputs) %>% tail(1))
  
  guardar <- readline(prompt = "Guardar fuente? y/n")
  
  if (guardar == "y") {
    df %>% 
      add_row(!!!inputs) %>% 
      write_csv("data/_FUENTES/fuentes_lista.csv")
  } else {
    warning("No se guardaron los datos de la fuente en fuentes_lista.csv")
    df %>% 
      add_row(!!!inputs)
  }

  
}
