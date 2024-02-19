#' agregar nueva fuente
#'
#' @param id_fuente 
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

actualizar_fuente <- function(id_fuente,
                              url = NULL,
                              nombre = NULL,
                              institucion  = NULL,
                              actualizable = NULL,
                              fecha_descarga= NULL,
                              fecha_actualizar = NULL,
                              path_raw = NULL,
                              script = NULL) {
  
  
  stopifnot("id_fuente es nulo" = !is.null(id_fuente))
  
 
  
  inputs <- list(
    "id_fuente" = id_fuente,
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
  
  
  inputs <- inputs[!sapply(inputs, is.null)]
  inputs <- inputs[!sapply(inputs, function(x) {length(x) == 0})]
  
  
  df <- read_csv(
    "data/_FUENTES/fuentes_lista.csv",
    col_types = cols(
      "numeric",
      "character",
      "character",
      "character",
      "logical",
      "Date",
      "Date",
      "character",
      "character"
    )
  )

  print("Registro previo")
  print( df[df$id_fuente == inputs$id_fuente,])
    
  for (i in names(inputs)) {
    print( str(inputs[i]))
    df[df$id_fuente == inputs$id_fuente , i] <-  inputs[i]
    
  }
  
  print("Registro actualizado")
  
  print( df[df$id_fuente == inputs$id_fuente ,])
  
  df %>%
    write_csv("data/_FUENTES/fuentes_lista.csv")

  
}
