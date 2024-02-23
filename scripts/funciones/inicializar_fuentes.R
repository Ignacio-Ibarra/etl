#' Inicializar tabla de fuentes
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 

inicializar_fuentes <- function() {
  
  df <- data.frame(
    "id_fuente" = numeric(),
    "url" = character(),
    "nombre" = character(),
    "institucion" = character(),
    "actualizable" = logical() ,
    "fecha_descarga" = as.Date(NULL),
    "fecha_actualizar" =  as.Date(NULL),
    "path_raw" = character(),
    "script" = character()
  )
  
  write_csv(df, "data/_FUENTES/fuentes_lista.csv", na = "", eol = "\n")
  cat("Tabla fuentes en data/_FUENTES/fuentes_lista.csv")
}
