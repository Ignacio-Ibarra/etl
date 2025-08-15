#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

library(DBI)
library(duckdb)

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

fecha_actualizar <- "Sin informacion"

source("scripts/utils/wrapper_presupuesto_catalogo.R")


# Normalizar nombres de columnas
normalize_names <- function(n) {
  n %>%
    trimws() %>%                 # quitar espacios al inicio/fin
    janitor::make_clean_names()  # minúsculas, snake_case
}



# Función para agregar columnas si no existen
add_columns_if_missing_dt <- function(DT, cols_with_values) {
  # cols_with_values: lista nombrada, ej: list(c = NA, d = 0)
  for(colname in names(cols_with_values)) {
    if(!colname %in% colnames(DT)) {
      DT[, (colname) := cols_with_values[[colname]]]
    }
  }
  invisible(DT)  
}


limpiar_datos <- function(datos, ejercicios, completar_cols) {
  setDT(datos)  # convertir a data.table si no lo es
  
  # Eliminar columnas *_desc
  cols_keep <- grep("_desc$", names(datos), invert = TRUE, value = TRUE)
  datos <- datos[, ..cols_keep]
  
  # Identificar columnas credito_*
  cols_credito <- grep("^credito_", names(datos), value = TRUE)
  
  # Convertir valores en columnas credito_* a numérico con punto decimal
  datos[, (cols_credito) := lapply(.SD, function(x) as.numeric(str_replace(x, ",", "."))),
        .SDcols = cols_credito]
  
  # Procesar fecha y año
  datos[, ultima_actualizacion_fecha := dmy(
    str_remove(ultima_actualizacion_fecha, ".*: "),
    locale = "es_ES.UTF-8"
  )]
  
  add_columns_if_missing_dt(datos, completar_cols)
  
  datos <- datos[, lapply(.SD, as.character)]
  
  return(datos[])
}



# Definir la función para crear la base de datos
upsert_duckdb <- function(df, con, table_name) {
  
      
      if (!dbExistsTable(con, table_name)) {
        dbWriteTable(con, table_name, df, overwrite = TRUE)
      } else {
        
        # Si la tabla ya existe, agregar los datos
        dbAppendTable(con, table_name, df)
      }
    cat(table_name, "...upsert realizado\n\n")
}





compilar_presupuesto <- function(df_urls, con, table_name) { 
  
  if(nrow(df_urls) == 0){stop("No hay recursos para descargar")}
  
  df_urls %>%
    purrr::pmap(function(ejercicios, urlArchivo) {
      datos <- PRESUPUESTO.get_data(ejercicios, urlArchivo) 
      
      df_clean <- limpiar_datos(datos, ejercicios, list(codigo_bapin_id = 0, prestamo_externo_id = 0))
      
      message("Se obtuvo la data limpia del ejercicio: ", ejercicios,". con dimensiones: ", dim(df_clean)[1]," ",dim(df_clean)[2] )
      
      if (!is.null(datos)) {
        upsert_duckdb(df_clean, con, table_name)
        message("Ejercicio ", ejercicios, " procesado y agregado a la tabla de DuckDB")
      }
      invisible(NULL)
    })
}

catalogo_completo <- PRESUPUESTO.get_catalog()

titulo_recurso <- "Presupuesto de gastos y su ejecución detallada - agrupación anual"

recursos<- catalogo_completo %>% 
  dplyr::filter(nombreRecurso == titulo_recurso, periodicidad == "eventual") %>% 
  mutate(ejercicios = as.integer(ejercicios)) %>% 
  arrange(ejercicios) %>% 
  select(ejercicios, urlArchivo)

anio_min <- min(recursos$ejercicios)
anio_max <- max(recursos$ejercicios)

url <- "https://www.presupuestoabierto.gob.ar/sici/datos-abiertos"

nombre <- glue::glue("{titulo_recurso} - ({anio_min}-{anio_max})")

institucion <- "Ministerio de Economía. Secretaría de Hacienda. Subsecretaría de Presupuesto."

download_filename <- glue::glue("mecon_{titulo_recurso %>% janitor::make_clean_names()}.duckdb")

destfile <- glue::glue("{tempdir()}/{download_filename}")

busqueda <- fuentes_raw() %>% 
  dplyr::filter(path_raw == download_filename) %>% 
  select(id_fuente, path_raw)

if (nrow(busqueda) == 1){
  
  anios_descargados <- argendataR::get_raw_path(busqueda$id_fuente) %>% 
    read_csv(., col_select = 'anio_id') %>% 
    pull() %>% unique()
  
  descarga_recursos <- recursos %>% 
    dplyr::filter(!(ejercicio %in% anios_descargados))
  
  
}else{
  
  descarga_recursos <- copy(recursos)
  
}


con <- dbConnect(duckdb::duckdb(), dbdir = destfile, read_only = FALSE)

compilar_presupuesto(descarga_recursos, con, "presupuesto")
dbDisconnect(con, shutdown = TRUE)



# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 430,
                      url = url, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)
