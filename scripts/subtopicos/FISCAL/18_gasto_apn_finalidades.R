#Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

library(DBI)
library(duckdb)

# Defino variables
subtopico <- "FISCAL"
output_name <- "gasto_apn_finalidades.csv"
analista <- "María Fernanda Villafañe & Micaela Fernandez Erlauer"

fuente1 <- 'R430C0' # Presupuesto Público
fuente2 <- 'R38C12' # Cuentas Nacionales. Agregados macroeconómicos (PIB). Series trimestrales de oferta y demanda globales. Años 2004-2024 - cuadro 8

safe_db_connect <- function(db_path, read_only = FALSE) {
  tryCatch({
    dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = read_only)
  }, error = function(e) {
    msg <- conditionMessage(e)
    pid <- as.integer(sub(".*PID ([0-9]+)\\).*", "\\1", msg))
    
    if (!is.na(pid)) {
      message("🔍 Proceso que mantiene el lock: PID ", pid)
      tryCatch({
        tools::pskill(pid)
        message("💀 Proceso ", pid, " terminado. Reintentando conexión...")
        Sys.sleep(1)
        return(dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = read_only))
      }, error = function(e2) {
        stop("⚠️ No se pudo matar el proceso ", pid, ": ", e2$message)
      })
    } else {
      stop("No se pudo detectar PID del mensaje de error:\n", msg)
    }
  })
}

db_file <- argendataR::get_raw_path('R430C0')
conn <- safe_db_connect(db_file, read_only = TRUE)

table_presupuesto <- tbl(conn, "presupuesto")


# APN
df_apn <- table_presupuesto %>% 
  mutate(credito_devengado = as.numeric(credito_devengado),
         anio = as.integer(ejercicio_presupuestario),
         finalidad_id = as.integer(finalidad_id)) %>%
  group_by(anio, finalidad_id) %>% 
  summarise(total_credito_devengado = sum(credito_devengado, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(anio) %>% 
  collect()

dbDisconnect(conn)


# Clasificador Finalidad

source("scripts/utils/wrapper_presupuesto_catalogo.R")

finalidad_funcion_diccionario_recurso <- PRESUPUESTO.get_catalog() %>% 
  dplyr::filter(nombreRecurso == "Clasificador presupuestario Finalidad y Función", periodicidad == 'eventual') %>%
  select(nombreRecurso, urlArchivo, urlDocumentacion, ejercicios) %>% 
  mutate(ejercicios = unlist(ejercicios)) %>% 
  dplyr::filter(ejercicios == max(ejercicios))


finalidad_dic <- PRESUPUESTO.get_data(finalidad_funcion_diccionario_recurso$ejercicios, 
                                      finalidad_funcion_diccionario_recurso$urlArchivo) %>% 
  distinct(finalidad_id, finalidad_desc)


# PIB
df_pib <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet() %>% 
  dplyr::filter(trim == "Total", indicador == "producto_interno_bruto") %>% 
  mutate(pbi = valor / 1000000) %>% 
  select(anio, pbi)


df_output <- df_apn %>% 
  left_join(finalidad_dic, join_by(finalidad_id)) %>% 
  inner_join(df_pib, join_by(anio)) %>% 
  mutate(total_credito_devengado = 100 * total_credito_devengado / pbi,
         anio = as.integer(anio)) %>% 
  select(anio, finalidad_desc, total_credito_devengado) 



df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            drive = T) %>% 
  mutate(anio = as.integer(ejercicio_presupuestario)) %>% 
  select(-ejercicio_presupuestario)

comparacion <- argendataR::comparar_outputs(
  df = df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = c("anio", "finalidad_desc")
)


armador_descripcion <- function(metadatos, etiquetas_nuevas = data.frame(), output_cols){
  # metadatos: data.frame sus columnas son variable_nombre y descripcion y 
  # proviene de la info declarada por el analista 
  # etiquetas_nuevas: data.frame, tiene que ser una dataframe con la columna 
  # variable_nombre y la descripcion
  # output_cols: vector, tiene las columnas del dataset que se quiere escribir
  
  etiquetas <- metadatos %>% 
    dplyr::filter(variable_nombre %in% output_cols) 
  
  
  etiquetas <- etiquetas %>% 
    bind_rows(etiquetas_nuevas)
  
  
  diff <- setdiff(output_cols, etiquetas$variable_nombre)
  
  stopifnot(`Error: algunas columnas de tu output no fueron descriptas` = length(diff) == 0)
  
  # En caso de que haya alguna variable que le haya cambiado la descripcion pero que
  # ya existia se va a quedar con la descripcion nueva. 
  
  etiquetas <- etiquetas %>% 
    group_by(variable_nombre) %>% 
    filter(if(n() == 1) row_number() == 1 else row_number() == n()) %>%
    ungroup()
  
  etiquetas <- stats::setNames(as.list(etiquetas$descripcion), etiquetas$variable_nombre)
  
  return(etiquetas)
  
}

# Tomo las variables output_name y subtopico declaradas arriba
metadatos <- argendataR::metadata(subtopico = subtopico) %>% 
  dplyr::filter(grepl(paste0("^", output_name), nombre_archivo)) %>% 
  distinct(variable_nombre, descripcion) 



# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output

etiquetas_nuevas <- data.frame(
  variable_nombre = c("anio"),
  descripcion = c("Año de referencia")
)


descripcion <- armador_descripcion(metadatos = metadatos,
                                   etiquetas_nuevas = etiquetas_nuevas,
                                   output_cols = output_cols)



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    control = comparacion, 
    fuentes = argendataR::colectar_fuentes(),
    analista = analista,
    es_serie_tiempo = T,
    columna_indice_tiempo = 'anio',
    pk = c("anio", "finalidad_desc"),
    descripcion_columnas = descripcion,
    unidades = list("total_credito_devengado" = "porcentaje")
  )


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "dev")
