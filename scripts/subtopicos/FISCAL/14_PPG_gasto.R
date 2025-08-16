#Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

library(DBI)
library(duckdb)

# Defino variables
subtopico <- "FISCAL"
output_name <- "PPG_gasto.csv"
analista <- "María Fernanda Villafañe & Micaela Fernandez Erlauer"

fuente1 <- 'R430C0' # Presupuesto Público

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

anio_max <- table_presupuesto %>% 
  distinct(as.integer(ejercicio_presupuestario)) %>% 
  collect() %>% 
  pull() %>% 
  max() %>% 
  as.integer()

df_stage <- table_presupuesto %>% 
  dplyr::filter(ejercicio_presupuestario == anio_max) %>% 
  collect()

dbDisconnect(conn)

source("scripts/utils/wrapper_presupuesto_catalogo.R")

catalogo_completo <- PRESUPUESTO.get_catalog()


catalogo_filtrado <- catalogo_completo %>% 
  dplyr::filter(grepl("Clasificador.*", nombreRecurso), periodicidad == 'eventual') %>%
  select(nombreRecurso, urlArchivo, urlDocumentacion, ejercicios) %>% 
  mutate(ejercicios = unlist(ejercicios)) %>% 
  dplyr::filter(ejercicios == anio_max)


institucion_diccionario_recurso <- catalogo_filtrado %>% 
  dplyr::filter(nombreRecurso == "Clasificador presupuestario Institución") %>% 
  select(ejercicios, urlArchivo)


apertura_programatica_diccionario_recurso <- catalogo_filtrado %>% 
  dplyr::filter(nombreRecurso == "Clasificador presupuestario Apertura Programática") %>% 
  select(ejercicios, urlArchivo)


finalidad_funcion_diccionario_recurso <- catalogo_filtrado %>% 
  dplyr::filter(nombreRecurso == "Clasificador presupuestario Finalidad y Función") %>% 
  select(ejercicios, urlArchivo)


juris_dicc <- PRESUPUESTO.get_data(institucion_diccionario_recurso$ejercicios, 
                                  institucion_diccionario_recurso$urlArchivo) %>% 
  distinct(jurisdiccion_id, jurisdiccion_desc)

actividad_dicc <- PRESUPUESTO.get_data(apertura_programatica_diccionario_recurso$ejercicios, 
                                      apertura_programatica_diccionario_recurso$urlArchivo) %>% 
  distinct(servicio_id, programa_id, subprograma_id, proyecto_id, actividad_id, programa_desc, actividad_desc)

finalidad_dic <- PRESUPUESTO.get_data(finalidad_funcion_diccionario_recurso$ejercicios, 
                                 finalidad_funcion_diccionario_recurso$urlArchivo) %>% 
  distinct(finalidad_id, finalidad_desc)


df_intermediate <- df_stage %>% 
  select(ejercicio_presupuestario, jurisdiccion_id, servicio_id, programa_id, subprograma_id, proyecto_id, actividad_id, finalidad_id, inciso_id, credito_devengado) %>% 
  mutate(credito_devengado = as.numeric(credito_devengado)) %>% 
  mutate(across(matches("*_id|ejercicio_presupuestario"), ~as.integer(.x))) %>% 
  left_join(finalidad_dic, join_by(finalidad_id)) %>% 
  left_join(juris_dicc, join_by(jurisdiccion_id)) %>% 
  left_join(actividad_dicc, join_by(servicio_id, programa_id, subprograma_id, proyecto_id, actividad_id)) %>% 
  mutate(ppg = ifelse(grepl("PPG", actividad_desc), "PPG", "No PPG")) %>% 
  group_by(ejercicio_presupuestario, programa_desc, ppg) %>% 
  summarise(credito_devengado = sum(credito_devengado, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(porcentaje = 100 * credito_devengado / sum(credito_devengado)) 


# Reemplazar los que no están en el top por "Otros"
df_output <- df_intermediate %>% 
  select(ppg_label = ppg, programa_desc, porcentaje) 
  

df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            drive = T) %>% 
  select(-nuevo_programa_desc)

comparacion <- argendataR::comparar_outputs(
  df = df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = c("ppg_label", "programa_desc")
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
  variable_nombre = c("programa_desc"),
  descripcion = c("Decripción del programa del presupuesto público")
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
    pk = c("ppg_label", "programa_desc"),
    descripcion_columnas = descripcion,
    unidades = list("porcentaje" = "porcentaje")
  )


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "dev")


