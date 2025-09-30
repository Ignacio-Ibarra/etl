# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "FISCAL"
output_name <- "base_socx_odce_cepal.csv"
analista <- "María Fernanda Villafañe & Micaela Fernandez Erlauer"

fuente1 <- 'R403C254' # OECD Social Expenditure Database
fuente2 <- 'R404C255' # Gasto social público y privado (metodología SOCX), en porcentajes del PIB

df_oecd <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

df_cepal <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet()

tabla_ocde <- df_oecd %>% 
  dplyr::filter(grepl("TP\\d{2}$|_T", programme_type),# Filtro los programas a dos dígitos
                unit_measure == "PT_B1GQ", # Porcentaje de PIB
                expend_source == "ES10", # Público 
                programme_type == "_T", #Total
                spending_type == "_T") %>% 
  select(geocodigoFundar = ref_area, anio = time_period, valor = obs_value) %>% 
  mutate(anio = as.integer(anio),
         fuente = "OECD")


tabla_cepal <- df_cepal %>% 
  dplyr::filter(fuente_id == 79339, programa == "Total") %>% 
  select(geocodigoFundar = iso3, anio, valor) %>% 
  mutate(anio = as.integer(anio), 
         fuente = "CEPAL")

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long)

df_output<- bind_rows(tabla_cepal %>% 
                        anti_join(tabla_ocde, join_by(anio, geocodigoFundar)),
                      tabla_ocde) %>% 
  left_join(geo_front, join_by(geocodigoFundar)) %>% 
  drop_na(valor)


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            drive = F) %>% 
  mutate(anio = as.integer(anio))


comparacion <- argendataR::comparar_outputs(
  df = df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = c("geocodigoFundar", "anio")
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
  variable_nombre = c("geocodigoFundar", 
                      "geonombreFundar",
                      "fuente"),
  descripcion = c("Códigos de país ISO 3166 - alfa 3",
                  "Nombre de país",
                  "Fuente de información utilizada")
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
    pk = c("geocodigoFundar", "anio"),
    es_serie_tiempo = T,
    nivel_agregacion = 'pais',
    columna_indice_tiempo = 'anio',
    columna_geo_referencia = 'geocodigoFundar',
    descripcion_columnas = descripcion,
    unidades = list("valor" = "porcentaje")
  )


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "dev")
