# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "FISCAL"
output_name <- "ciclicidad_paises.csv"
analista <- "María Fernanda Villafañe & Micaela Fernandez Erlauer"

fuente1 <- 'R426C274'

df_wb <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

df_output <- df_wb %>% 
  select(geocodigoFundar = iso3, geonombreFundar = pais_nombre, tipo_pais = desarrollo_economia, correlacion_ciclicidad_fiscal = corr_gasto_ciclico_gdp) %>% 
  mutate(correlacion_ciclicidad_fiscal = as.numeric(correlacion_ciclicidad_fiscal)) %>% 
  mutate(tipo_pais = ifelse(tipo_pais == "Emerging Markets", "Economía en desarrollo", "Economía avanzada"))


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            drive = T)  %>% 
  mutate(tipo_pais = ifelse(tipo_pais == "economia_en_desarrollo", "Economía en desarrollo", "Economía avanzada")) %>% 
  rename(geocodigoFundar = codigo_pais, geonombreFundar = pais)


comparacion <- argendataR::comparar_outputs(
  df = df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = c("geocodigoFundar")
)


# Tomo las variables output_name y subtopico declaradas arriba
metadatos <- argendataR::metadata(subtopico = subtopico) %>% 
  dplyr::filter(grepl(paste0("^", output_name), nombre_archivo)) %>% 
  distinct(variable_nombre, descripcion) 



# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output

etiquetas_nuevas <- data.frame(
  variable_nombre = c("geocodigoFundar", 
                      "geonombreFundar"),
  descripcion = c("Códigos de país ISO 3166 - alfa 3",
                  "Nombre de país de referencia")
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
    pk = c("geocodigoFundar"),
    columna_geo_referencia = "geocodigoFundar",
    nivel_agregacion = "pais",
    descripcion_columnas = descripcion,
    etiquetas_indicadores = list("correlacion_ciclicidad_fiscal" = "Correlación entre el componente cíclico del gasto público real y el PBI real (2000-2022)"),
    unidades = list("correlacion_ciclicidad_fiscal" = "unidades")
  )



output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "dev")