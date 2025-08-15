# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "FISCAL"
output_name <- "base_ceq_long.csv"
analista <- "María Fernanda Villafañe & Micaela Fernandez Erlauer"

fuente1 <- 'R425C273'

df_ceq <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

df_output <- df_ceq %>% 
  dplyr::filter(name %in% c("Market income plus pensions", "Final income"), 
                concept == "PDI") %>% 
  select(iso3, pais_nombre, 
         anio = start_year, 
         ingreso = name, 
         gini = indicator_value) %>% 
  drop_na(gini) %>% 
  rename(pais = pais_nombre) %>% 
  mutate(ingreso  = ifelse(ingreso == 'Final income', "Ingreso final", "Ingresos de mercado más pensiones"),
         gini = gini/100) %>% 
  rename(geocodigoFundar = iso3, geonombreFundar = pais)


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            drive = T)   %>%
  mutate(ingreso = ifelse(ingreso == 'mercado', "Ingresos de mercado más pensiones", "Ingreso final")) %>% 
  rename(geonombreFundar = pais)


comparacion <- argendataR::comparar_outputs(
  df = df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = c("geonombreFundar", "anio", "ingreso")
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
    control = comparacion,
    subtopico = subtopico,
    fuentes = argendataR::colectar_fuentes(),
    analista = analista,
    pk = c("geocodigoFundar", "anio", "ingreso"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "geocodigoFundar",
    nivel_agregacion = "pais",
    descripcion_columnas = descripcion,
    unidades = list("gini" = "indice")
  )


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "dev")
