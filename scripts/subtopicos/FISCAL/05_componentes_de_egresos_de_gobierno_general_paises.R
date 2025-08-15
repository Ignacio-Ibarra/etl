# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "FISCAL"
output_name <- "componentes_de_egresos_de_gobierno_general_paises.csv"
analista <- "María Fernanda Villafañe & Micaela Fernandez Erlauer"

fuente1 <- 'R427C0'

df_picn <- argendataR::get_raw_path(fuente1) %>% 
  read_csv()

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long)

df_output <- df_picn %>% 
  select(-pais) %>% rename(geocodigoFundar = codigo_pais) %>% 
  mutate(componente_egresos_del_gobierno = case_when(
    componente_egresos_del_gobierno == "consumo_del_gobierno" ~ "Consumo del gobierno",
    componente_egresos_del_gobierno == "subvenciones_economicas" ~ "Subvenciones económicas",
    componente_egresos_del_gobierno == "rentas_intereses" ~ "Rentas / Intereses",
    componente_egresos_del_gobierno == "prestaciones_sociales" ~ "Prestaciones sociales",
    componente_egresos_del_gobierno == "otras_transferencias_corrientes_y_de_capital" ~ "Otras transferencias corrientes y de capital",
    componente_egresos_del_gobierno == "formacion_bruta_de_capital" ~ "Formación bruta de capital",
    TRUE ~ "TOTAL"
  ),
  porcentaje_del_pib = 100*porcentaje_del_pib
        ) %>% 
  left_join(geo_front, join_by(geocodigoFundar)) %>% 
  select(geocodigoFundar, geonombreFundar, componente_egresos_del_gobierno, porcentaje_del_pib)

df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            drive = T)   %>% 
  select(-pais) %>% rename(geocodigoFundar = codigo_pais) %>%
  mutate(componente_egresos_del_gobierno = case_when(
    componente_egresos_del_gobierno == "consumo_del_gobierno" ~ "Consumo del gobierno",
    componente_egresos_del_gobierno == "subvenciones_economicas" ~ "Subvenciones económicas",
    componente_egresos_del_gobierno == "rentas_intereses" ~ "Rentas / Intereses",
    componente_egresos_del_gobierno == "prestaciones_sociales" ~ "Prestaciones sociales",
    componente_egresos_del_gobierno == "otras_transferencias_corrientes_y_de_capital" ~ "Otras transferencias corrientes y de capital",
    componente_egresos_del_gobierno == "formacion_bruta_de_capital" ~ "Formación bruta de capital",
    TRUE ~ "TOTAL"
  ),
  porcentaje_del_pib = 100*porcentaje_del_pib
  ) %>% 
  left_join(geo_front, join_by(geocodigoFundar)) %>% 
  select(geocodigoFundar, geonombreFundar, componente_egresos_del_gobierno, porcentaje_del_pib)


comparacion <- argendataR::comparar_outputs(
  df = df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = c("geocodigoFundar")
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
                      "geonombreFundar"),
  descripcion = c("Códigos de país ISO 3166 - alfa 3",
                  "Nombre de país")
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
    unidades = list("porcentaje_del_pib" = "proporción")
  )
