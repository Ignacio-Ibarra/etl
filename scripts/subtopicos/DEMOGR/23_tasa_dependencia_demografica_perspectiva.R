# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "DEMOGR"
output_name <- "tasa_dependencia_demografica_perspectiva.csv"
analista <- "Rafael Rofman & Ximena de la Fuente"

fuente1 <- 'R435C280'

df_wpp <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)


df_output <- df_wpp %>% 
  dplyr::filter(iso3_code == "ARG") %>% 
  select(anio = time, rango_etario = age_grp, V = pop_male, M = pop_female) %>% 
  pivot_longer(cols = c(V, M), names_to = "sexo", values_to = "poblacion_wpp" , values_transform = ~ .x *1000) %>%
  mutate(productiva_bool = case_when(
    as.numeric(str_extract(rango_etario, "(?<=-)[0-9]+")) < 20 ~ "no_productiva",
    as.numeric(str_extract(rango_etario, "(?<=-)[0-9]+")) %>% 
      between(., 20, 64) ~ "productiva",
    as.numeric(str_extract(rango_etario, "(?<=-)[0-9]+")) > 64 | rango_etario == "100+" ~ "no_productiva")) %>% 
  group_by(anio, productiva_bool) %>% 
  summarise(poblacion = sum(poblacion_wpp, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = anio, names_from = productiva_bool, values_from = poblacion) %>% 
  mutate(tasa_dependencia = 100 * no_productiva / productiva) %>% 
  select(anio, tasa_dependencia) %>% 
  dplyr::filter(anio >= year(Sys.Date()))


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico)

pks <- c('anio')

comparacion <- argendataR::comparar_outputs(
  df = df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = pks
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



descripcion <- armador_descripcion(metadatos = metadatos,
                                   # etiquetas_nuevas = etiquetas_nuevas,
                                   output_cols = output_cols)



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    control = comparacion, 
    fuentes = argendataR::colectar_fuentes(),
    analista = analista,
    pk = pks,
    descripcion_columnas = descripcion, 
    unidad = list("tasa_dependencia" = "porcentaje"))


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "main")
