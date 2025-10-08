#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


# Metadatos 
subtopico <- "INDUST"
output_name <- "share_id.csv"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R455C0' # OECD Main Science and Technology Indicators (MSTI database)


df_oecd <- argendataR::get_raw_path(fuente1) %>% 
  read.csv()
  

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long)



# Seleccionar sectores
df_berd <- df_oecd %>% 
  janitor::clean_names() %>% 
  dplyr::filter(activity %in% c('C','_T'),
         criteria == "MA",
         unit_measure == "USD_PPP",
         price_base == "V") %>% 
  select(anio = time_period, geocodigoFundar = ref_area, activity, obs_value) %>% 
  drop_na(obs_value) %>% 
  group_by(anio, geocodigoFundar) %>% 
  dplyr::filter(n() == 2) %>% 
  ungroup() 

df_berd_wld <- df_berd %>% 
  group_by(anio, activity) %>% 
  summarise(obs_value = sum(obs_value, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(anio) %>% 
  mutate(share_indust_id = obs_value / sum(obs_value)) %>% 
  ungroup() %>% 
  dplyr::filter(activity == "C") %>% 
  select(-activity, -obs_value) %>% 
  mutate(geocodigoFundar = "WLD")


df_output <- df_berd %>% 
  group_by(anio, geocodigoFundar) %>% 
  mutate(share_indust_id = obs_value / sum(obs_value)) %>% 
  ungroup() %>% 
  dplyr::filter(activity == "C") %>% 
  select(-activity, -obs_value) %>% 
  bind_rows(., df_berd_wld) %>% 
  left_join(geo_front, join_by(geocodigoFundar)) %>% 
  select(anio, geocodigoFundar, geonombreFundar, share_indust_id)
  
  
df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico, drive = T) 


df_comparable <- df_output %>% 
  rename(iso3 = geocodigoFundar,
         name_long = geonombreFundar) %>% 
  mutate(anio = as.numeric(anio)) %>% 
  dplyr::filter(iso3 != "WLD")


pks_comparacion <- c('anio','iso3')

comparacion <- argendataR::comparar_outputs(
  df = df_comparable,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = pks_comparacion
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
    unidad = list("poblacion" = "unidades", "share" = "porcentaje"))


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "main")



