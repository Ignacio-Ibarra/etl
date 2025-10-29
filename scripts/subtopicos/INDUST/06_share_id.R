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
  pivot_wider(id_cols = c(anio, geocodigoFundar), 
              names_from = activity, 
              values_from = obs_value) %>% 
  rename(c = C,
         t = `_T`)

df_berd_wld <- df_berd %>% 
  group_by(anio) %>% 
  summarise(berd_manuf = sum(c, na.rm = T),
            berd_total = sum(t, na.rm = T))  %>% 
  ungroup() %>% 
  mutate(share_indust_id = 100 * berd_manuf / berd_total) %>% 
  mutate(geocodigoFundar = "WLD") %>% 
  select(anio, geocodigoFundar, share_indust_id)


df_output <- df_berd %>% 
  mutate(share_indust_id = 100 * c / t) %>% 
  ungroup() %>% 
  drop_na(share_indust_id) %>% 
  select(anio, geocodigoFundar, share_indust_id) %>% 
  bind_rows(., df_berd_wld) %>% 
  left_join(geo_front, join_by(geocodigoFundar)) %>% 
  select(anio, geocodigoFundar, geonombreFundar, share_indust_id)
  
  
df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico) 


pks <- c('anio','geocodigoFundar')

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
    unidad = list("share_indust_id" = "porcentaje"))


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "main")



