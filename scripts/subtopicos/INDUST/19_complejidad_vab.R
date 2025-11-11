#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

# Metadatos 
subtopico <- "INDUST"
output_name <- "complejidad_vab.csv"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R225C97' # TiVA 2025 rama industria
fuente2 <- 'R227C0' # Descriptores


df_tiva <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

df_descriptores_industria <- argendataR::get_raw_path(fuente2) %>% 
  readxl::read_excel() %>% 
  dplyr::filter(grepl("^C.*", ind)) %>% 
  select(ind, intensidad_id_ocde_desc)

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long)

df_output <- df_tiva %>% 
  dplyr::filter(sector != "C") %>% 
  right_join(df_descriptores_industria, join_by(sector == ind)) %>% 
  dplyr::filter(vab_usd > 0 ) %>% 
  group_by(anio, iso3, intensidad_id_ocde_desc) %>% 
  summarise(
    vab = sum(vab_usd, na.rm = T)
  ) %>% 
  group_by(anio, iso3) %>% 
  mutate(prop_vab = vab / sum(vab, na.rm = T)) %>% 
  ungroup()  %>% 
  mutate(iso3 = ifelse(iso3 == "W", "WLD", iso3)) %>% 
  left_join(geo_front, join_by(iso3 == geocodigoFundar)) %>% 
  select(anio, geocodigoFundar = iso3, geonombreFundar, intensidad_id_ocde_desc, vab, prop_vab) %>% 
  drop_na(geonombreFundar)


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico) 


pks <- c('anio','geocodigoFundar', 'intensidad_id_ocde_desc')

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
    unidad = list("poblacion" = "unidades", "share" = "porcentaje"))


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "main")


