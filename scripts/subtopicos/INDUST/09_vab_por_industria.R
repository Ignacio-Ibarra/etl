#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

# Metadatos 
subtopico <- "INDUST"
output_name <- "vab_por_industria.csv"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R225C97' # TiVA 2025 rama industria

df_tiva <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long)

df_tiva_selection <- df_tiva %>% 
  dplyr::filter(!sector %in% c('_T','BTE'), 
                vab_usd > 0 )

df_aux <- df_tiva %>% 
  dplyr::filter(sector == "C") %>% 
  select(anio, iso3, vab_usd_ind = vab_usd) 

df_intermediate <- df_tiva_selection %>% 
  left_join(df_aux, join_by(iso3, anio)) %>% 
  dplyr::filter(sector != "C",
                grepl("^C.*", sector))

traducir_actividades_tiva <- function(codigo) {
  case_when(
    # Códigos agregados de manufactura
    codigo == "C10T12" ~ "Productos alimenticios, bebidas y tabaco",
    codigo == "C13T15" ~ "Textiles, productos textiles, cuero y calzado",
    codigo == "C16T18" ~ "Productos de madera, papel e impresión",
    codigo == "C19T23" ~ "Productos químicos y minerales no metálicos",
    codigo == "C24_25" ~ "Metales básicos y productos metálicos fabricados",
    codigo == "C26_27" ~ "Equipos informáticos, electrónicos y eléctricos",
    codigo == "C28" ~ "Maquinaria y equipos, n.c.o.p.",
    codigo == "C29_30" ~ "Equipos de transporte",
    codigo == "C31T33" ~ "Manufactura n.c.o.p.; reparación e instalación de maquinaria y equipos",
    TRUE ~ NA_character_)
  
}

df_output <- df_intermediate %>% 
  mutate(
    prop_sobre_industria = vab_usd / vab_usd_ind,
    actividad = traducir_actividades_tiva(sector)) %>% 
  dplyr::filter(!is.na(actividad)) %>% 
  select(anio, geocodigoFundar = iso3, sector, actividad, prop_sobre_industria) %>% 
  left_join(geo_front, join_by(geocodigoFundar)) %>% 
  dplyr::filter(!is.na(geonombreFundar))
  

df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico) 

pks_comparacion <- c('anio','geocodigoFundar', 'sector')

comparacion <- argendataR::comparar_outputs(
  df = df_output,
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
