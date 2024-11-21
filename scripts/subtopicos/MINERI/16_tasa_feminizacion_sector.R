################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "MINERI"
output_name <- "tasa_feminizacion_sector"
analista = "Kevin Corfield"
fuente1 <- "R235C147"
fuente2 <- "R235C105"


df_bel_2_4 <- arrow::read_parquet(argendataR::get_clean_path(fuente1)) 

df_bel_2_3 <- arrow::read_parquet(argendataR::get_clean_path(fuente2)) 


df_mineria <- df_bel_2_4 %>% 
  dplyr::filter(rama_de_actividad %in% c("Extracción de minerales metalíferos",
                                         "Explotación de otras minas y canteras",
                                         "Petróleo y gas")) %>% 
  select(1:4) %>% 
  mutate(
    anio = str_extract(trimestre_anio, ".*(\\d{4}).*", group=1),
    trimestre = str_extract(trimestre_anio, "(.*) \\d{4}", group=1),
  ) %>% 
  mutate(
    sector = case_when(
      rama_de_actividad == "Extracción de minerales metalíferos" ~ "Minería metalífera",
      rama_de_actividad == "Explotación de otras minas y canteras" ~ "Minería no metalífera",
      TRUE ~ rama_de_actividad
    )) %>% 
  select(anio, trimestre, sector, sexo, puestos = cantidad_puestos_privados_registrados) 

df_resto <- df_bel_2_3 %>% 
  mutate(
    anio = str_extract(periodo_trimestre_ano, ".*(\\d{4}).*", group=1),
    trimestre = str_extract(periodo_trimestre_ano, "(.*) \\d{4}", group=1),
  ) %>% 
  select(anio, trimestre, sector = letra_desc_abrev, sexo, puestos) %>% 
  dplyr::filter(sector != "Petróleo y minería")
  

df_output <- df_resto %>% 
  bind_rows(df_mineria) %>% 
  group_by(anio, sector, sexo) %>% 
  mutate(anios_completos = n() == 4) %>% 
  ungroup() %>% 
  dplyr::filter(anios_completos) %>% 
  dplyr::filter(anio == max(anio)) %>% 
  group_by(anio, sector, sexo) %>% 
  summarise(
    puestos_sumados = sum(puestos, na.rm = T)
  ) %>% 
  ungroup() %>% 
  group_by(anio, sector) %>% 
  mutate(
    tasa_feminizacion = 100*puestos_sumados / sum(puestos_sumados)
  ) %>% 
  ungroup() %>% 
  dplyr::filter(sexo != "Varón") %>% 
  select(sector, tasa_feminizacion) %>% 
  arrange(-tasa_feminizacion)




df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  mutate(
    
    sector = case_when(
      sector == "Servicios sociales y de salud" ~ "Salud",
      sector == "Intermediación financiera" ~ "Finanzas",
      sector == "Servicios comunitarios, sociales y personales" ~ "Serv. comunitarios, sociales y personales",
      sector == "Servicios inmobiliarios, empresariales y de alquiler" ~ "Serv. inmobiliarios y profesionales",
      sector == "Transporte y almacenamiento" ~ "Transporte y comunicaciones",
      sector == "Agricultura, ganadería, caza y silvicultura" ~ "Agro",
      TRUE ~ sector
    )
    )


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("sector"), # variables pk del dataset para hacer el join entre bases
  drop_joined_df =  F
)


#-- Exportar Output ----

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
  dplyr::filter(grepl(paste0("^", output_name,".csv"), dataset_archivo)) %>% 
  distinct(variable_nombre, descripcion) 


# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output



etiquetas_nuevas <- data.frame(
  variable_nombre = c("sector"),
  descripcion = c("Sector de la economía (nivel letra CIIU Rev. 3). Sector C 'Explotación de minas y canteras' se encuentra desagregado (nivel 2 dígitos del CIIU Rev. 3)")
)



descripcion <- armador_descripcion(metadatos = metadatos,
                                   etiquetas_nuevas = etiquetas_nuevas,
                                   output_cols = output_cols)


colectar_fuentes <- function(pattern = "^fuente.*"){
  
  # Genero un vector de codigos posibles
  posibles_codigos <- c(fuentes_raw()$codigo,fuentes_clean()$codigo)
  
  # Usar ls() para buscar variables en el entorno global
  variable_names <- ls(pattern = pattern, envir = globalenv())
  
  # Obtener los valores de esas variables
  valores <- unlist(mget(variable_names, envir = globalenv()))
  
  # Filtrar aquellas variables que sean de tipo character (string)
  # Esto es para que la comparacion sea posible en la linea de abajo
  strings <- valores[sapply(valores, is.character)]
  
  # solo devuelvo las fuentes que existen
  return(valores[valores %in% posibles_codigos])
}



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = colectar_fuentes(),
    analista = analista,
    pk = c("sector"),
    es_serie_tiempo = T,
    control = comparacion, 
    descripcion_columnas = descripcion,
    unidades = list("tasa_feminizacion" = "porcentaje"),
    aclaracion = "Se cambiaron nombres a algunos sectores. Se desagregó el sector de Minería y se crearon tres sectores 'Minería metalífera', 'Minería no metalífera' y 'Petróleo y gas' que no se encontraban anteriormente y también el sector 'No definido'."
  )

