#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "SEBACO"
output_name <- "15_participacion_femenina_ssi"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R235C147'

df_oede <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.) %>% 
  mutate(
    anio = as.integer(str_extract(trimestre_anio, "[0-9]+"))
  ) %>% 
  group_by(rama_de_actividad, anio, sexo) %>% 
  dplyr::filter(n() == 4) %>% # Años completos
  ungroup()

df_ssi <- df_oede %>% 
  dplyr::filter(rama_de_actividad == "Actividades de informática") %>% 
  mutate(sector = "SSI") %>% 
  group_by(anio, sector, sexo) %>%
  summarise(
    suma_meses = sum(cantidad_puestos_privados_registrados, na.rm = T)
  ) %>% 
  ungroup() %>% 
  group_by(anio, sector) %>% 
  mutate(
    prop_mujeres = suma_meses / sum(suma_meses)
  ) %>% 
  ungroup() %>% 
  dplyr::filter(sexo == "Mujer") %>% 
  select(anio, sector, prop_mujeres)


df_total <- df_oede %>% 
  # dplyr::filter(rama_de_actividad == "Actividades de informática") %>% 
  mutate(sector = "Total privados") %>% 
  group_by(anio, sector, sexo) %>%
  summarise(
    suma_meses = sum(cantidad_puestos_privados_registrados, na.rm = T)
  ) %>% 
  ungroup() %>% 
  group_by(anio, sector) %>% 
  mutate(
    prop_mujeres = suma_meses / sum(suma_meses)
  ) %>% 
  ungroup() %>% 
  dplyr::filter(sexo == "Mujer") %>% 
  select(anio, sector, prop_mujeres)
  

df_output <- bind_rows(df_ssi, df_total)



df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  mutate(sector = ifelse(sector == "Promedio economia", "Total privados", "SSI"),
         anio = as.integer(anio))


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c('anio','sector'),
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
  variable_nombre = c("sector", 
                      "prop_mujeres"),
  descripcion = c("Sector de actividad: 'SSI': 'Actividades de informática (Capítulo 72 del CIIU Rev 3)' o 'Total privados': 'Total de sector privado'",
                  "Proporción puestos de trabajo privados ocupados por mujeres")
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

aclaracion <- "Se tomaron los datos mensuales de aquellos años que poseían 4 trimestres completos. En sector == 'Promedio economia' se imputó 'Total privados' dado que el registro corresponde a los puestos de trabajo privados únicamente"



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    fuentes = colectar_fuentes(),
    subtopico = subtopico,
    analista = analista,
    pk = c('anio','sector'),
    es_serie_tiempo = T,
    control = comparacion, 
    descripcion_columnas = descripcion,
    aclaraciones = aclaracion,
    unidades = list("prop_mujeres" = "porpoción")
  )
