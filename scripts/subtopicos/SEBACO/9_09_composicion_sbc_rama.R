################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "SEBACO"
output_name <- "09_composicion_sbc_rama"
analista = "Nicolas Sidicaro"
fuente1 <- "R238C138"

## traigo la data
data <- arrow::read_parquet(argendataR::get_clean_path(fuente1))

# Paso 2: Filtrar las actividades específicas de SBC (Servicios Basados en Conocimientos)
sbc_puestos <- data %>% 
  dplyr::filter(ciiu_rev3_4d %in% c(
               '2213', # Edición de grabaciones
               '3530', # Fabricación y reparación de aeronaves (????)
               '7210', # Servicios de consultores en equipo de informática
               '7220', # Servicios de consultores en informática y suministros de programas de informática
               '7230', # Procesamiento de datos
               '7240', # Servicios relacionados con bases de datos
               '7290', # Actividades de informática n.c.p.
               '7300', # Investigación y desarrollo
               '7410', # Servicios jurídicos y de contabilidad, teneduría de libros y auditoría; asesoramiento en materia de impuestos; estudios de mercados y realización de encuestas de opinión pública; asesoramiento empresarial y en materia de gestión
               '7421', # Servicios de arquitectura e ingeniería y servicios conexos de asesoramiento técnico
               '7430', # Servicios de publicidad
               '7494', # Servicios de fotografía
               '7491', # Obtención y dotación de personal
               '9211'  # Producción y distribución de filmes y videocintas
  )) %>% 
  mutate(rama_de_actividad = case_when(ciiu_rev3_4d %in% c('7210','7220','7230','7240','7290') ~ 'SSI',
                                       ciiu_rev3_4d %in% c('7300') ~ 'Investigación y desarrollo',
                                       ciiu_rev3_4d %in% c('7410') ~ 'Ss. Jurídicos y de contabilidad',
                                       ciiu_rev3_4d %in% c('7421') ~ 'Ss. Arquitectura',
                                       ciiu_rev3_4d %in% c('7430') ~ 'Ss. publicidad',
                                       TRUE ~ 'Otras')) %>%
  group_by(anio,rama = rama_de_actividad) %>%
  summarize(puestos_sbc = sum(cant_promedio_puestos_privados, na.rm = TRUE)) %>% 
  ungroup()

df_output <- sbc_puestos %>% 
  group_by(anio) %>% 
  mutate(prop_rama = puestos_sbc / sum(puestos_sbc, na.rm =T)) %>% 
  ungroup() %>% 
  select(anio, rama, prop_rama)

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

## data anterior

df_anterior <- argendataR::descargar_output(nombre =output_name, 
                                            subtopico = subtopico, 
                                            entrega_subtopico = "primera_entrega") %>% 
  mutate(anio = as.integer(anio))

comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  pk = c("anio","rama"), # variables pk del dataset para hacer el join entre bases
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
  variable_nombre = c("prop_rama"),
  descripcion = c("Proporción del empleo registrado de cada rama dentro del sector SBC")
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
    control = comparacion,
    subtopico = subtopico,
    fuentes = colectar_fuentes(),
    analista = analista,
    pk = c("anio","rama"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    descripcion_columnas = descripcion,
    unidades = list("prop_rama" = "proporción")
  )


