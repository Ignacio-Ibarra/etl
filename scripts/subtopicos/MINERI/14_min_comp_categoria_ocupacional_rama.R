################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "MINERI"
output_name <- "min_comp_categoria_ocupacional_rama"
analista = "Kevin Corfield"
fuente1 <- "R49C16"


df_eph <- arrow::read_parquet(argendataR::get_clean_path(fuente1)) 


df_intermedio <- df_eph %>% 
  select(anio = ano4, pondera, pp04b_cod, pp04b_label, 
         letra_desc_abrev, cat_ocup, estado, pp07h) %>% 
  mutate(
    
    pp04b_cod = as.integer(pp04b_cod),
    
    rama_actividad = case_when(
      pp04b_cod %in% c(6,500,600,900) ~ "Extracción de petróleo y gas",
      pp04b_cod == 700 ~ "Minería metalífera",
      pp04b_cod == 800 ~ "Minería no metalífera",
      letra_desc_abrev == "Agro y pesca" ~ "Agro",
      letra_desc_abrev == "Serv. inmobiliarios" ~ "Inmobiliarias",
      letra_desc_abrev == "Industria manufacturera" ~ "Industria",
      letra_desc_abrev == "Servicio doméstico" ~ "Serv. Doméstico",
      letra_desc_abrev == "Hotelería y restaurantes" ~ "Hoteles y restaurantes",
      letra_desc_abrev == "Administración pública" ~ "Administración pública y defensa",
      pp04b_cod >= 58 & pp04b_cod < 64 ~ "Información y comunicación",
      pp04b_cod >= 5800 & pp04b_cod < 6400 ~ "Información y comunicación",
      pp04b_cod >= 69 & pp04b_cod < 77 ~ "Act. profesionales, científicas y técnicas",
      pp04b_cod >= 6900 & pp04b_cod < 7700 ~ "Act. profesionales, científicas y técnicas",
      pp04b_cod >= 94 & pp04b_cod < 97 ~ "Otros servicios",
      pp04b_cod >= 9400 & pp04b_cod < 9700 ~ "Otros servicios",
      pp04b_cod >=9900 & pp04b_cod <=99999 | pp04b_cod == 99 ~ "Otros",
      pp04b_cod >=9000 & pp04b_cod < 9400 ~ "Recreación",
      pp04b_cod > 90 & pp04b_cod < 94 ~ "Recreación",
      TRUE ~ letra_desc_abrev 
    ),
    categoria_ocupacional = case_when(
      cat_ocup == 3 & pp07h == 1 ~ "Asalariado registrado",
      cat_ocup == 3 & pp07h == 2 ~ "Asalariado no registrado",
      cat_ocup != 3 & estado == 1 ~ "No asalariado",
      estado != 1 ~ NA_character_,
      TRUE ~ NA_character_
    )
  ) %>% 
  filter(pp04b_cod != 0)

df_output <- df_intermedio %>% 
  group_by(
    rama_actividad, categoria_ocupacional
  ) %>% 
  summarise(
    pondera = sum(pondera, na.rm=T)
  ) %>% 
  ungroup() %>% 
  group_by(rama_actividad) %>%
  mutate(
    porcentaje_sobre_total_rama = 100 * pondera / sum(pondera, na.rm=T)
  ) %>% 
  ungroup() %>% 
  select(-pondera) %>% 
  complete(., rama_actividad, categoria_ocupacional, fill = list(porcentaje_sobre_total_rama = 0))
  

df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  mutate(
    categoria_ocupacional = case_when(
      categoria_ocupacional == "asalariados_registrados" ~ "Asalariado registrado",
      categoria_ocupacional == "asalariados_no_registrados" ~ "Asalariado no registrado",
      categoria_ocupacional == "no_asalariados" ~ "No asalariado"
    ),
    rama_actividad = case_when(
      rama_actividad == "Reciclamiento de desperdicios, agua y saneamiento" ~ "Agua y saneamiento",
      rama_actividad == "Act. Administrativas" ~ "Actividades administrativas",
      rama_actividad == "Otras minas y canteras" ~ "Minería no metalífera",
      
      TRUE ~ rama_actividad
    )
  )

comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("rama_actividad","categoria_ocupacional"), # variables pk del dataset para hacer el join entre bases
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




descripcion <- armador_descripcion(metadatos = metadatos,
                                   # etiquetas_nuevas = etiquetas_nuevas,
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
    pk =c("rama_actividad","categoria_ocupacional"),
    es_serie_tiempo = F,
    control = comparacion, 
    descripcion_columnas = descripcion,
    unidades = list("porcentaje_sobre_total_rama" = "porcentaje"),
    aclaracion = "Se cambiaron los strings de la columna 'categoria_ocupacional' para adaptarlos al gráfico. Se cambiaron las ramas de actividad. Se cambió la fuente de información de SIACAM a EPH Total Urbano, ya que el primero se habia basado en el segundo, el calculo del share se hace tomando un promedio global, se actualizaron los años. "
  )

