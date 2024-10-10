################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "ESTPRO"
output_name <- "particip_sectorial_empleo"
analista = "Gisella Pascuariello"


fuente1 <- "R228C98"
fuente2 <- "R228C99"
fuente3 <- "R228C100"
fuente4 <- "R229C0"


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





get_raw_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}raw/")
  df_fuentes_raw <- fuentes_raw() 
  path_raw <- df_fuentes_raw[df_fuentes_raw$codigo == codigo,c("path_raw")]
  return(paste0(prefix, path_raw))
}

get_clean_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}clean/")
  df_fuentes_clean <- fuentes_clean() 
  path_clean <- df_fuentes_clean[df_fuentes_clean$codigo == codigo,c("path_clean")]
  return(paste0(prefix, path_clean))
}


# Cargo data desde server

df_puestos_ar <- arrow::read_parquet(get_clean_path(fuente1)) %>% 
  dplyr::filter(edad_sexo == "Total general") %>%  
  select(-edad_sexo) %>% 
  replace_na(list(puestos_ar = 0))

df_puestos_anr <- arrow::read_parquet(get_clean_path(fuente2)) %>% 
  dplyr::filter(edad_sexo == "Total general") %>%  
  select(-edad_sexo) %>% 
  replace_na(list(puestos_anr = 0))

df_puestos_na <- arrow::read_parquet(get_clean_path(fuente3)) %>% 
  dplyr::filter(edad_sexo == "Total general") %>%  
  select(-edad_sexo) %>% 
  replace_na(list(puestos_na = 0))


df_puestos_total <- df_puestos_ar %>% 
  left_join(df_puestos_anr, join_by(anio, letra, letra_desc)) %>% 
  left_join(df_puestos_na, join_by(anio, letra, letra_desc)) %>% 
  mutate(puestos_total = puestos_ar + puestos_anr + puestos_na) %>% 
  select(anio, letra, puestos_total) %>% 
  mutate(letra = case_when(
    letra == "A + B" ~ "A_B",
    letra %in% c("N", "O") ~ "N_O",
    letra %in% c("L", "Q") ~ "L_Q",
    TRUE ~ letra)) %>% 
  group_by(anio, letra) %>% 
  summarise(puestos_total_indec = sum(puestos_total, na.rm = T) * 1000)


letras_indec <- unique(df_puestos_total$letra)


df_fichas_cep <- read_csv(get_raw_path(fuente4)) %>% 
  dplyr::filter(nombre_variable == "TOT_PUESTOS") %>% 
  select(anio, letra = id_sector_productivo, puestos_total_cep = valor) %>% 
  dplyr::filter(letra %in% letras_indec) %>% 
  mutate(puestos_total_cep = as.numeric(puestos_total_cep))


letra_desc_abrev <- list(
     'A_B' =  'Agro y pesca',
     'C' = 'Petróleo y minería',
     'D' = 'Industria manufacturera',
     'E' = 'Electricidad, gas y agua',
     'F' = 'Construcción',
     'G' = 'Comercio',
     'H' = 'Hotelería y restaurantes',
     'I' = 'Transporte y comunicaciones',
     'J' = 'Finanzas',
     'K' = 'Serv. inmobiliarios y profesionales',
     'L_Q' =  'Adm. pública y defensa',
     'M' = 'Enseñanza',
     'N_O' =  'Serv. comunitarios, sociales y personales',
     'P' = 'Servicio doméstico'
     )


df_output <- df_puestos_total %>% 
  full_join(df_fichas_cep, join_by(anio, letra)) %>% 
  mutate(
    puestos = ifelse(anio < 2016, puestos_total_cep, puestos_total_indec)
  ) %>% 
  select(anio, letra, puestos) %>% 
  mutate( 
    id_tipo_sector = ifelse(letra %in% c('A_B', 'C', 'D', 'E', 'F'),1, 2),
    tipo_sector = ifelse(letra %in% c('A_B', 'C', 'D', 'E', 'F'),'Bienes', 'Servicios'),
    letra_desc_abrev = recode(letra, !!!letra_desc_abrev)
    ) %>% 
  group_by(anio) %>% 
  mutate(share_sectorial = puestos / sum(puestos, na.rm=T)) %>% 
  ungroup()





# Modifico para que coincida con el nuevo formato
df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega") %>% 
  mutate(letra = case_when(
    letra == "AB" ~ "A_B",
    letra == "NO" ~ "N_O",
    letra == "LQ" ~ "L_Q",
    TRUE ~ letra)) 

comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("anio","letra"), # variables pk del dataset para hacer el join entre bases
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
  dplyr::filter(grepl(paste0(output_name,".csv"), dataset_archivo)) %>% 
  distinct(variable_nombre, descripcion) 


# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output


etiquetas_nuevas <- data.frame(
  variable_nombre = c("puestos"),
  descripcion = c("Cantidad de puestos")
)

descripcion <- armador_descripcion(metadatos = metadatos,
                                   etiquetas_nuevas = etiquetas_nuevas,
                                   output_cols = output_cols)



# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = colectar_fuentes(),
    analista = analista,
    pk = c("anio", "letra"),
    es_serie_tiempo = T,
    columna_indice_tiempo = 'anio',
    descripcion_columnas = descripcion,
    unidades = list("share_sectorial" = "unidades", "puestos" = "unidades")
    )
