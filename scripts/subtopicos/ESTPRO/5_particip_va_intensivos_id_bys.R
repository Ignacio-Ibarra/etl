################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "ESTPRO"
output_name <- "particip_va_intensivos_id_bys"
analista = "Gisella Pascuariello"

# Casos correctos
fuente1 <- "R225C97"
fuente2 <- "R226C96"
fuente3 <- "R227C0"

#Casos incorrectos
fuente4 <- data.frame(a = 1:3,
                      b = LETTERS[1:3])
fuente5 <- "R12345C0"

# Variaciones de casos correctos
fuente_fmi <- "R225C97"
fuente_ocde <- "R226C96"
fuente_sisi <- "R227C0"


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

# Traigo nomenclador desde google drive
geonomenclador <- argendataR::get_nomenclador_geografico() %>%
  select(iso3 = codigo_fundar, pais_nombre = desc_fundar, nivel_agregacion)


# Cargo data desde server


df_tiva <- arrow::read_parquet(get_clean_path(fuente1)) 

df_tiva_codes <- arrow::read_parquet(get_clean_path(fuente2)) %>% 
  select(code, industry)

df_clasif <- readxl::read_excel(argendataR::get_raw_path(fuente3)) %>% 
  select(sector = ind, gran_sector = gran_sector_desc, intensidad = intensidad_id_ocde_desc)

# Saco las que son agregaciones de df_tiva, quedandome solo con las que estan
# en df_tiva_codes

reemplazos_codigos <- list(
  "A5_A7" = "ZSCA",
  "WXD"   = "ROW",
  "E"     = "ZEUR",
  "EU28XEU15" = "EU13",
  "W" = "WLD",
  "F" = "F_ESTPRO", # Agregado al geonomenclador.json
  "W_O" = "ZOTH",
  "S2" = "EASIA",
  "S2_S8" = "ZASI",
  "NAFTA" = "NAFTA_ESTPRO", #Agregado al geonomenclador.json
  "WXOECD" = "NONOECD",
  "W_O" = "ZOTH"
)

df_output <- df_tiva %>% 
  right_join(df_tiva_codes, join_by(sector == code)) %>% 
  left_join(df_clasif, join_by(sector)) %>% 
  group_by(anio, iso3, intensidad) %>% 
  summarise(vab_usd = sum(vab_usd, na.rm=T)) %>% 
  ungroup() %>% 
  group_by(anio, iso3) %>% 
  mutate(particip = vab_usd / sum(vab_usd, na.rm=T)) %>% 
  ungroup() %>% 
  dplyr::filter(intensidad == "Media y alta intensidad de I+D") %>% 
  select(anio, iso3, particip_bys_alta_intensidad = particip) %>%
  mutate(iso3 = recode(iso3, !!!reemplazos_codigos)) %>% 
  left_join(geonomenclador, join_by(iso3)) 


df_output[df_output$iso3 == "NAFTA_ESTPRO", c("pais_nombre")] <- "Países miembros del NAFTA"
df_output[df_output$iso3 == "F_ESTPRO", c("pais_nombre")] <- "África"


# Modifico para que coincida con el nuevo formato
df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega") %>% 
  select(-iso3_desc_fundar, -es_agregacion) %>% 
  mutate(iso3 = recode(iso3, !!!reemplazos_codigos)) %>% 
  left_join(geonomenclador, join_by(iso3)) 

df_anterior[df_anterior$iso3 == "NAFTA_ESTPRO", c("pais_nombre")] <- "Países miembros del NAFTA"
df_anterior[df_anterior$iso3 == "F_ESTPRO", c("pais_nombre")] <- "África"


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("anio","iso3"), # variables pk del dataset para hacer el join entre bases
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
  variable_nombre = c("pais_nombre",
                      "nivel_agregacion",
                      "particip_bys_alta_intensidad"),
  descripcion = c("Nombre de país",
                  "Indica si el registro corresponde a un 'pais' o una 'agregacion' de países",
                  "Participación en el PBI de los bienes y servicios de media y alta intensidad")
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
    pk = c("anio", "iso3"),
    es_serie_tiempo = T,
    columna_indice_tiempo = 'anio',
    nivel_agregacion = "países",
    descripcion_columnas = descripcion,
    cambio_nombre_cols = list("pais_nombre" = "iso3_desc_fundar",
                              "nivel_agregacion" = "es_agregacion"),
    unidades = list("particip_bys_alta_intensidad" = "unidades"),
    aclaraciones = "Los codigos de varias agregaciones de países fueron modificados para ser armonizados con los códigos de agregaciones ya disponibles para OECD en el `argendataR::get_nomenclador_geografico()`"
  )
