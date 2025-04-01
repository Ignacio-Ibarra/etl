################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "CRECIM"
output_name <- "pib_per_capita_historico_cambio_1900"
analista = "Pablo Sonzogni"
fuente1 <- "R219C90"
fuente2 <- "R219C91"


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

geonomenclador <- argendataR::get_nomenclador_geografico() 

# Cargo data desde server
df_madd_c <- arrow::read_parquet(get_clean_path(fuente1)) %>% 
  dplyr::filter(anio>=1900) %>% 
  select(anio, iso3, gdppc) 

df_madd_r <- arrow::read_parquet(get_clean_path(fuente2)) %>%
  dplyr::filter(anio>=1900) %>% 
  select(anio, iso3, gdppc)

df_all <- df_madd_c %>% 
  bind_rows(df_madd_r) %>% 
  dplyr::filter(!is.na(gdppc))

df_1900 <- df_all %>% 
  dplyr::filter(anio == 1900) %>% 
  select(iso3, gdppc_1900 = gdppc)

df_output <- df_all %>% 
  left_join(df_1900, join_by(iso3)) %>% 
  mutate(cambio_relativo = (gdppc / gdppc_1900)-1) %>% 
  dplyr::filter(!is.na(cambio_relativo)) %>% 
  rename(pib_per_capita = gdppc) %>% 
  select(-gdppc_1900) #%>% 
  # left_join(geonomenclador, join_by(iso3)) %>% 
  # mutate(nivel_agregacion = ifelse(is.na(nivel_agregacion), "agregacion", nivel_agregacion))

check_iso3(df_output$iso3)

df_output <- df_output %>% 
  mutate(iso3 = case_when(
    iso3 == "YUG" ~ "SER",
    iso3 == "SUN" ~ "SVU",
    T ~ iso3
  ))

check_iso3(df_output$iso3)


# mutate(nivel_agregacion = ifelse(is.na(nivel_agregacion), "agregacion", nivel_agregacion))


df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")  


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("anio", "iso3"), # variables pk del dataset para hacer el join entre bases
  drop_joined_df =  F
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
  dplyr::filter(grepl(paste0("^", output_name,".csv"), dataset_archivo)) %>% 
  distinct(variable_nombre, descripcion) 


# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output


etiquetas_nuevas <- data.frame(
  variable_nombre = c("nivel_agregacion",
                      "cambio_relativo"),
  descripcion = c("Indicador si el registro refiere a un 'pais' o una 'agregacion' de países",
                  "Variación del PIB per cápita del año comparado con el de 1900")
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

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = colectar_fuentes(),
    control = comparacion,
    analista = analista,
    pk = c("anio", "iso3"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    aclaraciones = "El dataset entregado por el analista fue realizado con datos de Maddison Project Database 2020, en cambio en este caso se utilizaron datos de Maddison Project Database 2023. Los países Yugoslavia (YUG), Unión Soviética (SUN) y Checoslovaquia (CSK) fueron incorporados for los autores de la fuente (ver https://onlinelibrary.wiley.com/doi/10.1111/joes.12618). ",
    unidades = list("pib_per_capita" = "unidades",
                    "cambio_relativo" = "unidades")
  )


mandar_data(paste0(output_name, ".csv"), subtopico = "CRECIM", branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = "CRECIM",  branch = "dev")


