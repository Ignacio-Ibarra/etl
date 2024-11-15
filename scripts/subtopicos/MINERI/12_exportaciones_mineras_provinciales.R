################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "MINERI"
output_name <- "exportaciones_mineras_provinciales"
analista = "Kevin Corfield"
fuente1 <- "R268C137" 
fuente2 <- "R274C143"
fuente3 <- "R84C0"


df_siacam <- arrow::read_parquet(argendataR::get_clean_path(fuente1)) 

df_opex <- arrow::read_parquet(argendataR::get_clean_path(fuente2)) 

dicc_prov <- readr::read_csv(argendataR::get_raw_path(fuente3)) %>% distinct(provincia_id = prov_cod, provincia = prov_desc) %>% 
  mutate(provincia = ifelse(provincia == "Santa Fe", "Santa Fé", provincia)) 


expo_minera_prov_df <- df_siacam %>% 
  dplyr::filter(!(provincia %in% c('sin dato', 'sin nombre', 'Indeterminado'))) %>%  
  group_by(anio = anyo, prov, provincia) %>% 
  summarise(valor_expo_minera = sum(fob, na.rm = T)/1000000) %>% 
  ungroup() %>% 
  arrange(provincia) %>% 
  mutate(
    provincia = case_when(
      provincia == "Capital Federal" ~ "CABA",
      provincia == "Cordoba" ~ "Córdoba",
      provincia == "Entre Rios" ~ "Entre Ríos",
      provincia == "Neuquen" ~ "Neuquén",
      provincia == "Rio Negro" ~ "Río Negro",
      provincia == "Santiago Del Estero" ~ "Santiago del Estero",
      provincia == "Tierra Del Fuego" ~ "Tierra del Fuego",
      provincia == "Tucuman" ~ "Tucumán",
      provincia == "Santa Fe" ~ "Santa Fé",
      TRUE ~ provincia
    )
  )

expo_total_prov_df <- df_opex %>% 
  dplyr::filter(rubro == "Total rubros") %>% 
  select(-rubro) %>% 
  arrange(provincia) 
# %>% 
#   mutate(
#     provincia_nueva = case_when(
#       provincia == "Ciudad Autónoma de Buenos Aires" ~ "CABA",
#       provincia == "Santa Fe" ~ "Santa Fé",
#       provincia == "Entre Rios" ~ "Entre Ríos",
#       TRUE ~ provincia
#     ))

df_output <- expo_minera_prov_df  %>% 
  left_join(expo_total_prov_df , join_by(anio, provincia)) %>% 
  mutate(
    participacion_minera = 100*valor_expo_minera / valor_expo
  ) %>% 
  select(anio, provincia, participacion_minera) %>% 
  left_join(dicc_prov, join_by(provincia))



df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  mutate(provincia = tools::toTitleCase(str_replace_all(provincia, "_", " ")),
         anio = as.integer(anio)) %>% 
  group_by(anio, provincia) %>% 
  mutate(
    participacion_minera = 100* fob / sum(fob, na.rm = T)
  ) %>% 
  ungroup() %>% 
  filter(exportaciones == "mineras") %>% 
  select(anio, provincia, participacion_minera) %>% 
  left_join(dicc_prov, join_by(provincia))
  

comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("anio", "provincia_id"), # variables pk del dataset para hacer el join entre bases
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
  variable_nombre = c("participacion_minera",
                      "provincia_id"),
  descripcion = c("Participación de la minería en el total de las exportaciones provinciales, en precios corrientes",
                  "Codigo de provincia, según INDEC")
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
    analista = analista,
    pk = c("anio", "provincia_id"),
    es_serie_tiempo = T,
    control = comparacion, 
    columna_geo_referencia = 'provincia_id',
    columna_indice_tiempo = 'anio',
    descripcion_columnas = descripcion,
    cambio_nombre_cols = list("participacion_minera" = "fob"),
    unidades = list("participacion_minera" = "porcentaje"),
    aclaracion = "Se cambiaron levemente los nombres de las provincias, porque no llevaban tildes. Se cambio el nombre de la variable indicador 'participacion' = 'fob', se quitó la columna 'exportaciones' que incluia dos valores `c('mineras','resto')`. La idea es reflejar la importancia de las exportaciones mineras en el total exportado. Se agregaron las demás provincias y se incorporó el id de la provincia según indec. "
  )



