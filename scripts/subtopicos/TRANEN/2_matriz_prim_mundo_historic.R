##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "TRANEN"
output_name <- "matriz_prim_mundo_historic.csv"
analista = ""

fuente1 <- "R48C0"


df_raw <- readr::read_csv(argendataR::get_raw_path(fuente1))


data <- df_raw %>% 
  filter(!is.na(entities_code))%>% 
  mutate(fuente_energia = case_when(
    name == "Other renewables (including geothermal and biomass) - TWh" ~ "Otras renovables",
    name == "Biofuels consumption - TWh" ~ "Biocombustibles",
    name == "Solar consumption - TWh" ~ "Solar",
    name == "Coal consumption - TWh" ~ "Carbon",
    name == "Gas consumption - TWh" ~ "Gas natural",
    name == "Oil consumption - TWh" ~ "Petroleo",
    name == "Wind consumption - TWh" ~ "Eolica",
    name == "Nuclear consumption - TWh" ~ "Nuclear",
    name == "Hydro consumption - TWh" ~ "Hidro",
    TRUE ~ NA_character_
    )) %>% 
  rename(valor_en_twh = valor,
         iso3 = entities_code)


df_output <- data %>% 
  select(anio, iso3, fuente_energia, valor_en_twh) %>% 
  mutate(valor_en_twh = replace_na(valor_en_twh, 0)) %>% 
  group_by(anio, iso3) %>% 
  dplyr::filter(sum(valor_en_twh)>0) %>% 
  mutate(porcentaje = 100*valor_en_twh/sum(valor_en_twh, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(tipo_energia = case_when(
    fuente_energia %in% c("Gas natural", "Carbon", "Petroleo") ~ "Sucias",
    T ~ "Limpias"
  )) 



df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega") 


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("anio", "iso3", "tipo_energia", "fuente_energia"),
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
  dplyr::filter(grepl(paste0("^", output_name), dataset_archivo)) %>% 
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
# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso


aclaracion <- paste0("Se corrigió el calculo del porcentaje")
                     

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = colectar_fuentes(),
    analista = analista,
    pk = c("anio", "iso3", "tipo_energia", "fuente_energia"),
    es_serie_tiempo = T,
    control = comparacion, 
    columna_indice_tiempo = 'anio',
    descripcion_columnas = descripcion,
    unidades = list("porcentaje" = "porcentaje",
                    'valor_en_twh' = 'twh'),
    aclaracion = aclaracion
  )


mandar_data(paste0(gsub("\\.csv$", "", output_name), ".csv"), subtopico = "TRANEN", branch = "dev")
mandar_data(paste0(gsub("\\.csv$", "", output_name), ".json"), subtopico = "TRANEN",  branch = "dev")


