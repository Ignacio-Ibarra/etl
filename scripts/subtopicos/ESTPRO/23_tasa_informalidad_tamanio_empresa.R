################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "ESTPRO"
output_name <- "tasa_informalidad_tamanio_empresa"
analista <- "Gisella Pascuariello"
codigos.eph <- fuentes_raw() %>% filter(grepl("Encuesta Permanente de Hogares, Individual*", nombre)) %>% select(nombre, codigo) 


# librerías


#-- Lectura de Datos ----

current_year <- year(Sys.Date())

anios <- 2003:2023


get_clean_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}clean/")
  df_fuentes_clean <- fuentes_clean() 
  path_clean <- df_fuentes_clean[df_fuentes_clean$codigo == codigo,c("path_clean")]
  return(paste0(prefix, path_clean))
}


get_raw_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}raw/")
  df_fuentes_raw <- fuentes_raw() 
  path_raw <- df_fuentes_raw[df_fuentes_raw$codigo == codigo,c("path_raw")]
  return(paste0(prefix, path_raw))
}

###################################

# Creo una función custom para aplicar un determinado wrangling a cada dataset de EPH
eph_tasa_informalidad_tamanio_empresa <- function(eph_data) {
  outdf <- eph_data %>%
    select(anio = ano4, pp04c, estado, cat_ocup, pp07h, pondera) %>% 
    mutate(tamanio_cod = 
             case_when(
               pp04c<6 & pp04c>0 ~ 1,
               pp04c==6 ~ 2,
               pp04c==7 ~ 3,
               pp04c==8 ~ 4,
               pp04c==9 ~ 5,
               pp04c>9 & pp04c<99 ~ 6,
               TRUE ~ NA_real_
             ),
           tamanio_desc = 
             case_when(
               tamanio_cod == 1 ~ "Hasta 5 ocupados",
               tamanio_cod == 2 ~ "Entre 6 y 10 ocupados",
               tamanio_cod == 3 ~ "Entre 11 y 25 ocupados",
               tamanio_cod == 4 ~ "Entre 26 y 40 ocupados",
               tamanio_cod == 5 ~ "Entre 41 y 100 ocupados",
               tamanio_cod == 6 ~ "Más de 100 ocupados",
               TRUE ~ NA_character_
            ),
           asal_informal = case_when(
             estado==1 & cat_ocup==3 & pp07h==2 ~ 1,
             estado==1 & cat_ocup==3 & pp07h==1 ~ 0,
             TRUE ~ NA_integer_
           )
           ) %>% 
    dplyr::filter(!is.na(tamanio_cod)) %>% 
    group_by(anio, tamanio_cod, tamanio_desc) %>% 
    summarise(
      tasa_informalidad = stats::weighted.mean(asal_informal, pondera, na.rm=TRUE)
    ) %>% 
    select(anio, tamanio_cod, tamanio_desc, tasa_informalidad)
  
  return(outdf)
}

get_eph_fuente <- function(year, codes_and_names) {
  fuente <- codigos.eph%>% filter(grepl(year, nombre)) %>% select(codigo) %>% pull()
  fuente
}

# Creo una función que levanta el dataset correspondiente a un año
load_eph_by_year <- function(year, codes_and_names){
  fuente <- get_eph_fuente(year, codes_and_names)
  eph_df <- data.table::fread(argendataR::get_raw_path(fuente))
  return(eph_df)
}

# Creo una función de cleaning de cada archivo
cleaning_eph <- function(eph_data){
  colnames(eph_data) <- tolower(colnames(eph_data))
  return(eph_data)
}


# Creo una función que procesa una lista de años 
eph_processing <- function(years, codes_and_names, custom_wrangling){
  result_processing <- data.table()
  for (year in years){
    cat(sprintf("Archivo EPH (%s)... empezando\n", year))
    
    # Cargo archivo
    eph_df <- load_eph_by_year(year = year, codes_and_names = codes_and_names)
    dimensiones <- dim(eph_df)
    cat(sprintf("... cargando %s filas y %s columnas \n", dimensiones[1], dimensiones[2]))
    
    # Hago cleaning
    eph_df <- cleaning_eph(eph_data = eph_df)
    cat("... limpieza\n")
    
    # Hago wrangling
    result_df <- custom_wrangling(eph_data = eph_df)
    cat("... procesado\n")
    rm(eph_df)
    
    result_processing <- rbind(result_processing, result_df, fill=F)
    cat("... finalizado\n")
    
    
  }
  return(result_processing)
}

#-- Procesamiento ----

df_output <- eph_processing(years = anios, codes_and_names = codigos.eph, custom_wrangling = eph_tasa_informalidad_tamanio_empresa)



###################################



df_anterior <- argendataR::descargar_output(nombre =output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega") %>% 
  mutate( anio = as.integer(anio))

comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  pk = c("anio", "tamanio_cod"), # variables pk del dataset para hacer el join entre bases
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

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = codigos.eph$codigo,
    analista = analista,
    control = comparacion, 
    pk = c("anio", "tamanio_cod"),
    es_serie_tiempo = F,
    descripcion_columnas = descripcion,
    unidades = list("tasa_informalidad" = "unidades")
    )


# joined_df <- comparacion$joined_df
# 
# # Conozco cuales son mis primary keys
# primary_keys <- c('anio','tamanio_cod')
# 
# # Genero una columna de etiquetas para el tooltip, con las columnas que son pk
# joined_df$label <- apply(joined_df[primary_keys], 1, function(row) paste(row, collapse = " - "))
# 
# # Conozco la columna numérica que quiero comparar.
# col_comparar <- 'tasa_informalidad'
# cols_comp <- paste0(col_comparar, c('.x','.y'))
# col_x <- cols_comp[1]
# col_y <- cols_comp[2]
# 
# # Genero scatter de ggplot
# scatter <-  ggplot(joined_df, aes(x = !!sym(col_x), y = !!sym(col_y), label = label))+
#   geom_point() +
#   geom_abline(slope = 1,color = "red", alpha = 0.7) +
#   theme_minimal() +
#   labs(title = "Scatterplot dinámico", x = cols_comp[1], y = cols_comp[2])
# 
# # Agregar interactividad con plotly
# interactive_scatter <- plotly::ggplotly(scatter, tooltip = 'label')
# 
# # Mostrar el scatter interactivo
# interactive_scatter
