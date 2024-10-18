################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "ESTPRO"
output_name <- "va_por_trabajador_pais"
analista = "Gisella Pascuariello"

fuente1 <- "R233C104"
fuente2 <- "R234C0"
fuente3 <- "R227C0"




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


# Traducción al castellano de Gise Pascuariello 
descriptores <- haven::read_dta(get_raw_path(fuente2)) 

# Me lo traigo pero no es una fuente utilizada
icio_tables <-  arrow::read_parquet(get_clean_path("R226C96")) 


# Traigo nomenclador desde google drive
geonomenclador <- argendataR::get_nomenclador_geografico() %>%
  select(iso3 = codigo_fundar, pais_nombre = desc_fundar, nivel_agregacion)

df_clean <- arrow::read_parquet(get_clean_path(fuente1)) %>% 
  mutate(anio = as.integer(anio)) %>% 
  rename(letra = codigo_act) %>% 
  dplyr::filter(letra %in% LETTERS)



# reemplazo codigo de países
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
  "ZNAM" = "NAFTA_ESTPRO", #Agregado al geonomenclador.json
  "NAFTA" = "NAFTA_ESTPRO",
  "WXOECD" = "NONOECD",
  "W_O" = "ZOTH"
)



base <- df_clean %>% 
  select(-desc_act) %>% 
  left_join(descriptores %>% distinct(letra, letra_desc_abrev = letra_desc), join_by(letra)) %>% 
  dplyr::filter(!is.na(valor_usd_por_emp)) %>% 
  mutate(iso3 = recode(iso3, !!!reemplazos_codigos)) %>%
  left_join(geonomenclador, join_by(iso3))

base[base$iso3 == "NAFTA_ESTPRO", c("pais_nombre")] <- "Países miembros del NAFTA"
base[base$iso3 == "F_ESTPRO", c("pais_nombre")] <- "África"


argentina_df <- base %>% dplyr::filter(iso3 == "ARG") %>% select(anio, letra, indice = valor_usd_por_emp) 

df_output <- base %>% 
  left_join(argentina_df, join_by(anio, letra)) %>% 
  mutate(
    valor_relativo_arg = 100 * valor_usd_por_emp / indice
  ) %>% 
  select(-valor_usd_por_emp, -indice) %>% 
  dplyr::filter(valor_relativo_arg >= 0) # borro un registro raro de Malta. 


# Modifico para que coincida con el nuevo formato
df_anterior <- argendataR::descargar_output(nombre =output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega") %>% 
   mutate(iso3 = recode(iso3, !!!reemplazos_codigos),
          anio = as.integer(anio))  %>% 
  select(-es_agregacion, -iso3_desc_fundar) %>% 
  left_join(geonomenclador, join_by(iso3))
  

df_anterior[df_anterior$iso3 == "NAFTA_ESTPRO", c("pais_nombre")] <- "Países miembros del NAFTA"
df_anterior[df_anterior$iso3 == "F_ESTPRO", c("pais_nombre")] <- "África"


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  pk = c("anio","iso3", "letra"), # variables pk del dataset para hacer el join entre bases
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
                      "nivel_agregacion"),
  descripcion = c("Nombre de país o agregación de países",
                  "Describe si el registro refiere a un 'pais' o 'agregacion' de países")
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
    control = comparacion, 
    pk = c("anio", "iso3", "letra"),
    es_serie_tiempo = T,
    columna_indice_tiempo = 'anio',
    columna_geo_referencia = 'iso3',
    cambio_nombre_cols = list('pais_nombre' = 'iso3_desc_fundar', 'nivel_agregacion' = 'es_agregacion'),
    descripcion_columnas = descripcion,
    unidades = list("valor_relativo_arg" = "unidades")
  )


# joined_df <- comparacion$joined_df
# 
# # Conozco cuales son mis primary keys
# primary_keys <- c('anio','iso3','letra')
# 
# # Genero una columna de etiquetas para el tooltip, con las columnas que son pk
# joined_df$label <- apply(joined_df[primary_keys], 1, function(row) paste(row, collapse = " - "))
# 
# # Conozco la columna numérica que quiero comparar.
# col_comparar <- 'valor_relativo_arg'
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
