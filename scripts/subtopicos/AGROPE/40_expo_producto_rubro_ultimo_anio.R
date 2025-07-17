################################################################################
##                              Dataset: nombre                               ##
################################################################################

# Este script es una copia del script ~/etl/scripts/subtopicos/ACECON/7_pib_comp_va.R


#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "AGROPE"
output_name <- "expo_producto_rubro_ultimo_anio.csv"
analista = "Franco A. Mendoza y Kevin Corfield"
fuente1 <- "R335C210"

df_expo <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.) 


df_agro <- df_expo %>% 
  dplyr::filter(periodo_considerado == "Acumulado hasta diciembre", 
                anio == max(anio), 
                grepl("(PP|MOA)", gran_rubro), 
                rubro != "Minerales metalíferos, escorias y cenizas") %>% 
  mutate(division = "Agroindustrial") %>% 
  select(division, gran_rubro, rubro, anio, expo) 


df_no_agro <- df_expo %>% 
  dplyr::filter(periodo_considerado == "Acumulado hasta diciembre", 
                anio == max(anio)) %>% 
  mutate(division = "Resto no agroindustrial") %>% 
  select(division, gran_rubro, rubro, anio, expo) %>% 
  anti_join(df_agro, join_by(gran_rubro, rubro))


df_output <- bind_rows(df_agro, df_no_agro) %>% 
  mutate(share = expo / sum(expo))


# Esto lo hago como hack para poder hacer la comparación.
df_anterior <- df_output


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("rubro","anio"), # variables pk del dataset para hacer el join entre bases
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

division <- df_output %>% distinct(division) %>% pull() %>% paste0(., collapse = "', '")
grandes_rubros <- df_output %>% distinct(gran_rubro) %>% pull() %>% paste0(., collapse = ", ")

# No hay metadatos, los creo artificialmente. 
metadatos <- data.frame(
  variable_nombre = c("division",
                      "gran_rubro",
                      "rubro",
                      "anio",
                      "expo",
                      "share"),
  descripcion = c(glue::glue("Clasificación de rubro en '{division}'"),
                  glue::glue("Clasificación de grandes rubros de INDEC: '{grandes_rubros}'"),
                  "Sub-clasificación detallada dentro del 'gran_rubro'",
                  "Año seleccionado",
                  "Exportaciones en millones de dólares",
                  "Participación del rubro en el total de las exportaciones"
                  )
)

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
    fuentes = colectar_fuentes(),
    analista = analista,
    pk = c("rubro", "anio"),
    es_serie_tiempo = F,
    control = comparacion, 
    descripcion_columnas = descripcion,
    unidades = list("expo" = "millones de dólares",
                    "share" = "proporción")
  )


