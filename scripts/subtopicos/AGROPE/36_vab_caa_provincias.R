#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "AGROPE"
output_name_old <- "drawing_data.geojson"
output_name <- "vab_caa_provincias.csv"
analista = "Franco A. Mendoza y Kevin Corfield"
fuente1 <- "R305C173" # Subsecretaría de Programación Regional y Sectorial Cadenas Productivas 


df_sprys <- arrow::read_parquet(argendataR::get_clean_path(fuente1)) 

total_vab <- df_sprys %>% 
  filter(alcance_nombre == "Argentina", categoria_desc == "Agroindustriales") %>% 
  select(cadena_id = cadenas_id, total_cadena = valor)

df_output <- df_sprys %>%
  dplyr::filter(alcance_nombre != "Argentina", categoria_desc == "Agroindustriales") %>% 
  select(provincia_id = alcance_id, provincia = alcance_nombre, cadena_id =  cadenas_id, cadena = cadenas_desc, vab = valor) %>% 
  left_join(total_vab, join_by(cadena_id)) %>% 
  mutate(
    share = round(100 * vab / total_cadena,2)
  ) %>% 
  select(provincia_id, provincia, cadena, share)


# require(sf)
# 
# provs <- sf::read_sf("provincia.json")
# 
# provincias_geom <- provs %>%
#   mutate(provincia_id = as.integer(in1)) %>%
#   select(provincia_id, geometry) %>%
#   right_join(df_output, join_by(provincia_id))
# 
# 
# mi_paleta <- scale_fill_gradient(low = "#FFCC9E", high = "#24693D")
# 
# # Crear archivo PDF
# pdf("mapas_provincias.pdf", width = 10, height = 8)  # Ajusta el tamaño del gráfico según necesites
# 
# # Iterar sobre cada valor único de la variable 'cadena'
# valores_cadena <- unique(provincias_geom$cadena)
# 
# for (cadena in valores_cadena) {
#   # Filtrar los datos para la cadena actual
#   data_filtrada <- provincias_geom %>% filter(cadena == !!cadena)
#   
#   # Generar el gráfico para la cadena actual
#  gg <-  ggplot() +
#     geom_sf(data = data_filtrada %>% filter(provincia_id != 94),
#             aes(fill = share)) +
#     mi_paleta +
#     labs(
#       title = paste("Mapa para", cadena),
#       fill = "Participación provincial (%)"
#     ) +
#     theme_minimal() +
#     theme(plot.title = element_text(hjust = 0.5)) # Centra el título
#   
#   print(gg)
#   # La salida del gráfico se almacena automáticamente en una página del PDF
# }
# 
# # Cerrar el archivo PDF
# dev.off()

# Cargo datos utilizados por los analistas 
df_anterior <- read.csv("CAA_provincias.csv") %>%
  select(provincia_id, cadena, valor) %>% 
  mutate(valor = as.numeric(valor))


comparable_df <- df_output %>% 
  select(provincia_id, cadena, valor = share) %>% 
  mutate(cadena = tolower(cadena))


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = comparable_df,
  nombre = output_name,
  pk = c('provincia_id','cadena'), # variables pk del dataset para hacer el join entre bases
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
  dplyr::filter(grepl(paste0("^", output_name_old), dataset_archivo)) %>% 
  distinct(variable_nombre, descripcion) 


# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output

etiquetas_nuevas <- data.frame(
  variable_nombre = c("provincia_id", 
                      "provincia",
                      "cadena",
                      "share"),
  descripcion = c("Número identificador de provincia",
                  "Provincia",
                  "Cadena agroinudstial",
                  "Participacion del VAB de la cadena provincial en el VAB nacional de la cadena")
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


aclaracion = c("Se pasó de un archivo json a un archivo csv",
               "Se utilizó una fuente de información distinta: los datos inicialmente fueron tomados de un excel no publico del Laboratorio de Desarrollo Sectorial y Territorial de la FCE-UNLP. En esta versión, se tomaron datos publicados en la web del MECON aunque difieren de los iniciales",
               "La modificación de la fuente trae aparejada una modificación sustancial de los nombres de las cadenas y los valores arrojados, es por ello que la comparación da muy mal",
               "Se modificó el nombre del archivo, de drawing_data.geojson a vab_caa_provincias.csv")

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = colectar_fuentes(),
    analista = analista,
    pk = c('provincia_id', 'cadena'),
    control = comparacion, 
    cambio_nombre_output = list('nombre_nuevo' = output_name, 'nombre_anterior' = output_name_old),
    cambio_nombre_cols = list("share" = "valor"),
    descripcion_columnas = descripcion,
    unidades = list("share" = "porcentaje"),
    aclaraciones = paste0(aclaracion, collapse = ". ")
  )