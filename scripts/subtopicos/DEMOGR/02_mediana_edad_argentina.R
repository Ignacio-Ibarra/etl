# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "DEMOGR"
output_name <- "mediana_edad_argentina.csv"
analista <- "Rafael Rofman & Ximena de la Fuente"

fuente1 <- 'R432C278'
fuente2 <- 'R433C279'

df_indec <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

df_wpp <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet(.)

df_censos <- df_indec %>% 
  group_by(anio = censo, sexo, rango_etario = edad) %>% 
  summarise(poblacion = sum(poblacion, na.rm = T)) %>% 
  ungroup() 


df_mediana_censos <- df_censos  %>% 
  mutate(
    Li = as.numeric(str_extract(rango_etario, "^[0-9]+")),          # límite inferior
    Ls = as.numeric(str_extract(rango_etario, "(?<=-)[0-9]+")),     # límite superior
    w  = Ls - Li                                                   # ancho del intervalo
  ) %>%
  group_by(anio) %>%
  arrange(Li, .by_group = TRUE) %>%
  mutate(
    N = sum(poblacion),                         # total población por año
    FA = cumsum(poblacion),                     # frecuencia acumulada
    FA_prev = lag(FA, default = 0)              # acumulada anterior
  ) %>%
  # identificar el intervalo que contiene la mediana (N/2)
  filter(FA >= N/2) %>%
  slice(1) %>%
  mutate(
    edad_mediana = Li + ((N/2 - FA_prev) / poblacion) * w,
    fuente = "INDEC"
  ) %>%
  select(anio, edad_mediana, fuente)

df_mediana_wpp <- df_wpp %>% 
  dplyr::filter(iso3_code == "ARG") %>% 
  mutate(fuente = "World Population Prospects (UN)" ) %>% 
  select(anio = time, edad_mediana = median_age_pop, fuente) 

df_output <- df_mediana_censos %>%
  dplyr::filter(anio < 1950) %>% 
  bind_rows(df_mediana_wpp)


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico)


pks <- c('anio')

comparacion <- argendataR::comparar_outputs(
  df = df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = pks
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
  dplyr::filter(grepl(paste0("^", output_name), nombre_archivo)) %>% 
  distinct(variable_nombre, descripcion) 



# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output



descripcion <- armador_descripcion(metadatos = metadatos,
                                   # etiquetas_nuevas = etiquetas_nuevas,
                                   output_cols = output_cols)



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    control = comparacion, 
    fuentes = argendataR::colectar_fuentes(),
    analista = analista,
    pk = pks,
    descripcion_columnas = descripcion, 
    unidad = list("edad_mediana" = "unidades"))


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "main")


