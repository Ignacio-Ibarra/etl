# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "DEMOGR"
output_name <- "poblacion_arg_largo_plazo.csv"
analista <- "Rafael Rofman & Ximena de la Fuente"

fuente1 <- 'R219C90' # Maddison
fuente2 <- 'R433C279' # World Population Proscpects - Demographic Indicators. 1950-2100, medium. CSV format
fuente3 <- 'R444C0' # Lattes

df_madd <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

df_wpp <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet()

df_lattes <- argendataR::get_raw_path(fuente3) %>% 
  read.csv

df_madd_arg <- df_madd %>% 
  dplyr::filter(iso3 == "ARG") %>% 
  mutate(fuente = "Maddison Project Database" ) %>% 
  select(anio, poblacion = pop, fuente) %>% 
  drop_na(poblacion)

df_wpp_arg <- df_wpp %>% 
  dplyr::filter(iso3_code == "ARG",
                time <= year(Sys.Date())) %>% 
  mutate(poblacion = 1000*t_population1july,
         fuente = "World Population Prospects (UN)") %>% 
  select(anio = time, poblacion, fuente)

df_lattes_pob <- df_lattes %>% 
  mutate(
    anio = str_extract(quinquenio, "\\d{4}") %>% as.integer(),
    poblacion = poblacion_inicial,
    fuente = "Lattes et al (1975)") %>% 
  select(anio, poblacion, fuente)


df_output <- df_madd_arg %>% 
  dplyr::filter(anio < min(df_lattes_pob$anio)) %>% 
  bind_rows(
    df_lattes_pob %>% 
      dplyr::filter(anio < min(df_wpp_arg$anio))
  ) %>% 
  bind_rows(df_wpp_arg)

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
    unidad = list("poblacion" = "unidades"))


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "main")

