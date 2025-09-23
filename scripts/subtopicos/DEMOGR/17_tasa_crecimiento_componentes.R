# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "DEMOGR"
output_name <- "tasa_crecimiento_componentes.csv"
analista <- "Rafael Rofman & Ximena de la Fuente"

fuente1 <- 'R442C0' # Lattes, A. E., Recchini de Lattes, Z. L. (1975). La Población de Argentina. Cuadro 2.2
fuente2 <- 'R443C0' # Lattes, A. E., Recchini de Lattes, Z. L. (1975). La Población de Argentin. Cuadro 2.1
fuente3 <- 'R433C279' # World Population Proscpects - Demographic Indicators. 1950-2100, medium. CSV format


df_lattes_cuadro22<- argendataR::get_raw_path(fuente1) %>% 
  read.csv(.)

df_lattes_cuadro21 <- argendataR::get_raw_path(fuente2) %>% 
  read.csv(.)

df_wpp <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet()


df_saldos_lattes <- df_lattes_cuadro22 %>%
  mutate(saldo_migratorio_medio = 1000*migratorio_miles/5,
         saldo_vegetativo_medio = 1000*vegetativo_miles/5,
         anio = as.integer(str_extract(periodo, "^[0-9]{4}"))) %>% 
  select(anio, saldo_migratorio_medio, saldo_vegetativo_medio) 


df_poblacion_lattes <- df_lattes_cuadro21 %>% 
  mutate(poblacion_media = 1000*(poblacion_total + lead(poblacion_total))/2)  %>% 
  select(anio, poblacion_media) 

df_lattes_rates <- df_saldos_lattes %>% 
  left_join(df_poblacion_lattes, join_by(anio)) %>% 
  mutate(tasa_migracion_neta = 100* saldo_migratorio_medio / poblacion_media, 
         tasa_crecimiento_vegetativo = 100 * saldo_vegetativo_medio / poblacion_media,
         fuente = "Lattes et al (1975)") %>% 
  select(anio, tasa_migracion_neta,tasa_crecimiento_vegetativo)


df_wpp_rates <- df_wpp %>% 
  dplyr::filter(iso3_code == "ARG",
                time <= year(Sys.Date())) %>% 
  select(anio = time, pob = t_population1july, net_migrations, nat_change, pop_change) %>% 
  mutate(
    tasa_migracion_neta = 100 * net_migrations / pob,
    tasa_crecimiento_vegetativo = 100 * nat_change / pob
  ) %>% 
  select(anio, tasa_migracion_neta, tasa_crecimiento_vegetativo)


df_output <- df_lattes_rates %>% 
  dplyr::filter(anio < min(df_wpp_rates$anio)) %>% 
  bind_rows(df_wpp_rates)


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
    unidad = list("tasa_migracion_neta" = "porcentaje", 
                  "tasa_crecimiento_vegetativo" = "porcentaje"))


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "main")

