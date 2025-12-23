#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

library(data.table)
library(dplyr)

subtopico <- "INDUST"
output_name <- "importaciones_por_origen.csv"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R456C0' # BACI HS02
fuente2 <- 'R458C298' # Clasificador HS02 a Lall

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long)

hs02_lall <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet() %>% 
  select(hs02, lall_code) %>% 
  filter(!lall_code %in% c('Otros')) %>% 
  mutate(tipo_bien = if_else(lall_code == 'PP','Primarios','Manufacturas')) %>% 
  select(hs02, tipo_bien)
  
source("scripts/utils/baci_data.R")


con <- argendataR::get_raw_path(fuente1) %>% 
  BACI.get_db_from_zip(.)

dbWriteTable(con, 
             "hs_to_lall", 
             hs02_lall, 
             overwrite = TRUE)


query_output <- glue::glue(
  "SELECT t as anio, c.i as exporter_code, p.country_iso3 as exporter_iso3, c.j as importer_code, h.tipo_bien, SUM(c.v) as impo
   FROM comercio as c
   LEFT JOIN hs_to_lall as h ON h.hs02 = c.k
   LEFT JOIN paises as p ON c.i = p.country_code
   where c.j = 32
   GROUP BY t, c.i, p.country_iso3, c.j, h.tipo_bien"
)


df_query <- dbGetQuery(con, query_output) %>% 
  setDT(.)


df_output <- df_query %>% 
  dplyr::filter(!is.na(tipo_bien)) %>% 
  left_join(geo_front, join_by(exporter_iso3 == geocodigoFundar)) %>% 
  group_by(anio, tipo_bien) %>% 
  mutate(prop = 100 * impo / sum(impo)) %>% 
  ungroup() %>% 
  select(anio, exporter_iso3, geonombreFundar, tipo_bien, impo, prop)

rm(df_query)
gc()

pks <- c("anio", "exporter_iso3", "geonombreFundar", "tipo_bien")

# df_anterior <- argendataR::descargar_output(nombre = output_name,
#                                             subtopico = subtopico, branch = "main") 

df_anterior <- df_output

comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = pks, # variables pk del dataset para hacer el join entre bases
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
    unidad = list("impo" = "miles de dólares corrientes", "prop" = "porcentaje"))

output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "main")
