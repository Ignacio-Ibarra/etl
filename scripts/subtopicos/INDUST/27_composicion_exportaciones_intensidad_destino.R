#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

library(data.table)
library(dplyr)

subtopico <- "INDUST"
output_name <- "composicion_exportaciones_intensidad_destino.csv"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R456C0' # BACI HS02
fuente2 <- 'R458C298' # Clasificador HS02 a Lall

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long)

hs02_lall <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet() %>% 
  select(hs02, lall_code) %>% 
  filter(!lall_code %in% c('Otros')) 
  
source("scripts/utils/baci_data.R")


con <- argendataR::get_raw_path(fuente1) %>% 
  BACI.get_db_from_zip(.)

dbWriteTable(con, 
             "hs_to_lall", 
             hs02_lall, 
             overwrite = TRUE)


query_output <- glue::glue(
  "SELECT t as anio, c.i as exporter_code, c.j as importer_code, p.country_iso3 as importer_iso3, h.lall_code, SUM(c.v) as expo
   FROM comercio as c
   LEFT JOIN hs_to_lall as h ON h.hs02 = c.k
   LEFT JOIN paises as p ON c.j = p.country_code
   where c.i = 32
   GROUP BY t, c.i, c.j, p.country_iso3, h.lall_code"
)


df_query <- dbGetQuery(con, query_output) %>% 
  setDT(.)


paises_ue <- c("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "ESP", 
               "EST", "FIN", "FRA", "GRC", "HRV", "HUN", "IRL", "ITA", 
               "LTU", "LUX", "LVA", "MLT", "NLD", "POL", "PRT", "ROU", 
               "SVK", "SVN", "SWE")

              
df_paises <- df_query %>% 
  dplyr::filter(!is.na(lall_code)) %>% 
  left_join(geo_front, join_by(importer_iso3 == geocodigoFundar)) %>% 
  group_by(anio, importer_iso3, geonombreFundar) %>% 
  mutate(prop = 100 * expo / sum(expo)) %>% 
  ungroup() %>% 
  mutate(clasificacion_lall = case_when(lall_code == 'PP' ~ 'Productos primarios',
                                        lall_code == 'MRRNN' ~ 'Manufacturas en basadas en RRNN',
                                        lall_code == 'MBT' ~ 'Manufacturas de baja tecnología',
                                        lall_code == 'MMT' ~ 'Manufacturas de media tecnología',
                                        lall_code == 'MAT' ~ 'Manufacturas de alta tecnología')) %>% 
  select(anio, importer_iso3, geonombreFundar, clasificacion_lall, expo, prop)

df_ue <- df_query %>% 
  dplyr::filter(!is.na(lall_code), importer_iso3 %in% paises_ue) %>% 
  group_by(anio, importer_iso3 = "EUU", geonombreFundar = "Unión Europea", lall_code) %>% 
  summarise(expo = sum(expo)) %>% 
  ungroup() %>% 
  group_by(anio, importer_iso3, geonombreFundar) %>% 
  mutate(prop = 100 * expo / sum(expo)) %>% 
  ungroup() %>% 
  mutate(clasificacion_lall = case_when(lall_code == 'PP' ~ 'Productos primarios',
                                        lall_code == 'MRRNN' ~ 'Manufacturas en basadas en RRNN',
                                        lall_code == 'MBT' ~ 'Manufacturas de baja tecnología',
                                        lall_code == 'MMT' ~ 'Manufacturas de media tecnología',
                                        lall_code == 'MAT' ~ 'Manufacturas de alta tecnología')) %>% 
  select(anio, importer_iso3, geonombreFundar, clasificacion_lall, expo, prop)

df_output <- bind_rows(df_paises, df_ue)


rm(df_query)
gc()

pks <- c("anio", "importer_iso3", "geonombreFundar", "clasificacion_lall")

df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico, branch = "main") 


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
    unidad = list("expo" = "miles de dólares corrientes", "prop" = "porcentaje"))

output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "main")
