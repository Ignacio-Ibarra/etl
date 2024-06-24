# Este script es especial porque trabaja con una API para la que hay que descargar
# un df por año (2005 a año_actual: aprox 450MB para 18 años)
# para ello se hacen consultas, se guardan en server en formato parquet


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)



# WTO - descarga fuente raw desde API 

library(glue)
library(httr)
library(jsonlite)
library(tibble)


# PARAMS ---- 

wto_path <- Sys.getenv("WTO_DIR")


year_range <- 2005:2022 # descarga de bases de años iterada. Start = 2005 , End = 2024 (current year)

# URL base para llamada a API - year como parametro
base_url <- "https://api.wto.org/timeseries/v1/data?i=ITS_CS_AX6&ps={year}&max=900000&subscription-key={Sys.getenv('WTO_API')}"


# scrpaer API  ----

get_wtc_api <- function(year) {
  url <- glue(base_url)
  resp <- httr::GET(url)
  resp_char <- rawToChar(resp$content)
  parsed <- jsonlite::fromJSON(resp_char, flatten = TRUE)
  df_tibble <- tibble::as_tibble(parsed$Dataset)
  return(df_tibble)
}

# Container de data frames por año
data_list <- list()

# Iteración - rango años y scraper
for (year in year_range) {
  df <- get_wtc_api(year)
  data_list[[as.character(year)]] <- df
  
  print(glue::glue("Descargado año {year}"))
}


data_list

# Combiado en un unico data.frame
combined_df <- bind_rows(data_list, .id = "year")

combined_df %>% 
  dplyr::group_by(year) %>% 
  arrow::write_dataset(path = wto_path, 
                       format = 'parquet')



# Escribir archivo en TEMP
# argendataR::write_csv_fundar(x = df_tibble, file = glue::glue('{tempdir()}/wto_composicion_exportaciones_servicios_EBOPS_2digitos_agrupado.csv'))


raw_file <- "wto_composicion_exportaciones_servicios_EBOPS_2digitos_agrupado.csv"


df_tibble <- glue::glue("* Esta archivo reemplaza fuente raw que se encuentra alojada en {wto_path}") %>% as_tibble()  
 
  
  # Escribir archivo en TEMP
 argendataR::write_csv_fundar(x = df_tibble, file = glue::glue('{tempdir()}/{raw_file}'))
  
  


# Descarga y actualización de fuente RAW

#agregar_fuente_raw(url = 'https://api.wto.org/timeseries/v1/data?i=ITS_CS_AX6&ps=2017-2024',
#                   institucion = "WTO", actualizable = T,
#                   fecha_descarga = Sys.Date(),path_raw = 'wto_composicion_exportaciones_servicios_EBOPS_2digitos_agrupado.csv' ,
#                   script = "descarga_wto_commercial_service_exports_sector_partner.R",
#                   nombre = "Commercial services exports by sector and partner – annual"
#)

actualizar_fuente(id_fuente = 'R94C0' , fecha_actualizar = Sys.Date())