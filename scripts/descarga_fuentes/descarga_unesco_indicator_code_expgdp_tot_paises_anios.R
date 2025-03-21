# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

source("scripts/utils/unesco_api.R")


# Ejemplo de uso
indicador_seleccionado <- UNESCO.get_indicators() %>% 
  dplyr::filter(theme == "SCIENCE_TECHNOLOGY_INNOVATION") %>% 
  dplyr::filter(grepl("GERD", name))


periodicidad <- months(12)
fecha_ultima_actualizacion <- indicador_seleccionado$lastDataUpdate %>% as.Date(.)
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad  

indicator_code <- indicador_seleccionado$indicatorCode


resultado <- UNESCO.get_indicator_data(indicators = indicator_code, indicator_metadata = TRUE) # Si quiero GERD as a percentage of GDP y si quiero metadatos 


url <- resultado$url_consulta


metadatos_consulta <- resultado$response$indicatorMetadata


nombre <- metadatos_consulta$glossaryTerms[[1]]$name[[3]]

institucion <- "UNESCO Institute for Statistics (UIS)"

download_filename <- glue::glue("UNESCO_{indicator_code}.csv")

destfile <- glue::glue("{tempdir()}/{download_filename}")

df_raw <- resultado$response$records

df_raw %>% write_csv_fundar(., destfile)

# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = T
# )

actualizar_fuente_raw(id_fuente = 343,
                      url = url, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)
