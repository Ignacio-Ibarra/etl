#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2023-05-02")
fecha_actualizar <- "Sin informacion"

source("scripts/utils/censo_2022_indec_api.R")


indicadores_df <- censo_2022.get_indicator_list()

indicador <- indicadores_df[indicadores_df$id == 17, c("id", "descripcion")]

indicador_id <- indicador$id

titulo <- indicador$descripcion

download_filename <- titulo %>% janitor::make_clean_names() %>% paste0(.,".csv")

destfile <- glue::glue("{tempdir()}/{download_filename}")

result <- censo_2022.get_data(indicador_id)

df_raw <- result$data

url <- result$url

df_raw %>% write_csv_fundar(., destfile)

nombre = glue::glue("Censo Nacional de Población, Hogares y Viviendas 2022. Resultados definitivos. {titulo}")
institucion = "INDEC"

# agregar_fuente_raw(url = url,
#                    nombre = nombre,
#                    institucion = institucion,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar)


actualizar_fuente_raw(id_fuente = 447,
                      nombre = nombre, 
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename, 
                      script = code_name, 
                      api = T)