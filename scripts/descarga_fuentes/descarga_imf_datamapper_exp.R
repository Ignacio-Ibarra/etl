# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


fecha_actualizar <- "Sin informacion"

source("scripts/utils/imf_datamapper.R")

indicators_result <- IMF_DATAMAPPER.get_indicators()

indicator_id <- names(indicators_result$indicators)[
  purrr::map_lgl(
    indicators_result$indicators,
    ~ !is.null(.x$label) && .x$label == "Government expenditure, percent of GDP"
  )
]


indicator_metadata <- indicators_result$indicators[[indicator_id]]

nombre <- glue::glue("{indicator_metadata$label}. {indicator_metadata$source}.")

institucion <- "International Monetary Fund"

result <- IMF_DATAMAPPER.get_data(indicator_id)

url <- result$url

df_raw <- result$data

download_filename <- glue::glue("imf_datamapper_{indicator_id}.csv")

destfile <- glue::glue("{tempdir()}/{download_filename}")

df_raw %>% argendataR::write_csv_fundar(., destfile)


# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = T
# )

actualizar_fuente_raw(id_fuente = 424,
                      url = url, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)