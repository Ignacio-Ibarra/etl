#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


fecha_actualizar <- "Sin informacion"

source("scripts/utils/atlas_of_economic_complexity_harvard_api.R")

url_consultada = "https://atlas.hks.harvard.edu/explore/overtime?productClass=SITC&product=product-SITC-656&endYear=2021&view=markets&exporter=group-1&locationLevel=country&layout=share&ordering=totals"

countries_df <- ATLAS_OF_ECONOMIC_COMPLEXITY.get_countries_df()


download_filename <- glue::glue("atlas_of_economic_complexity_country_codes.csv")

destfile <- glue::glue("{tempdir()}/{download_filename}")

countries_df %>% argendataR::write_csv_fundar(., destfile)

nombre = glue::glue("International Trade Data - Codigos de Países")
institucion = "The Growth Lab at Harvard University - Harvard Dataverse"

# agregar_fuente_raw(url = url_consultada,
#                    nombre = nombre,
#                    institucion = institucion,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar)


actualizar_fuente_raw(id_fuente = 302,
                      nombre = nombre,
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)
