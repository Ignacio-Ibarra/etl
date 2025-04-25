#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


fecha_actualizar <- "Sin informacion"

source("scripts/utils/atlas_of_economic_complexity_harvard_api.R")

resultado <- ATLAS_OF_ECONOMIC_COMPLEXITY.list_datasets() %>% 
  dplyr::filter(tableName == "services_unilateral_country_product_year_4")

download_filename <- resultado$dvFileName

dvFileId <- resultado$dvFileId

destfile <- glue::glue("{tempdir()}/{download_filename}")

descarga <- ATLAS_OF_ECONOMIC_COMPLEXITY.download_by_id(dvFileId, destfile)

url <- descarga$url

nombre = glue::glue("The Growth Lab at Harvard University. International Trade Data - {resultado$displayName}: {resultado$repo} ({resultado$yearMin}-{resultado$yearMax})")
institucion = "Harvard Dataverse"

# agregar_fuente_raw(url = url,
#                    nombre = nombre,
#                    institucion = institucion,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar)


actualizar_fuente_raw(id_fuente = 396,
                      nombre = nombre,
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)
