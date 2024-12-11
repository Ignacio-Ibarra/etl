code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(1)
fecha_ultima_actualizacion <- as.Date("2024-11-15")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad


source("scripts/utils/world_bank_commodity_markets_scraper.R")

pattern <- ".*CMO-Historical-Data-Monthly\\.xlsx"

url <- WorldBank.CommodityMarkets.extraer_links(pattern = pattern)

nombre <- "Commodity Markets. Pink Sheet. Monthly Data"

institucion <- "World Bank"

download_filename <- "CMO-Historical-Data-Monthly.xlsx"

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile, mode = "wb")

# agregar_fuente_raw(url = url,
#                    nombre = nombre,
#                    institucion = institucion,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar)

actualizar_fuente_raw(id_fuente = 271,
                      url = url,
                      nombre = nombre, 
                      institucion = institucion,
                      script = code_name,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename)