
library(jsonlite)
library(httr)
library(tibble)

# WTO


link <- glue::glue("https://api.wto.org/timeseries/v1/data?i=ITS_CS_AX6&ps=2017-2024&subscription-key={Sys.getenv('WTO_API')}")


df <- jsonlite::fromJSON(txt = link, 
                         encoding = "latin1")


resp <- httr::GET(url = link)


resp_char <- rawToChar(resp$content)

parsed <- jsonlite::fromJSON(resp_char, flatten = T)

df_tibble <- tibble::as_tibble(parsed$Dataset)

argendataR::write_csv_fundar(x = df_tibble, file = glue::glue('{tempdir()}/wto_composicion_exportaciones_servicios_EBOPS_2digitos_agrupado.csv'))



#agregar_fuente_raw(url = 'https://api.wto.org/timeseries/v1/data?i=ITS_CS_AX6&ps=2017-2024',
#                   institucion = "WTO", actualizable = T,
#                   fecha_descarga = Sys.Date(),path_raw = 'wto_composicion_exportaciones_servicios_EBOPS_2digitos_agrupado.csv' ,
#                   script = "descarga_wto_commercial_service_exports_sector_partner.R",
#                   nombre = "Commercial services exports by sector and partner – annual"
#)

actualizar_fuente(id_fuente = 'R94C0' , fecha_actualizar = Sys.Date())