#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

source("./scripts/utils/human_development_report_undp_api.R")

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

next_update <- function(n_months){
  periodicidad <- months(n_months)
  fecha_actual <- Sys.Date()
  ultima_fecha_mes <- lubridate::ceiling_date(fecha_actual, "month") - days(1)
  fecha_actualizar <- ultima_fecha_mes + periodicidad
  return(fecha_actualizar)
  
}

periodicidad <- months(12)
fecha_actualizar <- as.Date("2025-04-30")


countries <- hdr_api.get_metadata(apikey = Sys.getenv("hdr_key"),
                                  entityName = "Countries")

hdr_regions <- hdr_api.get_metadata(apikey = Sys.getenv("hdr_key"),
                                    entityName = "HDRegions")

hdr_groups <- hdr_api.get_metadata(apikey = Sys.getenv("hdr_key"),
                                   entityName = "HDGroups")

hdr_indictators <- hdr_api.get_metadata(apikey = Sys.getenv("hdr_key"),
                                        entityName = "Indicators")

hdr_indicator <- hdr_indictators[hdr_indictators$code == "mys",]

area_codes <- c(countries$code, hdr_regions$code, hdr_groups$code)

content <- hdr_api.get_data(apikey = Sys.getenv("hdr_key"),
                       country_code = area_codes,
                       year = 1950:2023,
                       indicator = hdr_indicator$code)

df_raw <- content$data
url <- content$url


download_filename <- glue::glue("HDR_{hdr_indicator$code}.csv")

df_raw %>% write_csv_fundar(., glue::glue("{tempdir()}/{download_filename}"))

# agregar_fuente_raw(nombre = glue::glue("{hdr_indicator$name}. Human Development Report (Revision 2024)"),
#                    url = url,
#                    institucion = "United Nations Development Programme",
#                    actualizable = T,
#                    fecha_actualizar = as.character(fecha_actualizar),
#                    path_raw = filename,
#                    script = code_name,
#                    api = T
# )

actualizar_fuente_raw(id_fuente = 216,
                      fecha_actualizar = as.character(fecha_actualizar),
                      path_raw = download_filename
)
