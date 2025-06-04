#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2025-04-30")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad

source("./scripts/utils/human_development_report_undp_api.R")


countries <- hdr_api.get_metadata(apikey = Sys.getenv("hdr_key"),
                                  entityName = "Countries")

codigos_agragaciones <- c('ZZA.VHHD', 'ZZB.HHD', 'ZZC.MHD', 'ZZD.LHD', 'ZZE.AS', 'ZZF.EAP', 'ZZG.ECA', 'ZZH.LAC', 'ZZI.SA', 'ZZJ.SSA', 'ZZK.WORLD')

hdr_indictators <- hdr_api.get_metadata(apikey = Sys.getenv("hdr_key"),
                                        entityName = "Indicators")

hdr_indicator <- hdr_indictators[hdr_indictators$code == "le",]

area_codes <- c(countries$code, codigos_agragaciones)

current_year <- year(Sys.Date())

last_year <- current_year - 1


content <- hdr_api.get_data(apikey = Sys.getenv("hdr_key"),
                            country_code = area_codes,
                            year = 1950:last_year,
                            indicator = hdr_indicator$code)



df_raw <- content$data
url <- content$url


download_filename <- glue::glue("HDR_{hdr_indicator$code}.csv")

df_raw %>% write_csv_fundar(., glue::glue("{tempdir()}/{download_filename}"))

nombre <- glue::glue("{hdr_indicator$name}. Human Development Report (Revision {current_year})")

# agregar_fuente_raw(nombre = nombre,
#                    url = url,
#                    institucion = "United Nations Development Programme",
#                    actualizable = T,
#                    fecha_actualizar = as.character(fecha_actualizar),
#                    path_raw = download_filename,
#                    script = code_name,
#                    api = T
# )

actualizar_fuente_raw(id_fuente = 283,
                      fecha_actualizar = as.character(fecha_actualizar),
                      path_raw = download_filename,
                      script = code_name
)
