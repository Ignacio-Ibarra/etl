# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2025-03-01")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad  


source("scripts/utils/scraper_wipo.R")


indicators_list <- WIPO.get_indicators(selected_tab = 'patent')


indicator <- indicators_list$ipsIndicatorMap %>% 
  dplyr::filter(grepl(".*Total patent applications \\(direct and PCT national phase entries\\).*", label)) %>% 
  select(id = indicatorId, indicator_title = label)


report_type <- indicators_list$ipsRpTypeMap %>% 
  dplyr::filter(label == "Total count by applicant's origin") %>% 
  rename(id = value, report_title = label)

start_year <- indicators_list$ipsFYears[1]

end_year <- indicators_list$ipsToYearList


offices_codes <- WIPO.get_offices_tech(indicator = indicator$id, rp_type = report_type$id)$ipsOriginList %>% names(.) 

resultado <- WIPO.get_data_ipsOriSelValues(selected_tab = 'patent', 
                           indicator = indicator$id, 
                           report_type = report_type$id, 
                           from_year = start_year,
                           to_year = end_year,
                           offices = offices_codes,
                           tech_values = c(910,911,913,912,914))



url <- resultado$url

metadata <- resultado$output$recordInfo

json_data <- resultado$output

nombre <- glue::glue("{metadata$propertyRight}. {metadata$indicator}. {metadata$reportType}. {metadata$yearRange}. ")

institucion <- "WIPO IP Statistics Data Center"

download_filename <- glue::glue("wipo_{tolower(metadata$propertyRight)}_indicator_{indicator$id}_rptype_{report_type$id}_all_countries_all_years.json") 

destfile <- glue::glue("{tempdir()}/{download_filename}")

jsonlite::write_json(json_data, destfile)


# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = T
# )

actualizar_fuente_raw(id_fuente = 405,
                      url = url, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)