# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2024-12-31")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad


source("scripts/utils/wto_timeseries_api.R")


country_codes <- wto_timeseries_api.get_reporting_economies() %>% 
  dplyr::filter(!is.na(iso3A)) %>% 
  pull(code) %>% 
  paste0(., collapse = ",")


available_years <- wto_timeseries_api.get_years() %>% 
  dplyr::filter(year < year(Sys.Date())-1, year >= 2005) %>% 
  arrange(year) %>% 
  pull(year)


indicator_code = "ITS_CS_AX6"

# Container de data frames por año
data_list <- list()

# Iteración - rango años y scraper
for (year in available_years) {
  
  df <- wto_timeseries_api.get_data_points(
    indicator_code = indicator_code,
    reporting_economies = country_codes,
    partner_economies = '000', 
    include_sub_products = TRUE,
    time_period = year
  )
  
  data_list[[as.character(year)]] <- df
  
  rows <- nrow(df)
  
  cat(glue::glue("Descargado año {year}, {rows} filas"),"\n")
}


# Combiado en un unico data.frame
df_raw <- bind_rows(data_list)


download_filename <- glue::glue("wto_indicador_{tolower(indicator_code)}_maxima_desagregacion_productos_partner_world_all_countries.csv")

destfile <- glue::glue("{tempdir()}/{download_filename}")

df_raw %>% write_csv_fundar(., destfile)

indicator_name <- df_raw %>% distinct(IndicatorCategory, Indicator)

nombre <- glue::glue("{indicator_name$IndicatorCategory}. {indicator_name$Indicator}. Todos los países. Partner: Mundo. Desde 2005")

url_api <- "https://apiportal.wto.org/"

institucion <- "World Trade Organization"

# agregar_fuente_raw(url = url_api,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = T
# )


actualizar_fuente_raw(id_fuente = 317,
                      url = url_api, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)