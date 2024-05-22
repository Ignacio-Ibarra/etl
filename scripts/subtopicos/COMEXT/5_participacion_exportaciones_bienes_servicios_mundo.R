################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)


output_name <- stringr::str_sub(string = code_name, start = 3, end = -3)

#-- Librerias ----

#-- Lectura de Datos ----

## Location


location <- read_csv(get_temp_path("location.csv")) %>% 
            mutate(location_name_short_en = ifelse(location_code == 'SXM', "St Maarten", location_name_short_en ))

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Join base complexity con location


complexity <- readr::read_csv(glue::glue("{tempdir()}/country_sitcproductsection_year.csv")) %>% 
  select("year",  "location_id", "location_code", "export_value")


complexity <- complexity %>% 
  left_join(location, by = c("location_id","location_code"))


#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

df_output <-   complexity %>% 
  mutate(export_value = export_value/1000000) %>% 
  group_by(year, location_code,  location_name_short_en ) %>% 
  summarise(export_value = sum(export_value)) %>% 
  rename(x_tt = export_value, 
         iso3 = location_code) %>% 
  group_by(year) %>% 
  mutate(x_tt_pc = x_tt/sum(x_tt)*100) %>% 
  select(-x_tt)

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  pk = c("year", "iso3"),
  drop_output_drive = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso


df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c("/srv/server_data/argendata/atlas_economic_complexity/"),
    analista = analista,
    pk = c("year", "iso3"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "year",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais/region",
    etiquetas_indicadores = list("x_tt_pc" = "Exportaciones de Bienes y Servicios (% del Mundo)"),
    unidades = list("x_tt_pc" = "porcentaje")
  )
