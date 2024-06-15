################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)


output_name <- stringr::str_sub(string = code_name, start = 4, end = -3)

#-- Librerias ----

#-- Lectura de Datos ----

## Location


location <- read_csv(get_temp_path("location.csv")) %>% 
  mutate(location_name_short_en = ifelse(location_code == 'SXM', "St Maarten", location_name_short_en ))

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Join base complexity con location


country_sitcproductsection_year <- readr::read_csv(glue::glue("{tempdir()}/country_sitcproductsection_year.csv"))


country_sitcproductsection_year_location <- country_sitcproductsection_year %>% 
  left_join(location, by = c("location_id","location_code"))  %>% 
  select(year,  location_code, product_id, 
         sitc_product_code, export_value, import_value, 
         location_name_short_en) 

products <- readr::read_csv(glue::glue("{tempdir()}/sitc_product-dta.csv"))


complexity <- country_sitcproductsection_year_location %>% 
  left_join(products, by = c('product_id', 'sitc_product_code')) %>%
  mutate(sitc_product_name_es = case_when(
    sitc_product_name_short_en == "Food" ~ "Productos alimenticios",
    sitc_product_name_short_en == "Beverages" ~ "Bebidas y tabaco",
    sitc_product_name_short_en == "Crude materials" ~ "Materiales crudos, no comestibles, excepto combustibles",
    sitc_product_name_short_en == "Fuels" ~ "Combustibles minerales, lubricantes y productos similares",
    sitc_product_name_short_en == "Vegetable oils" ~ "Aceites y mantecas de origen animal y vegetal",
    sitc_product_name_short_en == "Chemicals" ~ "Productos químicos",
    sitc_product_name_short_en == "Material manufacturers" ~ "Artículos manufacturados, clasificados principalmente según el material",
    sitc_product_name_short_en == "Machinery and vehicles" ~ "Maquinaria y material de transporte",
    sitc_product_name_short_en == "Other manufacturers" ~ "Artículos manufacturados diversos",
    sitc_product_name_short_en == "Unspecified" ~ "Transacciones y mercaderías diversas, N. E. P.",
    sitc_product_name_short_en == "Services" ~ "Servicios",
    TRUE ~ sitc_product_name_short_en  # Default case if no match
  ))







#-- Parametros Generales ----


# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----


df_output <- complexity %>%
  filter(product_id != 10) %>%
  mutate(export_value = export_value / 1e6,
         import_value = import_value / 1e6) %>%
  group_by(year, location_code) %>%
  mutate(export_value_pc = export_value / sum(export_value) * 100,
         import_value_pc = import_value / sum(import_value) * 100) %>% 
  rename(iso3 = location_code, sitc_2_1_cod = sitc_product_code) %>%
  select(year, iso3, location_name_short_en, sitc_2_1_cod, sitc_product_name_es, 
         import_value_pc) %>% 
  arrange(year, iso3, sitc_2_1_cod) %>% 
  mutate(sitc_2_1_cod = as.double(sitc_2_1_cod))

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


df_anterior <- descargar_output(nombre = output_name, subtopico = "COMEXT", entrega_subtopico = "datasets_primera_entrega")



comparacion <- argendataR::comparar_outputs(df = df_output, df_anterior = df_anterior,
                                            pk = c("year", "iso3", "sitc_2_1_cod"))

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(directorio = "data/COMEXT/", 
                           aclaraciones = "Datos ok. Flujo de script, se uso desde server, aunque está disponible como fuentes R102C0 a R104C0",
                           output_name = output_name,
                           subtopico = subtopico,
                           fuentes = c("R102C0","R103C0","R104C0"),
                           analista = analista,
                           pk = c("year", "iso3", "sitc_2_1_cod"),
                           es_serie_tiempo = T,
                           columna_indice_tiempo = "year",
                           columna_geo_referencia = "iso3",
                           nivel_agregacion = "pais",
                           etiquetas_indicadores = list("import_value_pc" = "Importaciones de bienes (% del total importado en bienes)"),
                           unidades = list("import_value_pc" = "porcentaje")
  )

