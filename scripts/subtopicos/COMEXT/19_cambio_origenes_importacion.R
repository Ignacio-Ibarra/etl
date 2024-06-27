################################################################################
##                              Dataset: cambio_origenes_importacion          ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)


output_name <- stringr::str_sub(string = code_name, start = 4, end = -3)


#-- Librerias ----


#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
partners <-  read_csv(get_temp_path("R101C0")) %>% arrow::to_duckdb()





### LOCATIONS #####


location <-  haven::read_dta(get_temp_path("R154C0")) %>%
  mutate(location_name_short_en = if_else(location_code == "SXM", "St Maarten", location_name_short_en)) %>%
  mutate(id = location_id,
         id_code = location_code,
         id_name_short_en = location_name_short_en,
         id_name = location_id) 



#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----



# Keep necessary columns
data <- partners %>%
  select(year, location_id, location_code, partner_id, partner_code, export_value, import_value)

# Summarize data by year, location, and partner
data <- data %>%
  group_by(year, location_id, location_code, partner_id, partner_code) %>%
  summarize(
    export_value = sum(export_value, na.rm = TRUE),
    import_value = sum(import_value, na.rm = TRUE)
  )

# Convert values to millions of USD
data <- data %>%
  mutate(
    export_value = export_value / 1000000,
    import_value = import_value / 1000000
  )

# Merge location names
location_names <- location %>%
  select(location_id, location_name_short_en) %>%  arrow::to_duckdb()

data <- data %>%
  left_join(location_names, by = "location_id")

# Merge partner names
partner_names <- location_names %>%
  rename(partner_id = location_id, partner_name_short_en = location_name_short_en)

data <- data %>%
  left_join(partner_names, by = "partner_id")

# Calculate shares
data <- data %>%
  group_by(year, location_id) %>%
  mutate(
    export_value_pc = export_value / sum(export_value, na.rm = TRUE) * 100,
    import_value_pc = import_value / sum(import_value, na.rm = TRUE) * 100
  )

# Define periods

data <- data %>%
  mutate(period = case_when(
    year >= 1962 & year <= 1966 ~ "a",
    year >= 2017 & year <= 2021 ~ "b",
    TRUE ~ ""
  )) %>%
  filter(period != "")

# Collapse and reshape data
collapsed_data <- data %>%
  group_by(period, location_id, location_code, location_name_short_en, partner_id, partner_code, partner_name_short_en) %>%
  summarize(
    export_value_pc = mean(export_value_pc, na.rm = TRUE),
    import_value_pc = mean(import_value_pc, na.rm = TRUE)
  ) %>% 
  pivot_wider(
    names_from = period,
    values_from = c(export_value_pc, import_value_pc))


# Rename and label variables
collapsed_data <- collapsed_data %>%
  rename(
    iso3 = location_code,
    export_value_pca = export_value_pc_a,
    export_value_pcb = export_value_pc_b,
    import_value_pca = import_value_pc_a,
    import_value_pcb = import_value_pc_b
  )



df_output <- collapsed_data %>% 
  ungroup() %>% 
  select(iso3, location_name_short_en, partner_code, partner_name_short_en, import_value_pca, import_value_pcb) %>% 
  collect()

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

df_anterior <- descargar_output(nombre = output_name, subtopico = "COMEXT", entrega_subtopico = "datasets_primera_entrega") 


comparacion <- argendataR::comparar_outputs(df = df_output, df_anterior = df_anterior,
                                            pk = c( "iso3", "partner_code"))


#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(directorio = 'data/COMEXT/',
                           control = comparacion,
                           output_name = output_name,
                           subtopico = subtopico,
                           fuentes = c("R154C0","R101C0"),
                           analista = analista,
                           pk = c("partner_code", "iso3"),
                           es_serie_tiempo = FALSE,
                           columna_indice_tiempo = FALSE,
                           columna_geo_referencia = "iso3",
                           nivel_agregacion = "continente (region)",
                           etiquetas_indicadores = list("import_value_pca" = "% de las importaciones argentinas al mundo (promedio 1962-1966)",
                                                        "import_value_pcb" = "% de las importaciones argentinas al mundo (promedio 2017-2021)"),
                           unidades = list("import_value_pca" = "porcentaje", 
                                           "import_value_pcb" = "porcentaje"), 
                           aclaraciones =  ''
  )


