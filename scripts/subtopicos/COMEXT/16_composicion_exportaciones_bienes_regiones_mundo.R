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

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:





#-- PARTNERS

# Load the dataset
partners <-  read_csv(get_temp_path("R101C0")) %>% arrow::to_duckdb()

# Load location names



### LOCATIONS #####


location <-  haven::read_dta(get_temp_path("R154C0")) %>%
  mutate(location_name_short_en = if_else(location_code == "SXM", "St Maarten", location_name_short_en)) %>%
  mutate(id = location_id,
         id_code = location_code,
         id_name_short_en = location_name_short_en,
         id_name = location_id) 


## función para extaer etiquetas de un vector de STATA

extract_labels <- function(x) {
  if (haven::is.labelled(x)) {
    labels <- attr(x, "labels")
    label_names <- names(labels)
    label_values <- labels
    factor(x, levels = label_values, labels = label_names)
  } else {
    x
  }
}



# Subset de location para region
location_region <- location %>% 
  dplyr::filter(level == "region") 




## Extrae valores de etiquetas de subset region

column_name <- "location_id" # Param de columna a extraer etiquetas


data_region <- location_region %>%
  mutate(
    value_column = .data[[column_name]],
    label_column = extract_labels(.data[[column_name]])
  ) %>% 
  transmute(parent_id = as.numeric(as.character(label_column)), 
            partner_region_name_short_en = location_name_short_en)


# Nuevo dataset con agregado de region y conversion a DuckDB

location_with_region <- location %>% 
  left_join(data_region, by = 'parent_id') %>% arrow::to_duckdb()



#### GENERO OUTPUT ####

df_output <-  partners %>%
  left_join(location_with_region %>% select(partner_id = id, 
                                            partner_code = id_code, 
                                            partner_region_name_short_en, 
                                            location_name_short_en)) %>% 
  
  # Continentes en español
  mutate(partner_region_name_short_es = case_when(
    partner_region_name_short_en == "Africa" ~ "África",
    partner_region_name_short_en == "Asia" ~ "Asia",
    partner_region_name_short_en == "Europe" ~ "Europa",
    partner_region_name_short_en == "North America" ~ "Ámerica del Norte",
    partner_region_name_short_en == "Oceania" ~ "Oceanía",
    partner_region_name_short_en == "Other" ~ "Otros",
    partner_region_name_short_en == "South America" ~ "Ámerica del Sur",
    TRUE ~ partner_region_name_short_en
  )) %>% 
  # Colapsamos data a nivel region
  group_by(year, location_code, partner_region_name_short_es) %>%
  summarise(export_value = sum(export_value, na.rm = TRUE),
            import_value = sum(import_value, na.rm = TRUE)) %>%
  ungroup() %>% 
  
  # Calculo de %
  group_by(year, location_code) %>%
  mutate(export_value_pc = export_value / sum(export_value, na.rm = T) * 100,
         import_value_pc = import_value / sum(import_value, na.rm = T) * 100) %>%
  ungroup() %>% 
  
  # Renombramos cols
  left_join(location_with_region %>% select(location_code, location_name_short_en, partner_region_id = parent_id)) %>% 
  rename(iso3 = location_code)  %>% 
  select(year, iso3, location_name_short_en, partner_region_id, partner_region_name_short_es, export_value_pc) %>% 
  collect()




#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


df_anterior <- descargar_output(nombre = output_name, subtopico = "COMEXT", entrega_subtopico = "datasets_primera_entrega") 


comparacion <- argendataR::comparar_outputs(df = df_output, df_anterior = df_anterior,
                                            pk = c("year", "iso3", "location_name_short_en", "partner_region_name_short_es"))


#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(directorio = 'data/COMEXT/',
                           output_name = output_name,
                           subtopico = subtopico,
                           fuentes = c("R154C0","R101C0"),
                           analista = analista,
                           pk = c("year", "iso3", "location_name_short_en", "partner_region_name_short_es"),
                           es_serie_tiempo = TRUE,
                           columna_indice_tiempo = "year",
                           columna_geo_referencia = "iso3",
                           nivel_agregacion = "continente (region)",
                           etiquetas_indicadores = list("export_value_pc" = "Exportaciones de bienes (% del total exportado en bienes)"),
                           unidades = list("export_value_pc" = "porcentaje"), 
                           aclaraciones =  'PK declaras difieren de la metadata. Revisar porque. Output ok en la comparacion.'
  )




