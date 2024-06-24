################################################################################
##  Dataset: composicion_exportaciones_servicios_EBOPS_2digitos_agrupado      ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'


#-- Parametros Generales ----
code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)


output_name <- stringr::str_sub(string = code_name, start = 4, end = -3)



wto_path <- Sys.getenv("WTO_DIR")


#-- Librerias ----

#-- Lectura de Datos ----

#wto <-  read_csv(get_temp_path("R94C0"))  

codigos <- countrycode::codelist %>% select(iso3n, iso3c) %>% 
  dplyr::transmute(iso3n = stringr::str_pad(iso3n, 3, "left", 0), 
                 reporting_economy_iso3a_code = iso3c)


wto <- arrow::open_dataset(wto_path)


comentario <- warning('### Esta fuente se está levantando desde el /srv/ Tiene registrada fuente_raw fake con comentario')



#-- Procesamiento ----


data <- wto %>% 
 
  filter(IndicatorCode  == "ITS_CS_AX6") %>%
  rename(export_value = Value) %>% 

# Step 3: Keep only World as partner

  filter(PartnerEconomyCode == "000") %>% collect() %>% 
 # filter(ReportingEconomyCode != '000') %>% 

# Step 5: Filter for 2-digit product sector codes
  mutate(number_digits = str_length(ProductOrSectorCode)) %>%  
  filter(number_digits == 2) %>% 
  janitor::clean_names()


# data %>% select(product_or_sector, product_or_sector_code) %>% unique()

# Step 6: Encode and label product sectors in Spanish
  

product_labels <- c(
  "SA" = "Servicios de manufactura sobre insumos físicos pertenecientes a otros",
  "SB" = "Servicios de mantenimiento y reparación n.i.o.p.",
  "SC" = "Transportes",
  "SD" = "Viajes",
  "SE" = "Construcción",
  "SF" = "Servicios de seguros y pensiones",
  "SG" = "Servicios financieros",
  "SH" = "Cargos por el uso de la propiedad intelectual n.i.o.p.",
  "SI" = "Servicios de telecomunicaciones, informática e información",
  "SJ" = "Otros servicios empresariales",
  "SK" = "Servicios personales, culturales y recreativos",
  "SL" = "Bienes y servicios del gobierno n.i.o.p.",
  "SN" = "Servicios sin clasificar"
)

product_labels_tibble <- tibble(
  product_or_sector_code = names(product_labels),
  product_or_sector_ESP = product_labels)


data <- data %>% left_join(product_labels_tibble)

# Step 7: Aggregate categories into four main groups
data <- data %>%
  mutate(productsector_agregado = case_when(
    product_or_sector_code  %in% c("SD", "SJ", "SI", "SC") ~ product_or_sector_ESP,
    TRUE ~ "Resto"
  ))

# Step 8: Summarize the data
data <- data %>%
  left_join(codigos, by = c("reporting_economy_code" = "iso3n")) %>% 
  filter(!is.na(reporting_economy_iso3a_code)) %>% # exluye filas no ISO3 - paises
  group_by(year, reporting_economy_iso3a_code, reporting_economy, productsector_agregado) %>%
  summarize(export_value = sum(export_value, na.rm = TRUE)) %>%
  ungroup()

# Step 9: Calculate shares and label the variables
data <- data %>%
  group_by(year, reporting_economy) %>%
  mutate(export_value_pc = export_value / sum(export_value, na.rm = TRUE) * 100) %>%
  ungroup()

# Step 10: Rename and export the final dataset
data <- data %>% 
  rename(iso3 = reporting_economy_iso3a_code, 
         reportingeconomy = reporting_economy) %>%
  select(year, iso3,  reportingeconomy, productsector_agregado, export_value_pc) %>% 
  mutate(year = as.numeric(year))




df_output <- data



#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


df_anterior <- descargar_output(nombre = output_name, subtopico = "COMEXT", entrega_subtopico = "datasets_primera_entrega") 

comparacion <- argendataR::comparar_outputs(df = df_output, df_anterior = df_anterior,
                                            pk = c( "iso3", "year", "productsector_agregado"))


#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(directorio = 'data/COMEXT/',
                           output_name = output_name,
                           subtopico = subtopico,
                           fuentes = c("R94C0"),
                           analista = analista,
                           pk = c("iso3", "year", "productsector_agregado"),
                           es_serie_tiempo = TRUE,
                           columna_indice_tiempo = "year",
                           columna_geo_referencia = "iso3",
                           nivel_agregacion = "pais",
                           etiquetas_indicadores = list("export_value_pc" = "Exportaciones de servicios (% del total exportado en servicios)",
                                                        "export_value_pc" = "Exportaciones de servicios (% del total exportado en servicios)"),
                           unidades = list("export_value_pc" = "porcentaje"),
                           aclaraciones =  '' )


