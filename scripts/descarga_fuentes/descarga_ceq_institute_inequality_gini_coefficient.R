# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


fecha_actualizar <- "Sin informacion"

source("scripts/utils/ceq_institute_api.R")

selected_theme <- "Inequality"
  
selected_category <- "Gini Coefficient"

indicators <- CEQI.list_indicators()$data %>%
  select(id, theme, category, concept, name, dimension, unit) %>% 
  dplyr::filter(theme == selected_theme, category == selected_category)
  
indicators_id <- indicators$id

result <- CEQI.get_data(indicators = indicators_id)

url <- result$url

df_raw <- result$data %>% 
  left_join(indicators , join_by(indicator_id == id))

nombre <- glue::glue("CEQ Standar Indicators. {selected_theme}. {selected_category}")

institucion <- "CEQ Data Center on Fiscal Redistribution. Commitment to Equity (CEQ) Institute. Tulane University"

download_filename <- glue::glue("ceq_institute_{selected_theme}_{selected_category}.csv") %>% 
  janitor::make_clean_names()

destfile <- glue::glue("{tempdir()}/{download_filename}")

df_raw %>% argendataR::write_csv_fundar(., destfile)

# 
# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = T
# )

actualizar_fuente_raw(id_fuente = 425,
                      url = url, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)