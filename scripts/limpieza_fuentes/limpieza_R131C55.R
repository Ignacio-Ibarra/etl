#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 131
fuente_raw <- sprintf("R%sC0",id_fuente)


sheet_name <- get_raw_path(fuente_raw) %>% readxl::excel_sheets() %>% .[1]


# traigo la data 
df_clean <- readxl::read_xlsx(argendataR::get_raw_path(fuente_raw),skip = 1, sheet = sheet_name) %>% 
  janitor::clean_names() %>% 
  rename(anio = ano, valor_en_mtco2e = valor) %>%
  select(-cod_pcia_indec, -jurisdiccion)



sufijo <- gsub(".*/|\\..*", "", get_raw_path(fuente_raw))

filename <- glue::glue("{janitor::make_clean_names(sheet_name)}_{sufijo}.parquet")

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = glue::glue("Inventario Nacional de Gases de Efecto Invernadero y Monitoreo de Medidas de Mitigación (2018) - Solapa: {sheet_name}"),
#                      script = code_name)

df_anterior <- read_fuente_clean(55)

lista_comparacion <- comparar_fuente_clean(df_clean,
                      df_anterior,
                      pk = df_clean %>% select(-c(valor_en_mtco2e)) %>% colnames())

actualizar_fuente_clean(id_fuente_clean = 55,
                        nombre = glue::glue("Inventario Nacional de Gases de Efecto Invernadero y Monitoreo de Medidas de Mitigación (2022) - Solapa: {sheet_name}"),
                        df = df_clean, path_clean = filename,
                        script = code_name,
                        comparacion = list("No comparables","Cambiaron las etiquetas de sectores y actividades"))
