descargar_fuente("R133C0")

archivo <- "pbi_vab_sectorial_1980_2005_trim_pb_precios93.csv"

df <- readxl::read_excel(get_temp_path("R133C0"))

columnas <- df[5:13,] %>% 
  summarise(across(everything(), function(x) paste(x[!is.na(x)], collapse = " "))) %>% 
  mutate(across(everything(), function(x) {gsub("\\(.*\\)", "", x)})) %>% 
   as.character()

colnames(df) <- columnas


df <- df %>% 
  janitor::clean_names()

df <- df %>% 
  filter(!is.na(periodo))


df <- df %>% 
  mutate(anio = as.numeric(periodo))

df <- df %>% 
  fill(anio, .direction = "down")


df <- df %>% 
  mutate(trim = case_when(
    periodo == "I" ~ "1",
    periodo == "II" ~ "2",
    periodo == "III" ~ "3",
    periodo == "IV" ~ "4",
    !is.na(as.numeric(periodo)) ~ "total"
  ))

df <- df %>% 
  filter(trim != "total") %>% 
  mutate(trim = as.numeric(trim))

df <- df %>% 
  select(-where(function(x) all(is.na(x))))

df <- df %>% 
  select(-c(periodo)) %>% 
  select(anio, trim, everything())

df %>% 
  write_csv_fundar(file = glue::glue("{tempdir()}/{archivo}"))


# agregar_fuente_clean(id_fuente_raw = 133, path_clean = archivo,
#                      nombre = "Producto Interno Bruto a precios de mercado y Valor Agregado Bruto, por sector económico, a precios básicos. En millones de pesos, a precios de 1993",
#                      script = "scripts/limpieza_fuentes/limpieza_pbi_1980_2005_trim_indec.R")

actualizar_fuente_clean(id_fuente_clean = 58)