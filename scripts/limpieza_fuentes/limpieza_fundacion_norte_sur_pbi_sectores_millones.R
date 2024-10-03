#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection



id_fuente <- 36
fuente_raw <- sprintf("R%sC0",id_fuente)


sheet_name <- "PBI por sectores"


data <- readxl::read_excel(argendataR::get_raw_path(fuente_raw),
                           sheet = sheet_name, skip = 1)
# rename anio
data <- data %>% 
  rename(anio  = Año)

# armo diccionario de nombres de variables limpios
diccionario_vars <- data[1:4,-1] %>% 
  select(-where(function(x) all(is.na(x)))) %>% 
  filter(!if_all(everything(), is.na)) %>% 
  pivot_longer(everything()) %>% 
  mutate(name_fixed = str_replace_all(name, "\\.{2}\\d{1,2}", NA_character_)) %>% 
  fill(name_fixed)

# pivot longer los datos
data <- data[-c(1:4),] %>% 
  pivot_longer(cols = -anio,
               names_to = "indicador", values_to = "valor")

# le matcheo los nombres limpios de variabels
data <- left_join(data, diccionario_vars, by = c("indicador" = "name"))

# excluyo filas vacias
data <- data %>% 
  filter(!is.na(name_fixed)) %>% 
  select(anio, name_fixed, value, valor)

# agrego columna de pertenencia geografica
data <- data %>% 
  mutate(iso3 = "ARG")

# seleccion y rename de columnas
data <- data %>% 
  select(anio, iso3, indicador = name_fixed, unidad = value, valor)

# valores a numerico
data <- data %>% 
  mutate(valor = as.numeric(valor))

# exlcusion de filas vacias por pivoteo o foramto del excel
df_clean <- data %>% 
  filter(!is.na(valor))


normalized_sheet_name <- sheet_name %>% janitor::make_clean_names(.)

clean_filename <- "pbi_sect_prec_const_2004.parquet"

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()

clean_title <- glue::glue("{titulo.raw} - {sheet_name}")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)

actualizar_fuente_clean(id_fuente_clean = 82, path_clean = clean_filename, directorio = tempdir(), nombre = clean_title, script = code_name)

