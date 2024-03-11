
descargar_fuente_raw(id_fuente = 36, dir = "data/_FUENTES/raw/")

# carga

data <- readxl::read_excel("data/_FUENTES/raw/cuentas-nacionales-fundacion-norte-y-sur.xlsx",
                           sheet = 1, skip = 1)

# rename columns

data <- data %>% 
  rename(anio  = Año)

# formatear longer
diccionario_vars <- data[1:4,-1] %>% 
  select(-where(function(x) all(is.na(x)))) %>% 
  filter(!if_all(everything(), is.na)) %>% 
  pivot_longer(everything()) %>% 
  mutate(name_fixed = str_replace_all(name, "\\.{2}\\d{1,2}", NA_character_)) %>% 
  fill(name_fixed)

# pivot longer
data <- data[-c(1:4),] %>% 
  pivot_longer(cols = -anio,
               names_to = "indicador", values_to = "valor")

data <- left_join(data, diccionario_vars, by = c("indicador" = "name"))

data <- data %>% 
  filter(!is.na(name_fixed)) %>% 
  select(anio, name_fixed, value, valor)

data <- data %>% 
  mutate(iso3 = "ARG")

data <- data %>% 
  select(anio, iso3, indicador = name_fixed, unidad = value, valor)

data <- data %>% 
  mutate(valor = as.numeric(valor))

data <- data %>% 
  filter(!is.na(valor))

# guardar
write_csv_fundar(data,
                 file = "data/_FUENTES/clean/cuentas-nacionales-fundacion-norte-y-sur.csv")  

agregar_fuente_clean(id_fuente_raw = 36,path_clean = "cuentas-nacionales-fundacion-norte-y-sur.csv",
                     nombre = "Cuentas Nacionales",
                     script = "limpieza_cn_fundacion_norteysur.R")

actualizar_fuente_clean(id_fuente_clean = 4)