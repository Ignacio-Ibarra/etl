
# weo imf -----------------------------------------------------------------

descargar_fuente_raw(id_fuente = 34,
                     dir = "data/_FUENTES/raw/")

data <- readr::read_tsv("data/_FUENTES/raw/weo-imf.xls")

data <- data %>% 
  mutate(across(everything(), as.character))

# pivot longer
data <- data %>% 
  pivot_longer(cols = matches("\\d{4}"),
               names_to = "anio", values_to = "valor")

colnames(data)

# limpiar col names
data <- data %>% 
  janitor::clean_names()

colnames(data)


# rename cols en base a estandares fundar

data <- data %>% 
  rename(iso3 =  iso)

# limpiar columnas

data <- data %>% 
  filter(!if_all(-c(weo_country_code, anio), is.na))


# convertir a NA caracters que marcan dato faltante o dato que no es computable
data$valor[data$valor %in% c("n/a", "--")] <- NA

# chequear no digitos en valor
unique(str_extract(data$valor, "\\D"))
# contar NAs (no deberian aumentar dps de parsear)
sum(is.na(data$valor))
# pasar a numeric
data$valor <- as.numeric(gsub(",","",data$valor))
#controlar na de nuevo
sum(is.na(data$valor))

head(data)

data <- data %>% 
  mutate(valor = case_when(
    units == "Billions" ~ valor*1E9,
    units == "Millions" ~ valor*1E6,
    T ~ valor
  ))

diccionario <- data %>% 
  distinct(weo_subject_code, subject_descriptor, subject_notes, scale, iso3,
           country_series_specific_notes, estimates_start_after) %>% 
  mutate(scale_aclaracion = "Todos los indicadores fueron rexpresados en unidades.")


data <- data %>% 
  select(iso3, weo_subject_code, anio, valor)
# guardar

write_csv_fundar(data,
          file = "data/_FUENTES/clean/weo_imf.csv")

write_csv_fundar(diccionario,
                 file = "data/_FUENTES/clean/diccionario_weo_imf.csv")


# agregar_fuente_clean(path_clean = "weo_imf.csv",
#                      id_fuente_raw = 34,
#                      nombre = "World Economic Outlook database",
#                      script = "limpieza_weo_imf.R")

actualizar_fuente_clean(id_fuente_clean = 2)

# agregar_fuente_clean(path_clean = "diccionario_weo_imf.csv",
#                      id_fuente_raw = 34,
#                      nombre = "World Economic Outlook diccionario",
#                      script = "limpieza_weo_imf.R")

actualizar_fuente_clean(id_fuente_clean = 3)

