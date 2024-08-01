
## descargo la fuente


## hay traer dos fuentes, una para mundo y otro para argentina
## la data hay que filtrarla para 2016 en ambas fuentes

## descargo fuente raw para mundo 
descargar_fuente_raw(id_fuente = 160, tempdir())

#traigo la data
df_pbi_per_capita <- readxl::read_excel(glue::glue("{tempdir()}/pbg por provincia_R160C0.xlsx"), sheet = "serie empalmada PIBpc") %>% 
    janitor::clean_names()

# Identificar las columnas que contienen años con prefijo
columnas_años <- grep("^x\\d{4}$", names(df_pbi_per_capita), value = TRUE)

# Pivote de ancho a largo
df_pbi_per_capita_long <- df_pbi_per_capita %>%
  pivot_longer(cols = all_of(columnas_años),
               names_to = "año",
               values_to = "pbi_per_capita") %>% 
  mutate(anio = as.numeric(substr(año, 2, 5)),
         pbi_per_capita = as.numeric(sprintf("%.2f", pbi_per_capita))) %>% 
  filter(provincia != "Total") %>% 
  mutate (provincia = recode(provincia, "CABA" = "Ciudad Autónoma de Buenos Aires")) %>% 
  mutate(region_pbg = case_when(
    provincia %in% c("Mendoza", "La Rioja", "San Juan", "San Luis") ~ "Cuyo",
    provincia %in% c("Tucumán", "Salta", "Catamarca", "Jujuy", "Santiago del Estero") ~ "NOA",
    provincia %in% c("Chubut", "Río Negro", "Santa Cruz", "Tierra del Fuego", "Neuquén") ~ "Patagonia",
    provincia %in% c("Entre Ríos", "Corrientes", "Misiones", "Formosa", "Chaco") ~ "NEA",
    provincia %in% c("Buenos Aires", "La Pampa", "Santa Fe", "Córdoba", "Ciudad Autónoma de Buenos Aires") ~ "Pampeana y CABA",
    TRUE ~ NA_character_  
  )) %>% 
  select(1,2,5,4)

# guardo csv
write_csv_fundar(x = df_pbi_per_capita_long,
                 file = glue::glue("{tempdir()}/pbi_per_capita_prov_1895_2022.csv"))

# agrego fuente clean
agregar_fuente_clean(id_fuente_raw = 160, 
                     dir = tempdir(),
                     path_clean = "pbi_per_capita_prov_1895_2022.csv",
                     nombre = "PBI per capita por Provincia. Argentina Año 1895-2022",
                     script = "limpieza_empalme_dani_st_pbi_per_capita_1895_2018.R")

# actualizo fuente clean
actualizar_fuente_clean(id_fuente_clean = 70)



