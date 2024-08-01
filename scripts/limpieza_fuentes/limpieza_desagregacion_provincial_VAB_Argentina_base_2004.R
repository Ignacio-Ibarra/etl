

## descargo la fuente


## hay traer dos fuentes, una para mundo y otro para argentina
## la data hay que filtrarla para 2016 en ambas fuentes

## descargo fuente raw para mundo 
descargar_fuente_raw(id_fuente = 159, tempdir())


vab_provincia_2004 <- readxl::read_xlsx(get_temp_path("R159C0"),
                                                 sheet = 1,skip = 4,n_max = 25) %>% 
  janitor::clean_names()

# Identificar las columnas que contienen años con prefijo "x" y "_"
columnas_años <- grep("^x\\d{4}(_\\d+)?$", names(vab_provincia_2004), value = TRUE)

# Pivote de ancho a largo
df_long <- vab_provincia_2004 %>%
  pivot_longer(cols = all_of(columnas_años),
               names_to = "año",
               values_to = "vab_base_2004") %>% 
  mutate(anio = as.numeric(substr(año, 2, 5)),
         vab_base_2004 = as.numeric(sprintf("%.2f", vab_base_2004))) %>%  
  rename(provincia=jurisdiccion) %>% 
  select(4,1,3)

# guardo csv
write_csv_fundar(x = df_long,
                 file = glue::glue("{tempdir()}/vab_provincias_base_2004.csv"))

# agrego fuente clean
agregar_fuente_clean(id_fuente_raw = 159, 
                     dir = tempdir(),
                     path_clean = "vab_provincias_base_2004.csv",
                     nombre = "Desagregación provincial del valor agregado bruto de la Argentina, base 2004",
                     script = "limpieza_desagregacion_provincial_VAB_Argentina_base_2004.R")

# actualizo fuente clean
actualizar_fuente_clean(id_fuente_clean = 68)



