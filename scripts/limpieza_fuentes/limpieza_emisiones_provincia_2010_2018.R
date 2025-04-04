
## descargo la fuente


## hay traer dos fuentes, una para mundo y otro para argentina
## la data hay que filtrarla para 2016 en ambas fuentes

## descargo fuente raw para mundo 
descargar_fuente_raw(id_fuente = 157, tempdir())

# Función para procesar cada hoja
procesar_hoja <- function(hoja) {
  df <- readxl::read_excel(glue::glue(get_raw_path("R157C0")), sheet = hoja) %>% 
    janitor::clean_names()
  
  # Asignar nombres de columna desde la tercera fila
  columnas <- df[3,]
  colnames(df) <- columnas
  
  # Eliminar la primera fila
  df <- df[-1, ]
  
  # Limpiar nombres de columnas y crear la nueva columna 'sector'
  df <- df %>% 
    janitor::clean_names() %>% 
    mutate(sector = case_when(
      grepl("1. Sector Energía", sector_categoria) ~ "Energía",
      grepl("2. Sector Procesos industriales y uso de productos", sector_categoria) ~ "PIUP",
      grepl("3. Sector Agricultura, ganadería, silvicultura y otros usos de la tierra", sector_categoria) ~ "AGSyOUT",
      grepl("4. Sector Residuos", sector_categoria) ~ "Residuos",
      TRUE ~ NA_character_  # Para casos que no coincidan con ninguna de las condiciones anteriores
    )) %>% 
    filter(!is.na(sector))  %>% # Filtrar filas sin NA en la columna 'sector'
    mutate(provincia = hoja)
  
  return(df)
}

# Obtener todas las hojas del archivo
hojas <- readxl::excel_sheets(get_raw_path("R157C0"))

# Procesar todas las hojas a partir de la hoja 3
resultados <- lapply(hojas[3:length(hojas)], procesar_hoja)

# Combinar todos los resultados en un solo data frame
df_final <- bind_rows(resultados)

## modifico variables
df_final_final <- df_final %>%
  mutate_at(vars(2:10), ~ round(as.numeric(.), 4)) %>% 
  mutate(codigo = substr(provincia, 1, 3),  # Crear la variable 'codigo' con los primeros 3 caracteres
         provincia_final = substr(provincia, 4, nchar(provincia))) %>% 
select(2:11,14)

# Identificar las columnas que contienen años con prefijo "x"
columnas_años <- grep("^x\\d{4}$", names(df_final_final), value = TRUE)

# Pivote de ancho a largo
df_long <- df_final_final %>%
  pivot_longer(cols = all_of(columnas_años),
               names_to = "año",
               values_to = "valor_en_mtco2e") %>% 
  mutate(anio = as.numeric(substr(año, 2, 5))) %>% 
  mutate(provincia = ifelse(provincia_final == "CABA", "Ciudad Autónoma de Buenos Aires", provincia_final)) %>% 
select (5,6,1,4)  

# # guardo csv
# write_csv_fundar(x = df_long,
#                  file = glue::glue("{tempdir()}/emisiones_prov_2010_2018.csv"))
# 
# # agrego fuente clean
# agregar_fuente_clean(id_fuente_raw = 157, 
#                      dir = tempdir(),
#                      path_clean = "emisiones_prov_2010_2018.csv",
#                      nombre = "Emisiones por sector y Provincia. Argentina 2010-2018",
#                      script = "limpieza_emisiones_provincia_2010_2018.R")

lista_comparacion <- comparar_fuente_clean(df_long, id = 67, pk = c("anio", "provincia", "sector"))

# actualizo fuente clean
actualizar_fuente_clean(id_fuente_clean = 67, df = df_long, comparacion = lista_comparacion)



