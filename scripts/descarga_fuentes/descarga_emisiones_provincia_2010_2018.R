

emisiones_arg_prov_arg_2010_2018_url <- "https://inventariogei.ambiente.gob.ar/files/desagregacion-provincial_hasta_2018.xlsx"

download.file(url = emisiones_arg_prov_arg_2010_2018_url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("{tempdir()}/emis_arg_prov_2010_2018.xlsx"))

# agrego la fuente
agregar_fuente_raw(url = "https://inventariogei.ambiente.gob.ar/files/desagregacion-provincial_hasta_2018.xlsx", 
                   institucion = "Ministerio Ambiente - Informe Bienal de Actualización", 
                   actualizable = T,
                   fecha_descarga = Sys.Date(),
                   path_raw = "emis_arg_prov_2010_2018.xlsx", 
                   dir = tempdir(),
                   script = "descarga_emisiones_provincia_2010_2018.R",
                   nombre = "Emisiones por Provincia. Año 2010-2018"
)

actualizar_fuente_raw(id_fuente=157 ,url = "https://inventariogei.ambiente.gob.ar/files/inventario-nacional-gei-emisiones_hasta_2018.xlsx",
                      fecha_descarga = Sys.Date())

#list.files(tempdir())

#fuentes() %>% 
#  view()



df <- readxl::read_excel(glue::glue("{tempdir()}/emis_arg_prov_2010_2018.xlsx"), sheet = 3) %>% 
  clean_names()

columnas <- df[3,]
colnames(df) <- columnas

df <- df %>% 
  filter(!if_all(-`Sector / Categoría`, is.na)) %>% 
  filter(!is.na(`Sector / Categoría`)) 

df <- df[-1, ] 

df<- clean_names(df) %>% 
  mutate(sector = case_when(
    grepl("1. Sector Energía", sector_categoria) ~ "Energía",
    grepl("2. Sector Procesos industriales y uso de productos", sector_categoria) ~ "PIUP",
    grepl("3. Sector Agricultura, ganadería, silvicultura y otros usos de la tierra", sector_categoria) ~ "AGSyOUT",
    grepl("4. Sector Residuos", sector_categoria) ~ "Residuos",
    TRUE ~ NA_character_  # Para casos que no coincidan con ninguna de las condiciones anteriores
  )) %>% 
  filter(!is.na(sector))  # Filtrar filas sin NA en la columna 'sector'

  
view(df)




library(readxl)
library(dplyr)
library(janitor)
library(glue)

# Función para procesar cada hoja
procesar_hoja <- function(hoja) {
  df <- readxl::read_excel(glue::glue("{tempdir()}/emis_arg_prov_2010_2018.xlsx"), sheet = hoja) %>% 
    clean_names()
  
  # Asignar nombres de columna desde la tercera fila
  columnas <- df[3,]
  colnames(df) <- columnas
  
  # Eliminar la primera fila
  df <- df[-1, ]
  
  # Limpiar nombres de columnas y crear la nueva columna 'sector'
  df <- df %>% 
    clean_names() %>% 
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
hojas <- excel_sheets(glue::glue("{tempdir()}/emis_arg_prov_2010_2018.xlsx"))

# Procesar todas las hojas a partir de la hoja 3
resultados <- lapply(hojas[3:length(hojas)], procesar_hoja)

# Combinar todos los resultados en un solo data frame
df_final <- bind_rows(resultados)

# Ver el resultado final
view(df_final)
class(df_final$x2010)
names(df_final_final)

df_final_final <- df_final %>%
  mutate_at(vars(2:10), ~ round(as.numeric(.), 2)) %>% 
  mutate(codigo = substr(provincia, 1, 3),  # Crear la variable 'codigo' con los primeros 3 caracteres
         provincia_final = substr(provincia, 4, nchar(provincia))) 
  #select(12,1,2:10)

view(df_final_final)
class(df_final_final$x2010)


columnas_años <- grep("^x\\d{4}$", names(df_final_final), value = TRUE)

# Pivote de ancho a largo
df_long <- df_final_final %>%
  pivot_longer(cols = all_of(columnas_años),
               names_to = "año",
               values_to = "valor_en_mtco2e")


view(df_long)


df_final <- data.frame(
  provincia = c("Buenos Aires", "Córdoba"),
  x2010 = c(100, 150),
  x2011 = c(120, 160),
  x2012 = c(110, 140)
)

# Identificar las columnas que contienen años con prefijo "x"
columnas_años <- grep("^x\\d{4}$", names(df_final), value = TRUE)

