#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection
options(scipen=999) # notacion cientifica
# Metadatos 
subtopico <- "INDUST"
output_name <- "16_proporcion_importaciones_expo"
analista <- "Nicolás Sidicaro"
fuente1 <- 'sitc_country_product_year_2.csv' # Harvard Complexity Atlas   

# rutas 
instub <- 'indust/raw'
outstub <- 'indust/input'

# carga de fuentes - Argendata 
# df_comex <- argendataR::get_clean_path(fuente1) %>% 
#      arrow::read_parquet()

# carga de fuentes - nico 
df_comex <- readr::read_csv(file.path(instub,fuente1))

# Función principal: SITC -> Sectores principales (descripción completa)
asignar_sector_principal <- function(sitc_codes) {
  sitc_numeric <- as.numeric(substr(as.character(sitc_codes), 1, 2))
  
  case_when(
    sitc_numeric >= 0 & sitc_numeric <= 9 ~ "Agricultura, ganadería, caza y silvicultura",
    sitc_numeric >= 21 & sitc_numeric <= 22 ~ "Agricultura, ganadería, caza y silvicultura",
    sitc_numeric == 29 ~ "Agricultura, ganadería, caza y silvicultura",
    sitc_numeric == 3 ~ "Pesca",
    sitc_numeric >= 23 & sitc_numeric <= 28 ~ "Explotación de minas y canteras",
    sitc_numeric >= 32 & sitc_numeric <= 35 ~ "Explotación de minas y canteras",
    sitc_numeric >= 68 & sitc_numeric <= 68 ~ "Explotación de minas y canteras",
    sitc_numeric >= 11 & sitc_numeric <= 12 ~ "Industrias manufactureras",
    sitc_numeric >= 41 & sitc_numeric <= 43 ~ "Industrias manufactureras",
    sitc_numeric >= 51 & sitc_numeric <= 59 ~ "Industrias manufactureras",
    sitc_numeric >= 61 & sitc_numeric <= 67 ~ "Industrias manufactureras",
    sitc_numeric == 69 ~ "Industrias manufactureras",
    sitc_numeric >= 71 & sitc_numeric <= 79 ~ "Industrias manufactureras",
    sitc_numeric >= 81 & sitc_numeric <= 89 ~ "Industrias manufactureras",
    TRUE ~ "No clasificado"
  )
}

# convertir sitc_product_code a numerico 
df_comex <- df_comex %>% 
  mutate(industria = as.numeric(product_sitc_code))
df_comex <- df_comex %>% 
  mutate(industria = asignar_sector_principal(industria))

# calcular exportaciones 
df_comex <- df_comex %>% 
  group_by(year,industria,country_iso3_code) %>% 
  summarize(exportaciones = sum(export_value),
            importaciones = sum(import_value))

# Filtrar por industria 
df_comex <- df_comex %>% 
  filter(industria == 'Industrias manufactureras')

# Calcular proporcion entre ambas variables 
df_comex <- df_comex %>% 
  mutate(prop_impo_expo = importaciones / exportaciones)
df_comex$exportaciones <- NULL
df_comex$importaciones <- NULL
df_comex$industria <- NULL

# guardar resultados 
readr::write_csv(df_comex,file.path(outstub,paste0(output_name,'.csv')))
