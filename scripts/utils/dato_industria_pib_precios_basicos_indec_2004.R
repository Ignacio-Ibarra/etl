# Código R para convertir Cuadro 3 a formato long
# Valor Agregado Bruto a precios básicos por rama de actividad económica

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(httr)

# 1. LECTURA DEL ARCHIVO
# Leer el archivo Excel, específicamente la hoja "Cuadro 3"
anio_final <- 2025
GET('https://www.indec.gob.ar/ftp/cuadros/economia/sh_VBP_VAB_09_25.xls', 
    write_disk(tf <- tempfile(fileext = ".xls")))
cuadro3_raw <- read_excel(tf, 
                          sheet = "Cuadro 3", 
                          .name_repair = "minimal")

# 2. PREPARACIÓN DE HEADERS
# Extraer años (fila 4) y trimestres (fila 5)
anos_row <- cuadro3_raw[4, ]
trimestres_row <- cuadro3_raw[5, ]

# Crear vector de años y trimestres combinados para los nombres de columnas
col_names <- c("actividad_economica")
for (i in 2:ncol(cuadro3_raw)) {
  ano <- anos_row[[i]]
  trimestre <- trimestres_row[[i]]
  
  if (!is.na(ano) && !is.na(trimestre) && trimestre != "") {
    if (trimestre == "Total") {
      col_names <- c(col_names, paste0("total_", ano))
    } else {
      # Extraer número del trimestre
      num_trim <- str_extract(trimestre, "\\d+")
      col_names <- c(col_names, paste0("t", num_trim, "_", ano))
    }
  } else if (!is.na(trimestre) && trimestre != "" && i > 1) {
    # Si no hay año pero sí trimestre, usar el año anterior
    ano_anterior <- tail(anos_row[!is.na(anos_row)], 1)[[1]]
    if (trimestre == "Total") {
      col_names <- c(col_names, paste0("total_", ano_anterior))
    } else {
      num_trim <- str_extract(trimestre, "\\d+")
      col_names <- c(col_names, paste0("t", num_trim, "_", ano_anterior))
    }
  } else {
    col_names <- c(col_names, paste0("col_", i))
  }
}

# 3. PREPARACIÓN DE DATOS
# Extraer solo las filas de datos (a partir de la fila 7)
datos_inicio <- 7
cuadro3_data <- cuadro3_raw[datos_inicio:nrow(cuadro3_raw), ]

# Asignar nombres de columnas
names(cuadro3_data) <- col_names[1:ncol(cuadro3_data)]

# Filtrar filas que tengan actividad económica (no estén vacías)
cuadro3_clean <- cuadro3_data %>%
  filter(!is.na(actividad_economica) & actividad_economica != "")
# Convertir trimestres a formato long
cuadro3_trimestres_long <- cuadro3_clean %>%
  pivot_longer(cols = -actividad_economica,
               names_to = "periodo",
               values_to = "valor") %>%
  filter(!is.na(valor))

# Agregar trimestre 
datos_por_sector <- cuadro3_trimestres_long %>% 
  group_by(actividad_economica) %>% 
  summarize(cantidad = n()) %>% 
  select(cantidad) %>% 
  distinct() %>% 
  pull()
cantidad_sectores <- cuadro3_trimestres_long %>% 
  select(actividad_economica) %>% 
  distinct() %>% 
  pull()

# armar dato trimeste 
anios <- rep(2004:anio_final,each=5)
trimestres <- rep(c('1','2','3','4','total'),(anio_final-2004))
anio_trim <- paste(anios,trimestres,sep='_')
anio_trim <- anio_trim[1:datos_por_sector]
anio_trim <- rep(anio_trim,length(cantidad_sectores))

cuadro3_trimestres_long <- cuadro3_trimestres_long %>% 
  mutate(trimestre = anio_trim)

# Filtrar por anio
cuadro3_trimestres_long <- cuadro3_trimestres_long %>% 
  filter(str_detect(trimestre,'total'))

# Quedarme con el anio
cuadro3_trimestres_long <- cuadro3_trimestres_long %>% 
  mutate(trimestre = as.numeric(str_extract(trimestre,'[0-9]+')))

# Armar dato final
cuadro3_trimestres_long <- cuadro3_trimestres_long %>% 
  filter(actividad_economica == 'Industria manufacturera') %>% 
  mutate(valor = as.numeric(valor)) %>% 
  select(trimestre,valor)

rm(list=setdiff(ls(),'cuadro3_trimestres_long'))
