
##fuente
url<-"https://www.indec.gob.ar/ftp/cuadros/economia/sh_VBP_VAB_09_24.xls"

download.file(url, 
              destfile = "archivo.xls", mode = "wb")

data_vbp_rama_act_econ <- readxl::read_xls("archivo.xls", sheet = 2)

# eliminar filas 1 y 2
data_vbp_rama_act_econ <- data_vbp_rama_act_econ[-c(1, 2), ]

# fila 1 como nombres de columna
colnames(data_vbp_rama_act_econ) <- data_vbp_rama_act_econ[1, ] 

# Eliminar la nueva fila de nombres
data_vbp_rama_act_econ <- data_vbp_rama_act_econ[-1, ]  

# asigno nombre "valores" a la primera columna
colnames(data_vbp_rama_act_econ)[1] <- "valores"

# Filtro filas que sean completamente NA
data_vbp_rama_act_econ <- data_vbp_rama_act_econ %>% filter_all(any_vars(!is.na(.)))

# Elimina "(2)" o "(1)" de los nombres de columnas paso todas las vairavles menos 'valores' a numeric

data_vbp_rama_act_econ <- data_vbp_rama_act_econ %>%
  rename_with(~ gsub("\\s*\\(\\d+\\)", "", .x)) %>%  
  mutate(across(-valores, as.numeric))  

# pivot_longer para convertir los años (columnas) en filas
data_vbp_rama_act_econ_long <- data_vbp_rama_act_econ %>%
  pivot_longer(cols = -valores,  
               names_to = "año",  
               values_to = "valor") %>% 
  filter(!grepl("Datos provisorios|Datos preliminares|Fuente:|Nota:", valores)) %>%
  filter(!is.na(valores))  # Eliminar las filas que tienen NA en la columna de valores


data_vbp_rama_act_econ_long %>% 
  write_csv_fundar(normalizePath(sprintf("%s/valor_bruto_prod_prec_bas_por_act_econ.csv", tempdir())))

agregar_fuente_raw(
  url = url,
  nombre = "Valor Bruto de Producción a precios básicos por rama de actividad económica. Valores anuales en millones de pesos a precios de 2004",
  institucion =  "INDEC",
  actualizable = T,
  fecha_actualizar = "Sin informacion",
  path_raw = "valor_bruto_prod_prec_bas_por_act_econ.csv",
  directorio = tempdir(), script = "descarga_vbp_pre_bas_por_rama_act_econ.R",api = F
)

#actualizar_fuente_raw(, script = "descarga_vbp_pre_bas_por_rama_act_econ.R")


fuentes_raw()

223

