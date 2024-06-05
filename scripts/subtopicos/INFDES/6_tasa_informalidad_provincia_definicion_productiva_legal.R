################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "INFDES"
output_name <- "tasa_informalidad_provincia_definicion_productiva_legal"
fuente1 <- "R49C16"
fuente2 <- "R84C14"


#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
ephtu_df <- readr::read_csv(argendataR::get_temp_path(fuente1))

codigos <- readr::read_csv(argendataR::get_temp_path(fuente2))
codigos <- codigos %>% select(aglomerado = aglom_cod_indec, provincia = prov_cod, prov_desc)

#-- Procesamiento ----

ephtu_df <- ephtu_df %>% 
  left_join(codigos, by = join_by(aglomerado, provincia)) # Joineo así por los casos en que hay mismo aglomerado pero distinta provincia e.g. San Nicolás-Villa Constitucion


data <- data.frame(ephtu_df)


# Utilizo las categorías de SEDLAC Employment para calcular informalidad productiva
data$cat_cedlas <- NA

# Aplicar las condiciones para cat_cedlas

# "Patrón"
data$cat_cedlas[data$estado == 1 & data$cat_ocup == 1] <- 1 # "Patrón"

# "Asalariado en pequeñas y grandes empresas"
data$cat_cedlas[(data$estado == 1 & data$cat_ocup == 3 & data$pp04c >= 6 & data$pp04c < 13)] <- 2
data$cat_cedlas[(data$estado == 1 & data$cat_ocup == 3 & data$pp04c99 >= 2 & data$pp04c99 < 9)] <- 2

# "Asalariado público"
data$cat_cedlas[data$estado == 1 & data$cat_ocup == 3 & data$pp04a == 1] <- 3 

# "Cuentapropista profesional"
data$cat_cedlas[data$estado == 1 & data$cat_ocup == 2 & data$nivel_ed == 6] <- 4 

# "Asalariado en microempresas"
data$cat_cedlas[data$estado == 1 & data$cat_ocup == 3 & !data$cat_cedlas %in% c(2, 3)] <- 5 

# "Cuentapropista no profesional"
data$cat_cedlas[data$estado == 1 & data$cat_ocup == 2 & data$nivel_ed != 6] <- 6 

# "Trabajador sin remuneración"
data$cat_cedlas[data$estado == 1 & data$cat_ocup == 4] <- 7


data$cat_cedlas[data$estado != 1] <- NA
data$cat_cedlas[is.na(data$estado)] <- NA
data$cat_cedlas[is.na(data$cat_ocup)] <- NA
data$cat_cedlas[data$cat_ocup == 3 & data$pp04c == 99 & data$pp04c99 == 9] <- NA

# Calcular formal_def_productiva
data$formal_def_productiva <- NA
data$formal_def_productiva[data$cat_cedlas %in% c(5, 6, 7)] <- "informalidad_productiva"
data$formal_def_productiva[data$cat_cedlas %in% c(1, 2, 3, 4)] <- "formalidad_productiva"

# Calcular formal_def_legal
data$formal_def_legal <- NA
data$formal_def_legal[data$cat_ocup == 3 & data$estado == 1 & data$pp07h == 1] <- "formalidad_legal"
data$formal_def_legal[data$cat_ocup == 3 & data$estado == 1 & data$pp07h == 2] <- "informalidad_legal"

# Filtrar para estado == 1
data <- subset(data, estado == 1)


A <- data %>% 
  dplyr::filter(!is.na(formal_def_productiva)) %>% 
  group_by(ano4, provincia, prov_desc, formal_def_productiva) %>% 
  summarise(pondera = sum(pondera, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = formal_def_productiva, values_from = pondera) %>% 
  mutate(tasa_informalidad_productiva = informalidad_productiva / (formalidad_productiva + informalidad_productiva)) %>% 
  select(anio = ano4, prov_cod = provincia, prov_desc, tasa_informalidad_productiva)

TOTAL_A <- data %>% 
  dplyr::filter(!is.na(formal_def_productiva)) %>% 
  group_by(ano4, formal_def_productiva) %>% 
  summarise(pondera = sum(pondera, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = formal_def_productiva, values_from = pondera) %>% 
  mutate(tasa_informalidad_productiva = informalidad_productiva / (formalidad_productiva + informalidad_productiva)) %>% 
  select(anio = ano4, tasa_informalidad_productiva) %>% 
  mutate(prov_cod = -9,
         prov_desc = "Total país")

A <- bind_rows(A, TOTAL_A)

B <- data %>% 
  dplyr::filter(!is.na(formal_def_legal)) %>%
  group_by(ano4, provincia, prov_desc, formal_def_legal) %>% 
  summarise(pondera = sum(pondera, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = formal_def_legal, values_from = pondera) %>% 
  mutate(tasa_informalidad_legal = informalidad_legal / (formalidad_legal + informalidad_legal)) %>% 
  select(anio = ano4, prov_cod = provincia, prov_desc, tasa_informalidad_legal)

TOTAL_B <- data %>% 
  dplyr::filter(!is.na(formal_def_legal)) %>%
  group_by(ano4, formal_def_legal) %>% 
  summarise(pondera = sum(pondera, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = formal_def_legal, values_from = pondera) %>% 
  mutate(tasa_informalidad_legal = informalidad_legal / (formalidad_legal + informalidad_legal)) %>% 
  select(anio = ano4, tasa_informalidad_legal) %>% 
  mutate(prov_cod = -9,
         prov_desc = "Total país")

B <- bind_rows(B, TOTAL_B)


df_output <- left_join(A, B, by = join_by(anio, prov_cod, prov_desc)) %>% 
  pivot_longer(starts_with("tasa"), 
               names_to = "tipo_informalidad",
               values_to = 'valor') %>%
  mutate(tipo_informalidad = ifelse(tipo_informalidad == "tasa_informalidad_productiva", "Informalidad (definición productiva)",
                                    "Informalidad (definición legal)")) %>% 
  select(anio, prov_cod, prov_desc, tipo_informalidad, valor)

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  pk = c("anio", "prov_cod","tipo_informalidad"),
  drop_output_drive = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente1, fuente2),
    analista = "",
    pk = c("anio", "prov_cod","tipo_informalidad"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "prov_cod",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("valor" = "Tasa de informalidad"),
    unidades = list("valor" = "unidades")
  )

