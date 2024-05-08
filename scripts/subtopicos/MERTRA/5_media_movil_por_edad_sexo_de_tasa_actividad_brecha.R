################################################################################
## Dataset: Tasa de actividad por sexo y edad (media móvil 5 años), por año       ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "MERTRA"
output_name <- "media_movil_por_edad_sexo_de_tasa_actividad_brecha"
fuente1 <- "R49C0"  # Cambiar luego por la fuente clean. 
# fuente2 <- "R84C14"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
ephtu_df <- readr::read_csv(argendataR::get_temp_path(fuente1))
ephtu_df <- ephtu_df %>% rename_with(tolower, everything()) #esta linea no haría falta que esté cuando cambiemos el input de fuente1 por la fuente clean. 

#-- Procesamiento ----

ephtu_df <- ephtu_df %>% mutate(
  activo = case_when(
    estado == 1 | estado == 2 ~ 1,
    TRUE ~ 0
  ),
  # ocupado = case_when(
  #   estado == 1 ~ 1,
  #   TRUE ~ 0
  # )
)


a_total <- ephtu_df %>% 
  select(anio = ano4, activo, edad = ch06, pondera) %>% 
  group_by(anio, edad, activo) %>% 
  summarize(pondera = sum(pondera)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = activo, values_from = pondera, values_fill = 0) %>% 
  rename(activo = `1`, no_activo = `0`) %>% 
  mutate(activo = rollsum(activo, 5, align="right", fill = 0),
         no_activo = rollsum(no_activo, 5, align = "right", fill = 0)) %>% 
  mutate(tasa_actividad = activo / (no_activo + activo)) %>% 
  filter(edad >= 10 & edad <= 90) %>% 
  select(anio, edad, tasa_total = tasa_actividad)

b <- ephtu_df %>% 
  select(anio = ano4, activo, edad = ch06, sexo = ch04, pondera) %>% 
  group_by(anio, edad, sexo, activo) %>% 
  summarize(pondera = sum(pondera)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = c(activo, sexo), values_from = pondera, values_fill = 0) %>% 
  rename(no_activo_varon = `0_1`, 
         no_activo_mujer = `0_2`, 
         activo_varon = `1_1`,
         activo_mujer = `1_2`) %>% 
  mutate(rolled_no_activo_mujer = rollsum(no_activo_mujer, k = 5, by = 1, align = "right", fill = 0),
         rolled_no_activo_varon = rollsum(no_activo_varon, k = 5, by = 1, align = "right", fill = 0),
         rolled_activo_mujer = rollsum(activo_mujer, k = 5, by = 1, align = "right", fill = 0),
         rolled_activo_varon = rollsum(activo_varon, k = 5, by = 1, align = "right", fill = 0))%>% 
  mutate(tasa_varon = rolled_activo_varon / (rolled_activo_varon + rolled_no_activo_varon),
         tasa_mujer = rolled_activo_mujer / (rolled_activo_mujer + rolled_no_activo_mujer),
         brecha_tasa = tasa_mujer - tasa_varon) %>%
  filter(edad >= 10 & edad <= 90) %>%
  select(anio, edad, tasa_varon, tasa_mujer, brecha_tasa)


df_output <- left_join(a_total, b, by = join_by(anio, edad)) %>% 
  pivot_longer(cols = contains("tasa") , names_to = "apertura_sexo", values_to = "valor" ) %>%
  mutate(apertura_sexo = str_replace(apertura_sexo, "(tasa_)|(_tasa)", ""))
  
  

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  pk = c("anio", "edad", "apertura_sexo"),
  drop_output_drive = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente1),
    analista = "",
    pk = c("anio", "edad","apertura_sexo"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    etiquetas_indicadores = list("valor" = "Media móvil por quinquenios etarios de la tasa de actividad"),
    unidades = list("valor" = "porcentaje")
  )

