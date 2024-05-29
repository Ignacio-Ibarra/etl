################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "INFDES"
output_name <- "tasa_desempleo_ephtu_sexo_edad"
fuente1 <- "R49C16"



#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
ephtu_df <- readr::read_csv(argendataR::get_temp_path(fuente1))


#-- Procesamiento ----

data <- data.frame(ephtu_df) %>% 
  select(anio = ano4, sexo = ch04, edad = ch06, estado, pondera) %>% 
  mutate(
    desocupado = case_when(
      estado == 2 ~ 1,
      estado == 1 ~ 0,
      TRUE ~ NA
    ),
    rango_edad = case_when(
      edad < 30 ~ 1,
      edad >= 30 & edad < 40 ~ 2,
      edad >=40 & edad < 50 ~ 3,
      edad >= 50 ~ 4
    ),
    rango_edad_desc = case_when(
      edad < 30 ~ "Hasta 30",
      edad >= 30 & edad < 40 ~ "Entre 30 y 39",
      edad >=40 & edad < 50 ~ "Entre 40 y 49",
      edad >= 50 ~ "50 y más"
    ),
    sexo = case_when(
      sexo == 1 ~ "Varones",
      sexo == 2 ~ "Mujeres",
      TRUE ~ NA
    )
  ) %>% 
  dplyr::filter(!is.na(desocupado)) %>% 
  group_by(anio, desocupado, sexo, rango_edad, rango_edad_desc) %>% 
  summarise(pondera = sum(pondera, na.rm=T)) %>% 
  ungroup()


df_output <- data %>% 
  group_by(anio, sexo, rango_edad, rango_edad_desc) %>% 
  mutate(totalactivos = sum(pondera),
         tasa_desocupacion = pondera / totalactivos) %>%
  dplyr::filter(desocupado != 0) %>% 
  select(anio, rango_edad, rango_edad_desc, sexo, tasa_desocupacion)
  




#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  pk = c("anio", "rango_edad","sexo"),
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
    pk = c("anio", "rango_edad","sexo"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    etiquetas_indicadores = list("tasa_desocupacion" = "Tasa de desocupación"),
    unidades = list("tasa_desocupacion" = "unidades")
  )
