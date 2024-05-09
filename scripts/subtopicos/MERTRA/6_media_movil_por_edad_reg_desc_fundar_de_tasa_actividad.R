################################################################################
## Dataset: Tasa de actividad por edad y región, media móvil 5 años           ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "MERTRA"
output_name <- "media_movil_por_edad_reg_desc_fundar_de_tasa_actividad"
fuente1 <- "R49C0"  # Cambiar luego por la fuente clean. 
fuente2 <- "R84C14"


#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
ephtu_df <- readr::read_csv(argendataR::get_temp_path(fuente1))
ephtu_df <- ephtu_df %>% rename_with(tolower, everything()) #esta linea no haría falta que esté cuando cambiemos el input de fuente1 por la fuente clean. 

codigos <- readr::read_csv(argendataR::get_temp_path(fuente2))
codigos <- codigos %>% select(aglomerado = aglom_cod_indec, provincia = prov_cod, reg_desc_fundar)

#-- Procesamiento ----

ephtu_df <- ephtu_df %>% 
  left_join(codigos, by = join_by(aglomerado, provincia))

ephtu_df <- ephtu_df %>% mutate(
  activo = case_when(
    estado == 1 | estado == 2 ~ 'activo',
    TRUE ~ 'no_activo'
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
  mutate(activo = rollsum(activo, 5, align="right", fill = 0),
         no_activo = rollsum(no_activo, 5, align = "right", fill = 0)) %>% 
  mutate(tasa_actividad = activo / (no_activo + activo)) %>% 
  filter(edad >= 10 & edad <= 90) %>% 
  select(anio, edad, tasa_Todas = tasa_actividad)



regiones <- sort(unique(ephtu_df$reg_desc_fundar))

c <- ephtu_df %>% 
  select(anio = ano4, activo, edad = ch06, reg_desc_fundar, pondera) %>% 
  group_by(anio, edad, reg_desc_fundar, activo) %>% 
  summarize(pondera = sum(pondera)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = c(activo, reg_desc_fundar), values_from = pondera, values_fill = 0)

for (r in regiones){
  str_activo <- paste0("activo_",r)
  str_no_activo <- paste0("no_activo_", r)
  str_rolled_activo <- paste0("rolled_activo_",r)
  str_rolled_no_activo <- paste0("rolled_no_activo_",r)
  c <- c %>% 
    mutate(!!paste0("rolled_",str_activo) := rollsum(get(str_activo), k= 5, by=1, align = "right", fill = 0),
           !!paste0("rolled_", str_no_activo) := rollsum(get(str_no_activo), k= 5, by = 1, align = "right", fill = 0),
           !!paste0("tasa_",r) := get(str_rolled_activo) / (get(str_rolled_activo) + get(str_rolled_no_activo))
    )
}

df_output <- c %>%
  filter(edad >= 10 & edad <= 90) %>%
  select(anio, edad, starts_with("tasa_")) %>% 
  left_join(., a_total, by=join_by(anio, edad)) %>% 
  pivot_longer(cols = starts_with("tasa_"), names_to = "reg_desc_fundar", values_to = "tasa_actividad") %>% 
  mutate(reg_desc_fundar = str_replace(reg_desc_fundar, "tasa_", ""))




#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  pk = c("anio", "edad", "reg_desc_fundar"),
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
    pk = c("anio", "edad"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    etiquetas_indicadores = list("tasa_actividad" = "Media móvil por quinquenios etarios de la tasa de actividad"),
    unidades = list("tasa_actividad" = "unidades")
  )

