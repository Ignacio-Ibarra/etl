################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "INFDES"
output_name <- "tasa_informalidad_legal_por_edad_sexo"
fuente1 <- "R49C16"


#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
ephtu_df <- readr::read_csv(argendataR::get_temp_path(fuente1))


#-- Procesamiento ----

data <- data.frame(ephtu_df) %>% 
  select(ano4, ch04, ch06, estado, cat_ocup, pp07h, pondera)


# Calcular formal_def_legal
data$formal_def_legal <- NA
data$formal_def_legal[data$cat_ocup == 3 & data$estado == 1 & data$pp07h == 1] <- "formal_legal"
data$formal_def_legal[data$cat_ocup == 3 & data$estado == 1 & data$pp07h == 2] <- "informal_legal"

# Filtrar para estado == 1
data <- data %>% 
  dplyr::filter(!is.na(formal_def_legal))


data_sexo_total <- data %>% 
  group_by(ano4, ch06, pp07h, estado, cat_ocup, formal_def_legal) %>% 
  summarise(pondera = sum(pondera, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(ch04 = 9)

data <- bind_rows(data, data_sexo_total) %>% 
  mutate(ch04 = case_when(
    ch04 == 1 ~ "masculino",
    ch04 == 2 ~ "femenino",
    TRUE ~ "total"
  ))

data_sexo <- data %>% 
  group_by(ano4, formal_def_legal, ch04, ch06) %>% 
  summarise(pondera = sum(pondera, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = c(formal_def_legal,ch04), values_from = pondera, values_fill = 0) %>% 
  arrange(ano4, ch06) %>% 
  mutate(
    rolled_formal_legal_femenino = zoo::rollsum(formal_legal_femenino, k=5, align = "right", fill=NA, na.pad = T)
  )

sexos <- unique(data$ch04)

for (s in sexos){
  str_formal <- paste0("formal_legal_",s)
  str_informal <- paste0("informal_legal_", s)
  str_rolled_formal <- paste0("rolled_formal_legal_",s)
  str_rolled_informal <- paste0("rolled_informal_legal_",s)
  data_sexo <- data_sexo %>% 
    mutate(!!str_rolled_formal := zoo::rollsum(get(str_formal), k=5, align = "right", fill=NA, na.pad = T),
           !!str_rolled_informal := zoo::rollsum(get(str_informal),  k=5, align = "right", fill=NA, na.pad = T),
           !!s:= get(str_rolled_informal) / (get(str_rolled_informal) + get(str_rolled_formal))
    )
}



df_output <- data_sexo %>% 
  dplyr::filter(ch06>=18 & ch06<=70) %>% 
  select(anio = ano4, edad = ch06, femenino, masculino, total) %>% 
  pivot_longer(c(femenino, masculino, total), 
               names_to = "apertura_sexo", 
               values_to = "tasa_informalidad_legal")
  


#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  pk = c("anio", "edad","apertura_sexo"),
  drop_joined_df = F
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
    control = comparacion,
    pk = c("anio", "edad","apertura_sexo"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    etiquetas_indicadores = list("tasa_informalidad_legal" = "Tasa de informalidad legal"),
    unidades = list("tasa_informalidad_legal" = "unidades")
  )

