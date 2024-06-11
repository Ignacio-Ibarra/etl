################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "INFDES"
output_name <- "tasa_informalidad_legal_latam"
fuente1 <- "R115C32"


#-- Procesamiento


inf_legal_def <- readr::read_csv(argendataR::get_temp_path(fuente1)) 

data <- inf_legal_def %>% 
  dplyr::filter(apertura %in% c("Edad total")) %>% 
  select(-topico, -tematica, -variable)


# Me quedo copn los que tienen empalme completo. 
empalmados <- data %>% 
  dplyr::filter(serie == "Serie empalmada") 


originales <- data %>% 
  dplyr::filter(serie == "Serie original") %>% 
  anti_join(empalmados %>% select(iso3) , join_by(iso3)) %>% 
  group_by(iso3) %>% 
  mutate(n_fuentes = n_distinct(fuente)) %>% 
  ungroup() 

originales_unica_fuente <- originales %>% 
  filter(n_fuentes == 1)


originales_multi_fuente <- originales %>% 
  filter(n_fuentes>1) %>% 
  group_by(iso3, fuente) %>% 
  mutate(anio.max.iso.fuente = max(anio)) %>% 
  ungroup() %>% 
  group_by(iso3) %>% 
  mutate(anio.max.iso = max(anio)) %>% 
  ungroup() %>% 
  filter(anio.max.iso == anio.max.iso.fuente)



df_output <- bind_rows(empalmados, originales_unica_fuente, originales_multi_fuente) %>% 
  select(iso3, pais, anio, valor, serie) %>% 
  mutate(valor = valor/100)

df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")

#-- Controlar Output ----

comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  pk = c("iso3","anio"),
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
    pk =  c("iso3","anio"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    nivel_agregacion = "pais",
    aclaraciones = "Este dataset contiene solo series de datos comparables. Es decir, en aquellos países donde se puedo hacer un empalme entre fuentes que poseían años en común se utilizó el dato de empalme. En países donde no se pudo hacer empalme y se poseía más de una fuente de información, se mantuvieron unicamente las fuentes de datos más actuales",
    etiquetas_indicadores = list("valor" = "Tasa de informalidad legal"),
    unidades = list("valor" = "unidades")
  )

