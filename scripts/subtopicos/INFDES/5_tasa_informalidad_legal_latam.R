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
  anti_join(empalmados %>% select(iso3) , join_by(iso3))

  
df_output <- bind_rows(empalmados, originales) %>% 
  mutate(valor = valor/100)%>% 
  select(-fuente, -fuente_orden, -apertura)


#-- Controlar Output ----

comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
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
    etiquetas_indicadores = list("valor" = "Tasa de informalidad legal"),
    unidades = list("valor" = "unidades")
  )

