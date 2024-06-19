################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "INFDES"
output_name <- "anios_educacion_genero_por_pais"
fuente1 <- "R116C42"


#-- Procesamiento


age_gender_df <- readr::read_csv(argendataR::get_temp_path(fuente1)) 

data <- age_gender_df %>% 
  dplyr::filter(apertura %in% c("(25-65), Mujer", "(25-65), Hombre")) %>% 
  select(-topico, -tematica, -variable)


# Me quedo copn los que tienen empalme completo. 
empalmados <- data %>% 
  dplyr::filter(serie == "Serie empalmada") 


originales <- data %>% 
  dplyr::filter(serie == "Serie original") %>% 
  anti_join(empalmados %>% select(iso3) , join_by(iso3))


df_output <- bind_rows(empalmados, originales) %>% 
  mutate(apertura_sexo = ifelse(apertura == "(25-65), Mujer", "femenino", "masculino"))%>%
  select(-fuente, -fuente_orden, -apertura, -serie)


#-- Controlar Output ----

comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  pk = c("iso3","anio", "apertura_sexo"),
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
    etiquetas_indicadores = list("valor" = "Anios de educación promedio"),
    unidades = list("valor" = "unidades")
  )
