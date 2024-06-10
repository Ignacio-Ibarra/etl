################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "INFDES"
output_name <- "brecha_tasa_informalidad_genero_latam_2000_2021"
fuente1 <- "R115C32"


#-- Procesamiento


inf_legal_def <- readr::read_csv(argendataR::get_temp_path(fuente1)) 

data <- inf_legal_def %>% 
  dplyr::filter(apertura %in% c("Adultos (25-64), Género, Mujer", "Adultos (25-64), Género, Masculino")) %>% 
  select(-topico, -tematica, -variable)

# Función para encontrar el año más cercano
anio_mas_proximo <- function(serie, anio = 2017, thresh = NULL) {
  a <- as.numeric(serie)
  b <- a[which.min(abs(a - anio))]
  if (!is.null(thresh)) {
    if (abs(b - anio) > thresh) {
      return(NA)
    } else {
      return(as.integer(b))
    }
  } else {
    return(as.integer(b))
  }
}

# anio.circa.end <- max(data$anio)
anio.circa.end <- 2021
anio.circa.start <- 2000

# Filtro filas con anios que estan dentro del circa 2000 y circa ultimo año
circa <- data %>% 
  group_by(iso3,serie) %>%
  mutate(selection = (anio == anio_mas_proximo(anio, anio.circa.end, 2)) |( anio == anio_mas_proximo(anio, anio.circa.start, 2))) %>%
  ungroup() %>% 
  dplyr::filter(selection)


# Me quedo copn los que tienen empalme completo. 
empalmados <- circa %>% 
  dplyr::filter(serie == "Serie empalmada") %>% 
  group_by(iso3) %>% 
  mutate(completo = n() == 4) %>% 
  ungroup() %>% 
  dplyr::filter(completo) %>% 
  select(-selection, -completo) 



originales <- circa %>% 
  dplyr::filter(serie == "Serie original") %>% 
  anti_join(empalmados , join_by(iso3, anio, apertura)) %>% 
  group_by(iso3) %>% 
  mutate(completo= n() == 4) %>% 
  ungroup() %>% 
  dplyr::filter(completo) %>% 
  select(-selection, -completo)
  

df_output <- bind_rows(empalmados, originales) %>% 
  select(-fuente, -fuente_orden) %>% 
  mutate(apertura = ifelse(apertura == "Adultos (25-64), Género, Mujer", "mujer", "varon")) %>% 
  pivot_wider(names_from = apertura, values_from = valor) %>% 
  mutate(brecha = mujer - varon,
         circa = ifelse(abs(anio-anio.circa.start)<=4,anio.circa.start, anio.circa.end)) %>% 
  arrange(circa, iso3) %>% 
  rename(c("anio_circa"="anio"))

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  pk = c("iso3","circa"),
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
    pk = c("anio","apertura_sexo"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("valor" = "Tasa de informalidad"),
    unidades = list("valor" = "porcentaje")
  )



A <- inf_legal_def %>% 
  dplyr::filter(serie == "Serie empalmada") %>% 
  dplyr::filter(iso3 == "URY") %>% 
  dplyr::filter(apertura %in% c("Adultos (25-64), Género, Mujer", "Adultos (25-64), Género, Masculino"))
