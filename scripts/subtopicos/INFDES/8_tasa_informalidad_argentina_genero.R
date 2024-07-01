################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "INFDES"
output_name <- "tasa_informalidad_argentina_genero"
fuente1 <- "R115C32"


#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
inf_legal_def <- readr::read_csv(argendataR::get_temp_path(fuente1))

#-- Procesamiento ----

df_output <- inf_legal_def %>% 
  dplyr::filter(apertura %in% c("Adultos (25-64), Género, Mujer", "Adultos (25-64), Género, Masculino") & (serie == "Serie empalmada")) %>% 
  dplyr::filter(iso3 == "ARG") %>% 
  mutate(genero = ifelse(apertura == "Adultos (25-64), Género, Mujer", "Mujer", "Varón"),
         valor = valor /100) %>% 
  select(anio, genero, valor) %>% 
  pivot_wider(names_from = "genero", values_from = "valor") %>% 
  mutate(Brecha = get("Varón") - get("Mujer")) %>% 
  pivot_longer(cols = -anio, names_to = "apertura_sexo", values_to = "valor")



#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  pk = c("anio","apertura_sexo"),
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
    pk = c("anio","apertura_sexo"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("valor" = "Tasa de informalidad"),
    unidades = list("valor" = "porcentaje")
  )