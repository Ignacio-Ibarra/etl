################################################################################
##          Dataset: Puestos per cápita por año y país                        ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

subtopico <- "MERTRA"
output_name <- "puestos_percapita_anio_pais"
fuente1 <- "R92C15"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
pwt_df <- readr::read_csv(argendataR::get_temp_path(fuente1))


#-- Procesamiento ----

df_output <- pwt_df %>% 
  select(iso3 = countrycode, anio = year, emp, pop) %>% 
  mutate(puestos_per_capita = emp/pop) %>% 
  select(iso3, anio, puestos_per_capita)

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


df_anterior <- descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "datasets_primera_entrega")

comparacion <- argendataR::comparar_outputs(df = df_output, df_anterior = df_anterior,
                                            nombre = output_name,  pk = c("anio", "iso3"))





#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    control = comparacion,
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente1),
    analista = "",
    pk = c("anio", "iso3"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("puestos_per_capita" = "Cantidad de puestos de trabajo sobre el total de la poblacion"),
    unidades = list("puestos_per_capita" = "porcentaje")
  )

