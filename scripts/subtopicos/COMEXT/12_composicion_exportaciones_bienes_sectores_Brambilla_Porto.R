################################################################################
##                              Dataset: nombre                               ##
################################################################################
#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "AGROPE"
output_name <- "12_composicion_exportaciones_bienes_sectores_Brambilla_Porto.csv"
analista = "Leonardo Park"
fuente1 <- "R422C0" # BACI HS96


geonomenclador <- argendataR::get_nomenclador_geografico()

source("scripts/utils/baci_data.R")

con <- argendataR::get_raw_path(fuente1) %>% 
  BACI.get_db_from_zip(.)

query_output <- glue::glue(
  "SELECT c.i as m49_code, p.country_iso3 as iso3, c.k as ncm6, SUM(c.v) as expo
   FROM comercio as c
   WHERE t == 2020
   AND c.t = (SELECT MAX(t) FROM comercio)
   GROUP BY c.i, p.country_iso3, c.k"
)


df_query <- dbGetQuery(con, query_output)

comex_sectores_brambilla_porto <-arrow::read_parquet(argendataR::get_clean_path("R113C57"))



#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

df_output <- comex_sectores_brambilla_porto %>% 
  dplyr::select(year, iso3, country_name_abbreviation, sector_bp, sector_bp_name, export_value_pc)

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso




df_anterior <- descargar_output(nombre = output_name, subtopico = "COMEXT", entrega_subtopico = "datasets_primera_entrega")

comparacion <- argendataR::comparar_outputs(df = df_output, df_anterior = df_anterior,
                                            pk = c("year", "iso3", "sector_bp", "sector_bp_name"))

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  rename(anio = year) %>% # Modifico colname -year
  argendataR::write_output(
    control = comparacion, 
    directorio = "data/COMEXT/", 
    aclaraciones = "Minimas desvisaciones en tests y valores en comparacion con dataset de analista. Valores de Argentina casi equivalentes",
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c("R113C57"),
    analista = analista,
    pk = c("anio", "iso3", "sector_bp", "sector_bp_name"),
    es_serie_tiempo = FALSE,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("export_value_pc" = "Exportaciones de bienes (% del total exportado en bienes)"),
    unidades = list("export_value_pc" = "porcentaje")
  )