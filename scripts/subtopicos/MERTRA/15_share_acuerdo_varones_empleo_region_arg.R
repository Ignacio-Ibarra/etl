################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "MERTRA"
output_name <- "share_acuerdo_varones_empleo_region"
fuente1 <- "./data/_FUENTES/raw/EVS_WVS_Joint_csv_v4_0.csv"

#-- Lectura de Datos ----

wvs_df <- readr::read_csv(fuente1)

#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

df_output <- wvs_df %>% 
  dplyr::filter(cntry == 32) %>%
  select(reg_iso, gwght, c001) %>% 
  dplyr::filter(c001 >= 0) %>% 
  mutate(
      region_wvs = case_when(
        reg_iso == 32002 ~ "Provincia de Buenos Aires",
        reg_iso %in% c(32003, 32004, 32007, 32012, 32017, 32024) ~ "Norte",
        reg_iso == 32001 ~ "CABA",
        TRUE ~ "Centro y Sur (exc. CABA y PBA)"
      ),
    region_wvs_code = case_when(
        reg_iso == 32002 ~ 2,
        reg_iso %in% c(32003, 32004, 32007, 32012, 32017, 32024) ~ 3,
        reg_iso == 32001 ~ 4,
        TRUE ~ 1
      ),
    nivel_acuerdo = case_when(
        c001 == 1 ~ "De acuerdo",
        c001 == 2 ~ "En desacuerdo",
        TRUE ~ "Ni de acuerdo ni en desacuerdo"
      )) %>% 
  group_by(region_wvs_code, region_wvs, nivel_acuerdo) %>% 
  summarise(gwght_sum = sum(gwght, na.rm = TRUE), .groups = 'drop') %>%
  group_by(region_wvs_code, region_wvs) %>% 
  mutate(valor = gwght_sum / sum(gwght_sum, na.rm = TRUE)) %>%
  select(-gwght_sum)

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  pk = c("region_wvs_code","nivel_acuerdo" ),
  drop_output_drive = F
  )

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c("R37C1", "R34C2"),
    analista = analista,
    pk = c("anio", "iso3"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("pbi_per_capita_ppa_porcentaje_argentina" = "PBI per cápita PPA como porcentaje del de Argentina"),
    unidades = list("pbi_per_capita_ppa_porcentaje_argentina" = "porcentaje")
  )

