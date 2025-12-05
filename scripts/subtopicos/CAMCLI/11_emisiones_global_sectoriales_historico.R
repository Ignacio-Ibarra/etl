################################################################################
##                              Dataset: nombre                               ##
################################################################################

rm(list = ls())
gc()

#-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "emisiones_global_sectoriales_historico"
old_name <- "emisiones_global_sec_1850_2014"
f1 <- "R132C56"
#-- Librerias ----

#-- Lectura de Datos ----


# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
df <- read_fuente_clean(f1)

df <- df %>% 
  filter(entity == "KYOTOGHG (AR6GWP100)" & scenario_primap_hist == "HISTTP" &
           area_iso3 == "EARTH")

df %>% 
  filter(category_ipcc2006_primap %in% c("1","2","4","5","M.AG")) %>%
  distinct(entity, category_ipcc2006_primap) 



df <- df %>% 
  rename(category = category_ipcc2006_primap) %>%
  # Quédate solo con categorías agregadas para evitar doble conteo:
  filter(category %in% c("1","2","4","5","M.AG","M.LULUCF")) %>%
  mutate(sector = case_when(
    category == "1"                  ~ "Energía",
    category == "2"                  ~ "PIUP",
    category %in% c("M.AG") ~ "AGSyOUT",   # Agricultura 
    category == "4"                  ~ "Residuos",
    category == "5"                  ~ "Otros",
    TRUE                             ~ NA_character_
  )) %>%
  group_by(anio, sector, area_iso3, unit, entity) %>%
  summarise(valor_en_ggco2e = sum(valor, na.rm = TRUE)) %>% 
  ungroup()

df %>% 
  filter(anio >= 1970) %>% 
  ggplot(aes(x = anio, y = valor_en_ggco2e, color = sector, group = sector)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = -45))

check_iso3(df$area_iso3)

df$area_iso3 <- "WLD"

check_iso3(df$area_iso3)

df <- df %>% 
  rename(geocodigoFundar = area_iso3)

df <- df %>% 
  left_join(argendataR::get_nomenclador_geografico_front() %>% 
              select(geocodigoFundar = geocodigo, geonombreFundar = name_long))


df <- df %>% 
  mutate(anio = as.numeric(anio))

#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


df_anterior <- descargar_output(nombre = old_name, subtopico = "CAMCLI")

comparacion <- argendataR::comparar_outputs(
  df,
  df_anterior = df_anterior %>% 
    mutate(anio = as.numeric(anio)),
  nombre = output_name,
  k_control_num = 3,
  pk = c("sector","anio"),
  drop_joined_df = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df %>%
  argendataR::write_output(
    output_name = output_name,
    control = comparacion,
    subtopico = "CAMCLI",
    fuentes = c("R132C56"),
    analista = "",
    pk = c("anio", "sector"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    #columna_geo_referencia = "iso3",
    nivel_agregacion = "mundo",
    aclaraciones = "Cambio primap no rounding por versión final con redondeo. No incluye cambio de uso de la tierra por inconsistencia de la serie alrededor 1990",
    etiquetas_indicadores = list(anio = "Año",
                                 geocodigoFundar = "codigo geonomenclador",
                                 unit = "unidades",
                                 entity = "Estándar de seguimiento de GEI",
                                 valor_en_ggco2e = "emisiones de GEI en gigagramos de CO2 equivalentes",
                                 geonombreFundar = "nombre entidad geografica",
                                 "sector" = "Sector según IPCC‑2006 reagrupado"),
    unidades = list("valor_en_ggco2e" = "miles de toneladas (gigagramos) de CO2 equivalente")
  )



mandar_data(paste0(output_name, ".csv"), subtopico = "CAMCLI", branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = "CAMCLI",  branch = "main")


