################################################################################
##                              Dataset: nombre                               ##
################################################################################

rm(list = ls())

#-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "emisiones_per_cap"

#-- Librerias ----

#-- Lectura de Datos ----


fuente1 <- "R468C0"
fuente2 <- "R472C0"
fuentes_usadas <- c(fuente1, fuente2)

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
emisiones<-readr::read_csv(argendataR::get_raw_path(fuente1)) %>% 
  janitor::clean_names() %>% 
  rename(anio = year, valor = annual_emissions_ghg_total_co2eq)

poblacion <-readr::read_csv(argendataR::get_raw_path(fuente2)) %>% 
  janitor::clean_names() 

poblacion <- poblacion %>%
  mutate(poblacion = ifelse(is.na(population_historical), population_projection, population_historical)) %>% 
  rename(anio = year)

poblacion <- poblacion %>%
   filter(anio >= 1700 & anio <= year(today()))

poblacion <- poblacion %>%
  select(code, anio, poblacion)


emisiones %>% 
  count(code, anio) %>% 
  count(code, n) %>% 
  filter(n != 1)

poblacion %>% 
  count(code, anio) %>% 
  count(code, n) %>% 
  filter(n != 1)

emisiones <- emisiones %>% 
  filter(!is.na(code))

poblacion <- poblacion %>% 
  filter(!is.na(code))

df <- left_join(emisiones, poblacion, by = c("code", "anio"))

df <- df %>% 
  mutate(valor_per_cap = valor/poblacion) %>% 
  select(code, anio, valor_per_cap, valor , poblacion)

df %>% 
  filter(is.na(valor_per_cap)) %>% 
  distinct(code)

geonomenclador <- argendataR::get_nomenclador_geografico_front()


check_iso3(df$code )



df$code[df$code == "OWID_WRL"] <- "WLD"


check_iso3(df$code)

# tragio info genomenclador
df <- df %>% 
  inner_join(geonomenclador, by = c("code" = "geocodigo"))

# dejo la variables que ncesitamos
df <- df %>% 
  rename(geocodigoFundar = code, geonombreFundar = name_long, emisiones = valor) 

df <- df %>% 
  select(-c(name_short, iso_2))

df <- df %>% 
  filter(!is.na(valor_per_cap))


#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

# me quedo con las variables que necesitamos
# emis_per_cap_co2_toneladas <- emis_per_cap_co2_toneladas %>% 
#   select(-name,-id,-unit,-shortUnit,-datasetId,-entities_id)

# # paso a formato long
# emis_per_cap_co2_toneladas_long <- pivot_longer(emis_per_cap_co2_toneladas, 
#                                            cols = -c(entities_name,entities_code),  # Columnas a mantener fijas
#                                            names_to = "anio",             # Nombre para la columna de años
#                                            values_to = "valor")          # Nombre para la columna de valores
# 
# # paso anios a numeric
# emis_per_cap_co2_toneladas_long <- emis_per_cap_co2_toneladas_long %>%
#   mutate(anio = as.numeric(anio))

# traigo info geonomenclagor
# emis_per_cap_co2_toneladas_long <- emis_per_cap_co2_toneladas_long %>% 
#   inner_join(geonomenclador, by = c("entities_code" = "codigo_fundar"))

# dejo la variables que ncesitamos
# emis_per_cap_co2_toneladas_long <- emis_per_cap_co2_toneladas_long %>% 
#   select(entities_code,anio,valor) %>% 
#   rename(iso3 = entities_code,
#          valor_en_ton=valor)

## elimino NA en valor
# emis_per_cap_co2_toneladas <- emis_per_cap_co2_toneladas %>%
#   filter(!is.na(valor) &!is.na(code))

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


df_anterior <- descargar_output(output_name, subtopico = "CAMCLI")

comparacion <- argendataR::comparar_outputs(
  df,
  df_anterior = df_anterior %>%  rename(valor_per_cap = valor_en_ton),
  k_control_num = 3,
  pk = c("geocodigoFundar","anio"),
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
    fuentes = fuentes_usadas,
    analista = "",
    pk = c("anio", "geocodigoFundar"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "geocodigoFundar",
    nivel_agregacion = "pais",
    aclaraciones = "Cambia la fuente utilizada. Se representan emisiones GEI.",
    etiquetas_indicadores = list( "geocodigoFundar" = "codigo geonomenclador",
                                  "geonombreFundar" = "Nombre geografico", "anio" = "Año",
                                  "emisiones" = "Emisiones GEI en toneladas de CO2 equivalentes",
                                  "poblacion" = "Poblacion estimada",
                                 "valor_per_cap" = "Emisiones GEI en toneladas de CO2 equivalentes per capita"),
    unidades = list("valor_per_cap" = "ton CO2 per capita")
  )



mandar_data(paste0(output_name, ".csv"), subtopico = "CAMCLI", branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = "CAMCLI",  branch = "main")


