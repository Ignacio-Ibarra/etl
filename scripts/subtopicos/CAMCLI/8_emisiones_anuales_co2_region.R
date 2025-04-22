################################################################################
##                              Dataset: nombre                               ##
################################################################################

rm(list = ls())

#-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "emisiones_anuales_co2_region"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 

emis_anual_co2_region<-readr::read_csv(get_raw_path("R119C0"))
geonomenclador <- argendataR::get_nomenclador_geografico()

#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

## transformo los datos

emis_anual_co2_region <- emis_anual_co2_region %>% 
  mutate(entities_code = ifelse(entities_name %in% c("International shipping", "International aviation"), "TRANS", entities_code))


# me quedo con las variables que necesitamos
emis_anual_co2_region <- emis_anual_co2_region %>% 
  select(-name,-id,-unit,-shortUnit,-datasetId,-entities_id)

# paso a formato long
emis_anual_co2_region_long <- pivot_longer(emis_anual_co2_region, 
              cols = -c(entities_name,entities_code),  # Columnas a mantener fijas
              names_to = "anio",             # Nombre para la columna de años
              values_to = "valor")          # Nombre para la columna de valores

# paso anio a numeric
emis_anual_co2_region_long <- emis_anual_co2_region_long %>%
  mutate(anio = as.numeric(anio))

emis_anual_co2_region_long <- emis_anual_co2_region_long %>%
  group_by(pick(entities_code, anio)) %>% 
  summarise(valor = sum(valor, na.rm = T)) %>% 
  ungroup()

print(paste(rep("#", 80), collapse = ""))
print("Entidades geograficas sin codigo y por tanto excluidas")
unique(emis_anual_co2_region_long$entities_name[is.na(emis_anual_co2_region_long$entities_code)])
print(paste(rep("#", 80), collapse = ""))

emis_anual_co2_region_long <- emis_anual_co2_region_long %>% 
  filter(!is.na(entities_code))

# tragio info genomenclador
emis_anual_co2_region_long <- emis_anual_co2_region_long %>% 
  inner_join(geonomenclador, by = c("entities_code" = "codigo_fundar"))

# dejo la variables que ncesitamos
emis_anual_co2_region_long <- emis_anual_co2_region_long %>% 
  select(entities_code,continente_fundar,anio,valor) %>% 
  rename(iso3 = entities_code,
         valor_en_ton=valor)

## elimino NA en valor
emis_anual_co2_region_long <- emis_anual_co2_region_long %>%
  filter(!is.na(valor_en_ton))

df_output <- emis_anual_co2_region_long

head(df_output)

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

df_anterior <- get_output_repo("emisiones_anuales_co2_region.csv", subtopico = "CAMCLI")

df_anterior <- df_anterior %>% 
  filter(!is.na(iso3)) 


comparacion <- argendataR::comparar_outputs(
  emis_anual_co2_region_long,
  df_anterior = df_anterior,
  subtopico = "CAMCLI",
  pk = c("iso3","anio"),
  drop_joined_df = F
)

#-- Exportar Output ----

check_iso3(df_output$iso3)

df_output$iso3[df_output$iso3 == "OWID_KOS"] <- "XKX"

df_output$iso3[df_output$iso3 == "OWID_WRL"] <- "WLD"

check_iso3(df_output$iso3)

 
# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    control = comparacion,
    subtopico = "CAMCLI",
    fuentes = c("R119C0"),
    analista = "",
    pk = c("anio", "iso3"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("valor_en_ton" = "Valor emisiones co2 en troneladas"),
    unidades = list("valor_en_ton" = "toneladas")
  )



mandar_data(paste0(output_name, ".csv"), subtopico = "CAMCLI", branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = "CAMCLI",  branch = "dev")


