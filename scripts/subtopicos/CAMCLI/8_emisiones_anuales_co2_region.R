################################################################################
##                              Dataset: nombre                               ##
################################################################################

rm(list = ls())

#-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name_old <- "emisiones_anuales_co2_region"
output_name <- "emisiones_anuales_gei_region"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 

df <-readr::read_csv(get_raw_path("R468C0"))
geonomenclador <- argendataR::get_nomenclador_geografico_front()

#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

## transformo los datos

# no cinlcuye transprote 
# df <- emis_anual_co2_region %>% 
#   mutate(entities_code = ifelse(entities_name %in% c("International shipping", "International aviation"), "TRANS", entities_code))

df <- df %>%
  janitor::clean_names() %>% 
  rename(anio = year, valor  = annual_emissions_ghg_total_co2eq) 



print(paste(rep("#", 80), collapse = ""))
print("Entidades geograficas sin codigo y por tanto excluidas")
unique(df$entity[is.na(df$code)])
print(paste(rep("#", 80), collapse = ""))

df <- df %>% 
  filter(!is.na(code))


check_iso3(df$code)

df$code[df$code == "OWID_WRL"] <- "WLD"


check_iso3(df$code)


# tragio info genomenclador
df <- df %>% 
  inner_join(geonomenclador, by = c("code" = "geocodigo"))

# dejo la variables que ncesitamos
df <- df %>% 
  select(geocodigoFundar = code, geonombreFundar = name_long, anio,valor) 
        
## elimino NA en valor
df <- df %>%
  filter(!is.na(valor))

df_output <- df

head(df_output)

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

df_anterior <- get_output_repo("emisiones_anuales_co2_region.csv", subtopico = "CAMCLI")



comparacion <- argendataR::comparar_outputs(
  df,
  df_anterior = df_anterior %>% 
    rename(valor = valor_en_ton),
  subtopico = "CAMCLI",
  pk = c("geocodigoFundar","anio"),
  drop_joined_df = T
)

#-- Exportar Output ----

 
# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    cambio_nombre_output = list('nombre_nuevo' = output_name,
                                'nombre_anterior' = output_name_old),
    control = comparacion,
    subtopico = "CAMCLI",
    fuentes = c("R468C0"),
    analista = "",
    pk = c("anio", "geocodigoFundar"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "geocodigoFundar",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list( "geocodigoFundar" = "codigo geonomenclador",
                                  "geonombreFundar" = "Nombre geografico", "anio" = "Año",
                                 "valor" = "Valor emisiones en toneladas de CO2 equivalentes"),
    unidades = list("valor" = "toneladas")
  )



mandar_data(paste0(output_name, ".csv"), subtopico = "CAMCLI", branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = "CAMCLI",  branch = "main")


