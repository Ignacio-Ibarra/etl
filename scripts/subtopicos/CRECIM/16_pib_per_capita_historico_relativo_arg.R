################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "CRECIM"
output_name <- "pib_per_capita_historico_relativo_arg.csv"
analista = "Pablo Sonzogni"
fuente1 <- "R219C90"
fuente2 <- "R219C91"


get_raw_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}raw/")
  df_fuentes_raw <- fuentes_raw() 
  path_raw <- df_fuentes_raw[df_fuentes_raw$codigo == codigo,c("path_raw")]
  return(paste0(prefix, path_raw))
}

get_clean_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}clean/")
  df_fuentes_clean <- fuentes_clean() 
  path_clean <- df_fuentes_clean[df_fuentes_clean$codigo == codigo,c("path_clean")]
  return(paste0(prefix, path_clean))
}

geonomenclador <- argendataR::get_nomenclador_geografico() 

# Cargo data desde server
df_madd_c <- arrow::read_parquet(get_clean_path(fuente1)) %>% 
  select(anio, iso3, area_desc = pais_nombre, gdppc) %>% 
  dplyr::filter(anio >= 1820)

df_madd_r <- arrow::read_parquet(get_clean_path(fuente2)) %>%
  select(anio, iso3, area_desc = region, gdppc)%>% 
  dplyr::filter(anio >= 1820)

df_all <- df_madd_c %>% 
  bind_rows(df_madd_r) %>% 
  dplyr::filter(!is.na(gdppc))


df_arg <- df_madd_c %>% dplyr::filter(iso3 == "ARG") %>% 
  select(anio, pib_pc_arg = gdppc)

df_output <- df_all %>% 
  rename(pib_per_capita = gdppc) %>% 
  # left_join(geonomenclador, join_by(iso3)) %>% 
  # mutate(nivel_agregacion = ifelse(is.na(nivel_agregacion), "agregacion", nivel_agregacion)) %>% 
  left_join(df_arg, by=join_by(anio)) %>% 
  mutate(relativo_arg = pib_pc_arg / pib_per_capita) %>% 
  select(-pib_per_capita, -pib_pc_arg) %>% 
  dplyr::filter(iso3!="ARG")
  
df_output <- df_output %>% 
  select(-area_desc)

check_iso3(df_output$iso3)

df_output <- df_output %>% 
  mutate(iso3 = case_when(
    iso3 == "YUG" ~ "SER",
    iso3 == "SUN" ~ "SVU",
    T ~ iso3
  ))

check_iso3(df_output$iso3)

# mutate(nivel_agregacion = ifelse(is.na(nivel_agregacion), "agregacion", nivel_agregacion))


df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")  

df_anterior <- df_anterior %>% 
  mutate(iso3 = case_when(
    iso3 == "YUG" ~ "SER",
    iso3 == "SUN" ~ "SVU",
    iso3 == "WRL_MPD" ~ "WLD",
    T ~ iso3
  ))

comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("anio", "iso3"), # variables pk del dataset para hacer el join entre bases
  drop_joined_df =  F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente1, fuente2),
    control = comparacion,
    analista = analista,
    pk = c("anio", "iso3"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("relativo_arg" = "PBI per cápita de Argentina relativo al de otros países. El PIB per cápita es a PPA (en u$s a precios internacionales constantes de 2011)"),
    unidades = list("relativo_arg" = "unidades")
  )


output_name <- gsub("\\.csv", "", output_name)

mandar_data(paste0(output_name, ".csv"), subtopico = "CRECIM", branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = "CRECIM",  branch = "dev")

