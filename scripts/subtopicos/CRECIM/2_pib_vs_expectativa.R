################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

library(httr)
library(dplyr)
library(tidyr)
require(WDI)
library(sjlabelled)
library(stringr)



get_raw_path <- function(codigo){
  prefix <- "/srv/shiny-server/static/etl-fuentes2/raw/"
  df_fuentes_raw <- fuentes_raw() 
  path_raw <- df_fuentes_raw[df_fuentes_raw$codigo == codigo,c("path_raw")]
  return(paste0(prefix, path_raw))
}



subtopico <- "CRECIM"
output_name <- "pib_vs_expectativa"
analista = "Pablo Sonzogni"
fuente1 <- "R126C0"


# Cargo data desde server
data_pibpc_ppp <- read_csv(get_raw_path(fuente1)) %>% 
  select(iso3 = iso3c, anio = year, pib_pc =`NY.GDP.PCAP.PP.KD`) %>% 
  dplyr::filter(iso3!="") %>% 
  dplyr::filter(!is.na(pib_pc)) %>% 
  dplyr::filter(!is.na(iso3)) %>% 
  sjlabelled::zap_labels() 

url_le <- "https://population.un.org/wpp/Download/Files/1_Indicator%20(Standard)/EXCEL_FILES/4_Mortality/WPP2024_MORT_F05_1_LIFE_EXPECTANCY_BY_AGE_BOTH_SEXES.xlsx"

# Desactivo la verificacion de SSL
options(download.file.method="libcurl"
        # , download.file.extra="-k -L --ssl-allow-unsafe-legacy-renegotiation"
)


destfile <- glue::glue("{tempdir()}/WPP2024_MORT_F05_1_LIFE_EXPECTANCY_BY_AGE_BOTH_SEXES.xlsx")

download.file(url_le, destfile = destfile, mode = "wb")

data_raw <- readxl::read_excel(destfile)

cols <- data_raw[12,] %>% as.matrix()

data_raw <- data_raw[13:nrow(data_raw),]

names(data_raw) <- cols

data_clean <- data_raw %>% rename(iso3 = `ISO3 Alpha-code`, anio = Year) %>% 
  select(all_of(c('iso3','anio',0:99,"100+"))) %>% 
  dplyr::filter(!is.na(iso3)) %>% 
  mutate(anio = as.integer(anio)) %>% 
  pivot_longer(-one_of(c("iso3","anio")),
               names_to = "edad",
               values_to = "expectativa_vida")


df_output <- data_pibpc_ppp %>% 
  left_join(data_clean %>% 
              dplyr::filter(edad == "0") %>% 
              select(-edad), by=join_by(iso3, anio)) %>% 
  dplyr::filter(!is.na(expectativa_vida)) %>% 
  dplyr::filter(!is.na(pib_pc_ppp)) %>% 
  rename(expectativa_al_nacer = expectativa_vida)

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega") 


comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = c("iso3", "anio"), 
  drop_joined_df = F
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

