################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection
require(WDI)
library(sjlabelled)

subtopico <- "CRECIM"
output_name <- "pib_vs_expectativa"
analista = "Pablo Sonzogni"
fuente1 <- ""

url <- "https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.KD"
indicator_code <- str_split_1(url, "/") %>% tail(.,1)


# Descargo data usando wrapper https://github.com/vincentarelbundock/WDI
data_pibpc_ppp <- WDI(indicator=indicator_code, country = 'all') %>% 
  select(iso3 = iso3c, anio = year, pib_pc_ppp=`NY.GDP.PCAP.PP.KD`) %>% 
  dplyr::filter(iso3!="") %>% 
  dplyr::filter(!is.na(pib_pc_ppp)) %>% 
  dplyr::filter(!is.na(iso3)) %>% 
  sjlabelled::zap_labels() 

library(httr)

url_le <- "https://population.un.org/wpp/Download/Files/1_Indicator%20(Standard)/EXCEL_FILES/4_Mortality/WPP2024_MORT_F05_1_LIFE_EXPECTANCY_BY_AGE_BOTH_SEXES.xlsx"

# Desactivo la verificacion de SSL
options(download.file.method="libcurl"
        # , download.file.extra="-k -L --ssl-allow-unsafe-legacy-renegotiation"
        )


destfile <- glue::glue("{tempdir()}/WPP2024_MORT_F05_1_LIFE_EXPECTANCY_BY_AGE_BOTH_SEXES.xlsx")

download.file(url_le, destfile = destfile, mode = "wb")

httr::GET(url_le, write_disk(destfile, overwrite = TRUE), config(ssl_verifypeer = FALSE))

data_life_exp <- readxl::read_excel(destfile)


#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

df_output <- proceso

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  pk = c("var1", "var2"), # variables pk del dataset para hacer el join entre bases
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

