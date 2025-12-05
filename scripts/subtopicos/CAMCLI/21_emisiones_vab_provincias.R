#################################################################################
##                              Dataset: nombre                               ##
################################################################################

rm(list = ls())

#-- Descripcion ----
#' Breve descripcion de output creado
#'
#'

output_name <- "emisiones_vab_provincias"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

# traigo la data clean para este dataset (3 fuentes)

emisiones_provincias_2010_2018<- read_fuente_clean("R157C67")
# vab_provincias_2004_bis<- read_fuente_clean("R159C68")
pbi_per_capita_1895_2022 <- read_fuente_clean("R160C70")
vab_provincias_2004<- read_csv(get_raw_path("R222C0"))

anio_max  <- 2018 
#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

## proceso emisiones por provincia, filtro 2018 cambio nombres a provincias}
emisiones_provincias_2018 <- emisiones_provincias_2010_2018 %>% 
  mutate(anio = as.numeric(anio)) %>% 
  filter(anio== anio_max) %>% 
  mutate(provincia = ifelse(provincia == "PBA", "Buenos Aires", provincia),
         provincia = ifelse(provincia == "Cordoba", "Córdoba", provincia),
         provincia = ifelse(provincia == "Tucuman", "Tucumán", provincia),
         provincia = ifelse(provincia == "Rio Negro", "Río Negro", provincia),
         provincia = ifelse(provincia == "Neuquen", "Neuquén", provincia),
         provincia = ifelse(provincia == "Entre Rios", "Entre Ríos", provincia),
         provincia = ifelse(provincia == "Santa Fe", "Santa Fé", provincia),
         provincia = ifelse(provincia == "Ciudad Autónoma de Buenos Aires", "CABA",  provincia)) %>% 
  filter(sector == "Total Jurisdiccion")

emisiones_provincias_2018 <- emisiones_provincias_2018 %>% 
  select(-c(sector, categoria))

provs_emisiones <- emisiones_provincias_2018 %>% 
  pull(provincia) %>% unique()

## proceso vab por provincia, filtro 2018 cambio nombres a provincias}
vab_provincias_2018 <- vab_provincias_2004 %>% 
  filter(anio==anio_max) %>% 
  filter(provincia != "No distribuido") %>% 
  mutate(provincia = ifelse(provincia == "PBA", "Buenos Aires", provincia),
                                provincia = ifelse(provincia == "Cordoba", "Córdoba", provincia),
                                provincia = ifelse(provincia == "Tucuman", "Tucumán", provincia),
                                provincia = ifelse(provincia == "Rio Negro", "Río Negro", provincia),
                                provincia = ifelse(provincia == "Neuquen", "Neuquén", provincia),
                                provincia = ifelse(provincia == "Entre Rios", "Entre Ríos", provincia),
                                provincia = ifelse(provincia == "Santa Fe", "Santa Fé", provincia),
                                provincia = ifelse(provincia == "Ciudad Autonoma de Buenos Aires", "Ciudad Autónoma de Buenos Aires", provincia),
                                provincia = ifelse(provincia == "Ciudad de Buenos Aires", "Ciudad Autónoma de Buenos Aires", provincia)) %>% 
  select (1:3)

provs_vab <- vab_provincias_2018 %>% 
  pull(provincia) %>% unique()

## proceso pbi per capita, filtro 2018 cambio nombres a provincias
pbi_per_capita_2018 <- pbi_per_capita_1895_2022 %>% 
  filter(anio==anio_max) %>% 
  mutate(provincia = ifelse(provincia == "PBA", "Buenos Aires", provincia),
         provincia = ifelse(provincia == "Cordoba", "Córdoba", provincia),
         provincia = ifelse(provincia == "Tucuman", "Tucumán", provincia),
         provincia = ifelse(provincia == "Rio Negro", "Río Negro", provincia),
         provincia = ifelse(provincia == "Neuquen", "Neuquén", provincia),
         provincia = ifelse(provincia == "Entre Rios", "Entre Ríos", provincia),
         provincia = ifelse(provincia == "Santa Fe", "Santa Fé", provincia),
         provincia = ifelse(provincia == "Ciudad Autónoma de Buenos Aires", "CABA", provincia)) %>% 
    select (1,3,4)

provs_pib <- pbi_per_capita_2018 %>% 
  pull(provincia) %>% unique()

## traigo la población por provincia de las proyecciones que están en el raw de daniel
poblacion_prov_2018 <- readxl::read_excel(get_raw_path("R160C0"),
                                          sheet = "Población 2004-2022 long") %>% 
  janitor::clean_names() %>% 
  filter(ano==anio_max) %>% mutate(provincia = ifelse(provincia == "PBA", "Buenos Aires", provincia),
                               provincia = ifelse(provincia == "Cordoba", "Córdoba", provincia),
                               provincia = ifelse(provincia == "Tucuman", "Tucumán", provincia),
                               provincia = ifelse(provincia == "Rio Negro", "Río Negro", provincia),
                               provincia = ifelse(provincia == "Neuquen", "Neuquén", provincia),
                               provincia = ifelse(provincia == "Entre Rios", "Entre Ríos", provincia),
                               provincia = ifelse(provincia == "CABA", "Ciudad Autónoma de Buenos Aires", provincia),
                               provincia = ifelse(provincia == "Santa Fe", "Santa Fé", provincia),
                               provincia = ifelse(provincia == "Ciudad Autónoma de Buenos Aires", "CABA", provincia)) %>% 
  rename (anio=ano) %>% 
  mutate(poblacion_millones = round(poblacion / 1000000, 2)) %>% 
  select(2,4)

provs_pob <- poblacion_prov_2018 %>% 
  pull(provincia) %>% unique()

print(provs_emisiones %>% sort())
print(provs_vab %>% sort())
print(provs_pib %>% sort())
print(provs_pob %>% sort())

## junto las tres bases para poder hacer los cálculos
df_output <- pbi_per_capita_2018 %>%
  left_join(vab_provincias_2018, by = "provincia") %>%
  left_join(emisiones_provincias_2018, by = "provincia") %>% 
  left_join(poblacion_prov_2018, by = "provincia") %>% 
  mutate(
         emis_per_cap = round(valor / poblacion_millones, 2)) %>%  
rename(valor_en_mtco2e_per_cap=emis_per_cap,
       vab_precios_basicos_2004_per_cap=pbi_per_capita) 
#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

df_output <- df_output %>% 
  select(-c(anio.y, anio.x)) 

df_output <- df_output %>% 
  mutate(provincia = ifelse(provincia == "Santa Fé", "Santa Fe", provincia)) %>% 
  rename(geonombreFundar = provincia)

df_output <- df_output %>% 
  select(-c(vab_pb, cod_provincia, valor, poblacion_millones))

geo <- get_nomenclador_geografico_front()


df_output <- df_output %>% 
  left_join(geo %>% select(geocodigoFundar = geocodigo, geonombreFundar = name_long) )
  

df_anterior <- descargar_output(nombre=output_name,
                                subtopico = "CAMCLI")


#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(df_output,
                                            df_anterior,
                                            k_control_num = 3,
                                            pk = c("anio","geonombreFundar"),
                                            drop_joined_df = F)


#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso


df_output %>%
  argendataR::write_output(
    output_name = output_name,
    control = list("No son comparables por la fecha de corte"),
    subtopico = "CAMCLI",
    fuentes = c("R157C67", "R222C0", "R160C70"),
    analista = "",
    pk = c("anio", "geocodigoFundar"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    aclaraciones = "el dato de población por provincia para el año 2018 surge de la hoja Población 2004-2022 long del xlsx de Daniel St que es fuente raw R160C70" ,
    #columna_geo_referencia = "iso3",
    nivel_agregacion = "provincia",
    etiquetas_indicadores = list(
      "geonombreFundar" = "nombre geografico",
      "geocodigoFundar" = "codigo geonomenclador",
      "anio" = "Año",
      "valor_en_mtco2e_per_cap" = "Emisiones de dioxido de carbono en toneladas per capita",
      "vab_precios_basicos_2004_per_cap" = "VAB per cápita en millones de pesos a precios de 2004"
    ),
    unidades = list(
      "valor_en_mtco2e_per_cap" = "Millones de toneladas de CO2 equivalente per capita",
      "vab_precios_basicos_2004_per_cap" = "VAB per cápita en millones de pesos a precios de 2004"
    )
  )



mandar_data(paste0(output_name, ".csv"), subtopico = "CAMCLI", branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = "CAMCLI",  branch = "main")



