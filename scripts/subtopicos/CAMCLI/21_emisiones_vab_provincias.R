#################################################################################
##                              Dataset: nombre                               ##
################################################################################

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

emisiones_provincias_2010_2018<- readr::read_csv(argendataR::get_temp_path("R157C67"))
vab_provincias_2004<- readr::read_csv(argendataR::get_temp_path("R159C68"))
pbi_per_capita_1895_2022 <- readr::read_csv(argendataR::get_temp_path("R160C70"))

#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

## proceso emisiones por provincia, filtro 2018 cambio nombres a provincias}
emisiones_provincias_2018 <- emisiones_provincias_2010_2018 %>% 
  filter(anio==2018) %>% 
  group_by(provincia) %>% 
  mutate(provincia = ifelse(provincia == "PBA", "Buenos Aires", provincia),
         provincia = ifelse(provincia == "Cordoba", "Córdoba", provincia),
         provincia = ifelse(provincia == "Tucuman", "Tucumán", provincia),
         provincia = ifelse(provincia == "Rio Negro", "Río Negro", provincia),
         provincia = ifelse(provincia == "Neuquen", "Neuquén", provincia),
         provincia = ifelse(provincia == "Entre Rios", "Entre Ríos", provincia),
         provincia = ifelse(provincia == "Santa Fe", "Santa Fé", provincia),
         provincia = ifelse(provincia == "Ciudad Autonoma de Buenos Aires", "Ciudad Autónoma de Buenos Aires", provincia)) %>% 
  summarize(total_valor = sum(valor_en_mtco2e)) 

## proceso vab por provincia, filtro 2018 cambio nombres a provincias}
vab_provincias_2018 <- vab_provincias_2004 %>% 
  filter(anio==2018) %>% 
  mutate(provincia = ifelse(provincia == "PBA", "Buenos Aires", provincia),
                                provincia = ifelse(provincia == "Cordoba", "Córdoba", provincia),
                                provincia = ifelse(provincia == "Tucuman", "Tucumán", provincia),
                                provincia = ifelse(provincia == "Rio Negro", "Río Negro", provincia),
                                provincia = ifelse(provincia == "Neuquen", "Neuquén", provincia),
                                provincia = ifelse(provincia == "Entre Rios", "Entre Ríos", provincia),
                                provincia = ifelse(provincia == "Santa Fe", "Santa Fé", provincia),
                                provincia = ifelse(provincia == "Ciudad Autonoma de Buenos Aires", "Ciudad Autónoma de Buenos Aires", provincia),
                                provincia = ifelse(provincia == "Ciudad de Buenos Aires", "Ciudad Autónoma de Buenos Aires", provincia)) %>% 
  select (2,3)

## proceso pbi per capita, filtro 2018 cambio nombres a provincias
pbi_per_capita_2018 <- pbi_per_capita_1895_2022 %>% 
  filter(anio==2018) %>% 
  mutate(provincia = ifelse(provincia == "PBA", "Buenos Aires", provincia),
         provincia = ifelse(provincia == "Cordoba", "Córdoba", provincia),
         provincia = ifelse(provincia == "Tucuman", "Tucumán", provincia),
         provincia = ifelse(provincia == "Rio Negro", "Río Negro", provincia),
         provincia = ifelse(provincia == "Neuquen", "Neuquén", provincia),
         provincia = ifelse(provincia == "Entre Rios", "Entre Ríos", provincia),
         provincia = ifelse(provincia == "Santa Fe", "Santa Fé", provincia),
         provincia = ifelse(provincia == "Ciudad Autonoma de Buenos Aires", "Ciudad Autónoma de Buenos Aires", provincia),
         provincia = ifelse(provincia == "Ciudad de Buenos Aires", "Ciudad Autónoma de Buenos Aires", provincia)) %>% 
  select (1,4)

## traigo la población por provincia de las proyecciones que están en el raw de daniel
poblacion_prov_2018 <- readxl::read_excel(glue::glue("{tempdir()}/pbg por provincia_R160C0.xlsx"), sheet = "Población 2004-2022 long") %>% 
  janitor::clean_names() %>% 
  filter(ano==2018) %>% mutate(provincia = ifelse(provincia == "PBA", "Buenos Aires", provincia),
                               provincia = ifelse(provincia == "Cordoba", "Córdoba", provincia),
                               provincia = ifelse(provincia == "Tucuman", "Tucumán", provincia),
                               provincia = ifelse(provincia == "Rio Negro", "Río Negro", provincia),
                               provincia = ifelse(provincia == "Neuquen", "Neuquén", provincia),
                               provincia = ifelse(provincia == "Entre Rios", "Entre Ríos", provincia),
                               provincia = ifelse(provincia == "CABA", "Ciudad Autónoma de Buenos Aires", provincia),
                               provincia = ifelse(provincia == "Santa Fe", "Santa Fé", provincia),
                               provincia = ifelse(provincia == "Ciudad Autonoma de Buenos Aires", "Ciudad Autónoma de Buenos Aires", provincia),
                               provincia = ifelse(provincia == "Ciudad de Buenos Aires", "Ciudad Autónoma de Buenos Aires", provincia)) %>% 
  rename (anio=ano) %>% 
  mutate(poblacion_millones = round(poblacion / 1000000, 2)) %>% 
  select(2,4)

## junto las tres bases para poder hacer los cálculos
df_output <- pbi_per_capita_2018 %>%
  full_join(vab_provincias_2018, by = "provincia") %>%
  full_join(emisiones_provincias_2018, by = "provincia") %>% 
  full_join(poblacion_prov_2018, by = "provincia") %>% 
  mutate(anio=2018,
         emis_per_cap = round(total_valor / poblacion_millones, 2)) %>%  
rename(valor_en_mtco2e_per_cap=emis_per_cap,
       vab_precios_basicos_2004_per_cap=pbi_per_capita) %>% 
select (6,1,7,2)

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso



df_anterior <- descargar_output(nombre=output_name,
                                subtopico = "CAMCLI",
                                entrega_subtopico = "datasets_segunda_entrega") %>% 
  mutate(provincia = df$provincia)

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(df_output,
                                            df_anterior,
                                            k_control_num = 3,
                                            pk = c("anio","provincia"),
                                            drop_joined_df = F)


#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    control = comparacion,
    subtopico = "CAMCLI",
    fuentes = c("R157C67","R159C68","R160C70"),
    analista = "",
    control = comparacion,
    pk = c("anio","provincia"),
    es_serie_tiempo = F,
    columna_indice_tiempo = "anio",
    aclaraciones = "el dato de población por provincia para el año 2018 surge de la hoja Población 2004-2022 long del xlsx de Daniel St que es fuente raw R160C70" ,
    #columna_geo_referencia = "iso3",
    nivel_agregacion = "provincia",
    etiquetas_indicadores = list("anio" = "Año","valor_en_mtco2e_per_cap"="Emisiones de dioxido de carbono en toneladas per capita","vab_precios_basicos_2004_per_cap"="VAB per cápita en millones de pesos a precios de 2004"),
    unidades = list("valor_en_mtco2e_per_cap" = "Millones de toneladas de CO2 equivalente per capita","vab_precios_basicos_2004_per_cap"="VAB per cápita en millones de pesos a precios de 2004")
  )

