################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "INFDES"
output_name <- "tasa_desempleo_sexo_paises_modelada"
fuente1 <- "R109C0" # SL.UEM.TOTL.ZS
fuente2 <- "R110C0" # SL.UEM.TOTL.FE.ZS
fuente3 <- "R111C0" # SL.UEM.TOTL.MA.ZS

#-- Librerias ----


#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
desempleo_total.df <- readr::read_csv(argendataR::get_temp_path(fuente1)) %>% 
  select(iso3 = iso3c, anio = year,  Total = SL.UEM.TOTL.ZS) %>% 
  mutate(Total = Total/100) %>% 
  filter(anio>1990) %>% 
  filter(!is.na(iso3))

desempleo_fem.df <- readr::read_csv(argendataR::get_temp_path(fuente2)) %>% 
  select(iso3 = iso3c, anio = year,  Mujeres = SL.UEM.TOTL.FE.ZS) %>% 
  mutate(Mujeres = Mujeres/100) %>% 
  filter(anio>1990) %>% 
  filter(!is.na(iso3))

desempleo_masc.df <- readr::read_csv(argendataR::get_temp_path(fuente3)) %>% 
  select(iso3 = iso3c, anio = year,  Varones = SL.UEM.TOTL.MA.ZS) %>% 
  mutate(Varones = Varones/100) %>% 
  filter(anio>1990) %>% 
  filter(!is.na(iso3))

geonomenclador <- argendataR::get_nomenclador_geografico()

df_output <- left_join(desempleo_total.df, desempleo_fem.df, by=join_by(anio, iso3)) %>% left_join(., desempleo_masc.df, by=join_by(anio, iso3)) %>% 
  pivot_longer(!all_of(c('anio','iso3')), names_to = "sexo", values_to = "tasa_desempleo") %>% 
  left_join(geonomenclador %>% select(iso3 = codigo_fundar, pais_desc = desc_fundar, continente_fundar), by = join_by(iso3)) %>% 
  filter(!is.na(continente_fundar))
  


#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  subtopico = subtopico,
  nombre = output_name,
  pk = c("anio", "iso3", "sexo"),
  drop_joined_df = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente1, fuente2, fuente3),
    analista = "",
    pk = c("anio", "iso3", "sexo"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("tasa_desempleo" = "Desempleo como proporción de la fuerza laboral"),
    unidades = list("tasa_desempleo" = "unidades")
  )

