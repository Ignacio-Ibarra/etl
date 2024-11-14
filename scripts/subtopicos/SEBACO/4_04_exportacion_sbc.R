################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "SEBACO"
output_name <- "04_exportacion_sbc"
analista = "Nicolas Sidicaro"
fuente1 <- "R245C133"

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R"
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

#readr::read_csv(argendataR::get_temp_path("RXXCX"))

get_raw_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}raw/")
  df_fuentes_raw <- fuentes_raw() 
  path_raw <- df_fuentes_raw[df_fuentes_raw$codigo == codigo,c("path_raw")]
  return(paste0(prefix, path_raw))
}

## traigo la data
data <- arrow::read_parquet(get_clean_path(fuente1))

#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

data <- data %>% 
  mutate(sdmx = paste(FREQ,ADJUSTMENT,REF_AREA,COUNTERPART_AREA,REF_SECTOR,COUNTERPART_SECTOR,FLOW_STOCK_ENTRY,ACCOUNTING_ENTRY,INT_ACC_ITEM,FUNCTIONAL_CAT,INSTR_ASSET,MATURITY,UNIT_MEASURE,CURRENCY_DENOM,VALUATION,COMP_METHOD,sep='.'))

data <- data %>%
  mutate(anio = str_extract(TIME_PERIOD,'[0-9]+'),
         OBS_VALUE = as.double(OBS_VALUE)) %>% 
  group_by(sdmx,anio) %>% 
  summarize(OBS_VALUE = sum(OBS_VALUE))

# Filtrar 2023
data <- data %>% 
  filter(anio != '2023')

# Filtrar por datos 
data <- data %>% 
  mutate(sdmx_desc = case_when(sdmx== 'Q.N.AR.W1.S1.S1.T.B.G._Z._Z._Z.USD._T._X.N' ~ 'Bienes - BC',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.C.G._Z._Z._Z.USD._T._X.N' ~ 'Bienes - Expo',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.B.S._Z._Z._Z.USD._T._X.N' ~ 'Servicios - BC',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.C.S._Z._Z._Z.USD._T._X.N' ~ 'Servicios - Expo',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.C.SH._Z._Z._Z.USD._T._X.N' ~ 'Propiedad intelectual - Expo',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.B.SH._Z._Z._Z.USD._T._X.N' ~ 'Propiedad intelectual - BC',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.C.SI2._Z._Z._Z.USD._T._X.N' ~ 'SSI - Expo',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.B.SI2._Z._Z._Z.USD._T._X.N' ~ 'SSI - BC',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.C.SJ1._Z._Z._Z.USD._T._X.N' ~ 'I+D - Expo',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.B.SJ1._Z._Z._Z.USD._T._X.N' ~ 'I+D - BC',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.B.SJ2._Z._Z._Z.USD._T._X.N' ~ 'Servicios profesionales - BC',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.C.SJ2._Z._Z._Z.USD._T._X.N' ~ 'Servicios profesionales - Expo',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.C.SJ3._Z._Z._Z.USD._T._X.N' ~ 'Ss. arquitectura, ingeniería y otros - Expo',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.B.SJ3._Z._Z._Z.USD._T._X.N' ~ 'Ss. arquitectura, ingeniería y otros - BC',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.C.SK1._Z._Z._Z.USD._T._X.N' ~ 'Ss. audiovisuales - Expo',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.B.SK1._Z._Z._Z.USD._T._X.N' ~ 'Ss. audiovisuales - BC',
                               TRUE ~ 'Otras'))
data <- data %>% 
  filter(sdmx_desc != 'Otras')


# Separar Balanza comercial de expo
data <- data %>% 
  mutate(tipo_dato = if_else(str_detect(sdmx_desc,'Expo$'),'Exportaciones','Balanza'),
         descripcion = str_remove(sdmx_desc,' - .*'))
data <- data %>% 
  ungroup() %>% 
  select(-c(sdmx_desc,sdmx)) %>% 
  distinct() %>% 
  pivot_wider(names_from=tipo_dato,values_from=OBS_VALUE)

data <- data %>% 
  mutate(importaciones = Exportaciones - Balanza)
data <- janitor::clean_names(data)

# Separar bienes y servicios 
data_aux1 <- data %>% 
  filter(descripcion %in% c('Bienes'))
data_aux2 <- data %>% 
  filter(descripcion %in% c('Servicios'))
data <- data %>% 
  filter(! descripcion %in% c('Bienes','Servicios'))

# Armar total SBC
data_aux <- data %>% 
  group_by(anio) %>% 
  summarize(balanza_sbc = sum(balanza),
            exportaciones_sbc = sum(exportaciones),
            importaciones_sbc = sum(importaciones)) %>% 
  ungroup()
data <- data %>% 
  left_join(data_aux)

# Agregar bienes y servicios
data_aux1 <- data_aux1 %>% 
  rename(balanza_bienes=balanza,exportaciones_bienes=exportaciones,importaciones_bienes=importaciones) %>% 
  select(-c(descripcion))
data_aux2 <- data_aux2 %>% 
  rename(balanza_servicios=balanza,exportaciones_servicios=exportaciones,importaciones_servicios=importaciones) %>% 
  select(-c(descripcion))

data <- data %>% 
  left_join(data_aux1)
data <- data %>% 
  left_join(data_aux2)

# Calcular total y share 
data <- data %>% 
  ungroup() %>% 
  mutate(balanza_total = balanza_bienes + balanza_servicios,
         exportaciones_total = exportaciones_bienes + exportaciones_servicios,
         importaciones_total = importaciones_bienes + importaciones_servicios) %>% 
  mutate(prop_expo_sbc_servicios = exportaciones_sbc / exportaciones_servicios,
         prop_impo_sbc_servicios = importaciones_sbc / importaciones_servicios,
         prop_expo_sbc_total = exportaciones_sbc / exportaciones_total,
         prop_impo_sbc_total = importaciones_sbc / importaciones_total)

# Calcular proporcion de cada SBC sobre servicios y SBC 
data <- data %>% 
  mutate(prop_sobre_sbc_expo = exportaciones / exportaciones_sbc,
         prop_sobre_sbc_impo = importaciones / importaciones_sbc,
         prop_sobre_serv_expo = exportaciones / exportaciones_servicios,
         prop_sobre_serv_impo = importaciones / importaciones_servicios)

# Armar solo share de SBC abierto
df_clean <- data %>% 
  select(anio,descripcion,prop_sobre_sbc_expo) %>% 
  pivot_wider(names_from=descripcion,values_from=prop_sobre_sbc_expo)

# Seleccionar datos y dejar long 
df_clean <- data %>% 
  select(anio,exportaciones_sbc, prop_expo_sbc_servicios, prop_expo_sbc_total) %>% 
  distinct()

df_output <- df_clean %>%
  mutate(anio = as.numeric(anio))

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

## data anterior

df_anterior <- argendataR::descargar_output(nombre =output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")

comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  pk = c("anio"), # variables pk del dataset para hacer el join entre bases
  drop_joined_df =  F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    control = comparacion,
    subtopico = subtopico,
    fuentes = c("R245C133"),
    analista = analista,
    pk = c("anio"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    #columna_geo_referencia = "",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("exportaciones_sbc"="Nivel de exportaciones en millones de dólares del complejo de SBC",
                                 "prop_expo_sbc_servicios"="Proporción de las exportaciones de SBC sobre las exportaciones totales de servicios",
                                 "prop_expo_sbc_total"="Proporción de las exportaciones de SBC sobre las exportaciones totales"),
    unidades = list("exportaciones_sbc" = "millones","prop_expo_sbc_servicios"="porcentaje","prop_expo_sbc_total"="porcentaje")
  )

#temp_dir <- tempdir()
#temp_files <- list.files(temp_dir, full.names = TRUE)
#view(temp_files)
#list.files(tempdir())

