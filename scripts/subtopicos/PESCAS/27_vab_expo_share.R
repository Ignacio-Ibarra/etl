#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "PESCAS"
output_name <- "27_vab_expo_share.csv"
analista <- "Ignacio Ibarra"
fuente1 <- 'R223C94' # INDEC VABpb corrientes por sector.
fuente2 <- 'R245C133' # INDEC BOP
fuente3 <- 'R338C211' # INDEC Complejos Exportadores. Revisión 2018. 2021 a ultimo anio


df_vabpb <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)


df_bop <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet(.)


df_complejos <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet(.)

df_vabpb_pesca <- df_vabpb %>% 
  dplyr::filter(trimestre == "Total", sub_sector == "Total sector", anio == max(anio)) %>% 
  mutate(participacion = vab_pb / sum(vab_pb),
         indicador = "PIB") %>% 
  dplyr::filter(sector == "Pesca") %>% 
  select(anio, sector, indicador, participacion)


df_expo_totales_ultimo_anio <- df_bop %>% 
  mutate(sdmx_id = paste(FREQ,
                         ADJUSTMENT,
                         REF_AREA,
                         COUNTERPART_AREA,
                         REF_SECTOR,
                         COUNTERPART_SECTOR,
                         FLOW_STOCK_ENTRY,
                         ACCOUNTING_ENTRY,
                         INT_ACC_ITEM,
                         FUNCTIONAL_CAT,
                         INSTR_ASSET,
                         MATURITY,
                         UNIT_MEASURE,
                         CURRENCY_DENOM,
                         VALUATION,
                         COMP_METHOD,
                         sep = "."
                         ),
         anio = as.integer(str_extract(TIME_PERIOD, "(\\d{4})\\-.*", group = 1)) ) %>% 
  dplyr::filter(sdmx_id == "Q.N.AR.W1.S1.S1.T.C.GS._Z._Z._Z.USD._T._X.N", anio == max(anio)) %>% 
  summarise(expo_total = sum(OBS_VALUE))


df_expo_pesca <- df_complejos %>% 
  dplyr::filter(complejos == "Complejo pesquero", anio == max(anio)) %>% 
  mutate(sector = "Pesca",
         indicador = "Exportaciones",
         participacion = expo /df_expo_totales_ultimo_anio$expo_total) %>% 
  select(anio, sector, indicador, participacion)


df_output <- bind_rows(df_vabpb_pesca, df_expo_pesca)
  

df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  mutate(anio = as.integer(anio))


pk <- c("indicador")

comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = pk, # variables pk del dataset para hacer el join entre bases
  drop_joined_df =  F
)

colectar_fuentes <- function(pattern = "^fuente.*"){
  
  # Genero un vector de codigos posibles
  posibles_codigos <- c(fuentes_raw()$codigo,fuentes_clean()$codigo)
  
  # Usar ls() para buscar variables en el entorno global
  variable_names <- ls(pattern = pattern, envir = globalenv())
  
  # Obtener los valores de esas variables
  valores <- unlist(mget(variable_names, envir = globalenv()))
  
  # Filtrar aquellas variables que sean de tipo character (string)
  # Esto es para que la comparacion sea posible en la linea de abajo
  strings <- valores[sapply(valores, is.character)]
  
  # solo devuelvo las fuentes que existen
  return(valores[valores %in% posibles_codigos])
}



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = colectar_fuentes(),
    analista = analista,
    pk =  pk,
    es_serie_tiempo = T,
    control = comparacion,
    columna_indice_tiempo = NULL,
    columna_geo_referencia = NULL,
    nivel_agregacion = NULL,
  )