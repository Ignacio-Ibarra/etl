#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "SEBACO"
output_name <- "19_saldo_comex_x_sector"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R245C133' # INDEC Estadísticas de balanza de pagos, en formato SDMX-ML.

df_bop <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet() %>% 
  mutate(
    anio = as.integer(str_extract(TIME_PERIOD,'[0-9]+'))
  )


anio_max <- df_bop %>% 
  distinct(trimestre = TIME_PERIOD, anio) %>% 
  group_by(anio) %>% 
  dplyr::filter(n() == 4) %>% 
  ungroup() %>% 
  summarise(anio_max = max(anio)) %>% 
  pull(anio_max)



data <- df_bop %>% 
  mutate(sdmx = paste(FREQ,ADJUSTMENT,REF_AREA,COUNTERPART_AREA,REF_SECTOR,COUNTERPART_SECTOR,FLOW_STOCK_ENTRY,ACCOUNTING_ENTRY,INT_ACC_ITEM,FUNCTIONAL_CAT,INSTR_ASSET,MATURITY,UNIT_MEASURE,CURRENCY_DENOM,VALUATION,COMP_METHOD,sep='.'),
         sdmx_desc = case_when(sdmx== 'Q.N.AR.W1.S1.S1.T.B.G._Z._Z._Z.USD._T._X.N' ~ 'Bienes - BC',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.C.G._Z._Z._Z.USD._T._X.N' ~ 'Bienes - Expo',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.B.S._Z._Z._Z.USD._T._X.N' ~ 'Servicios - BC',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.C.S._Z._Z._Z.USD._T._X.N' ~ 'Servicios - Expo',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.C.SH._Z._Z._Z.USD._T._X.N' ~ 'Propiedad intelectual - Expo',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.B.SH._Z._Z._Z.USD._T._X.N' ~ 'Propiedad intelectual - BC',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.C.SI2._Z._Z._Z.USD._T._X.N' ~ 'SSI - Expo',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.B.SI2._Z._Z._Z.USD._T._X.N' ~ 'SSI - BC',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.C.SJ1._Z._Z._Z.USD._T._X.N' ~ 'Investigación y desarrollo - Expo',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.B.SJ1._Z._Z._Z.USD._T._X.N' ~ 'Investigación y desarrollo - BC',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.B.SJ2._Z._Z._Z.USD._T._X.N' ~ 'Servicios profesionales - BC',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.C.SJ2._Z._Z._Z.USD._T._X.N' ~ 'Servicios profesionales - Expo',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.C.SJ3._Z._Z._Z.USD._T._X.N' ~ 'Ss. arquitectura, ingeniería y otros - Expo',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.B.SJ3._Z._Z._Z.USD._T._X.N' ~ 'Ss. arquitectura, ingeniería y otros - BC',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.C.SK1._Z._Z._Z.USD._T._X.N' ~ 'Ss. audiovisuales - Expo',
                               sdmx == 'Q.N.AR.W1.S1.S1.T.B.SK1._Z._Z._Z.USD._T._X.N' ~ 'Ss. audiovisuales - BC',
                               TRUE ~ 'Otras')
         
  ) %>% 
  dplyr::filter(anio <= anio_max ) %>% 
  group_by(sdmx, sdmx_desc, anio) %>% 
  summarize(OBS_VALUE = sum(OBS_VALUE)) %>% 
  ungroup() %>% 
  dplyr::filter(sdmx_desc != 'Otras') %>% 
  mutate(tipo_dato = if_else(str_detect(sdmx_desc,'Expo$'),'expo','balanza'),
         descripcion = str_remove(sdmx_desc,' - .*')) %>% 
  select(-sdmx_desc, -sdmx) %>% 
  distinct() %>% 
  pivot_wider(names_from=tipo_dato,values_from=OBS_VALUE) %>% 
  mutate(impo = expo - balanza)


# # Separar bienes y servicios 
# data_bienes <- data %>% 
#   filter(descripcion %in% c('Bienes'))
# 
# data_servicios <- data %>% 
#   filter(descripcion %in% c('Servicios'))

df_output <- data %>% 
  filter(! descripcion %in% c('Bienes','Servicios')) %>% 
  rename(sector = descripcion) %>% 
  select(anio, sector, balanza)



df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  mutate(anio = as.integer(anio))


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c('anio','sector'),
  drop_joined_df =  F
)


#-- Exportar Output ----

armador_descripcion <- function(metadatos, etiquetas_nuevas = data.frame(), output_cols){
  # metadatos: data.frame sus columnas son variable_nombre y descripcion y 
  # proviene de la info declarada por el analista 
  # etiquetas_nuevas: data.frame, tiene que ser una dataframe con la columna 
  # variable_nombre y la descripcion
  # output_cols: vector, tiene las columnas del dataset que se quiere escribir
  
  etiquetas <- metadatos %>% 
    dplyr::filter(variable_nombre %in% output_cols) 
  
  
  etiquetas <- etiquetas %>% 
    bind_rows(etiquetas_nuevas)
  
  
  diff <- setdiff(output_cols, etiquetas$variable_nombre)
  
  stopifnot(`Error: algunas columnas de tu output no fueron descriptas` = length(diff) == 0)
  
  # En caso de que haya alguna variable que le haya cambiado la descripcion pero que
  # ya existia se va a quedar con la descripcion nueva. 
  
  etiquetas <- etiquetas %>% 
    group_by(variable_nombre) %>% 
    filter(if(n() == 1) row_number() == 1 else row_number() == n()) %>%
    ungroup()
  
  etiquetas <- stats::setNames(as.list(etiquetas$descripcion), etiquetas$variable_nombre)
  
  return(etiquetas)
  
}

# Tomo las variables output_name y subtopico declaradas arriba
metadatos <- argendataR::metadata(subtopico = subtopico) %>% 
  dplyr::filter(grepl(paste0("^", output_name,".csv"), dataset_archivo)) %>% 
  distinct(variable_nombre, descripcion) 



# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output


etiquetas_nuevas <- data.frame(
  variable_nombre = c("sector", 
                      "balanza"),
  descripcion = c("Rama de actividad perteneciente a 'Sectores basados en conocimiento' (SBC)",
                  "Valor de la balanza comercial en millones de dólares corrientes")
)


descripcion <- armador_descripcion(metadatos = metadatos,
                                   etiquetas_nuevas = etiquetas_nuevas,
                                   output_cols = output_cols)

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
    fuentes = colectar_fuentes(),
    subtopico = subtopico,
    analista = analista,
    pk = c('anio','sector'),
    es_serie_tiempo = T,
    control = comparacion, 
    descripcion_columnas = descripcion,
    unidades = list("balanza" = "millones de dólares corrientes")
  )
