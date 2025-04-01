#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "SEBACO"
output_name <- "22_participacion_expo_sbc"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R317C186' # WTO Exportaciones servicios por país, subproducto, anio.

df_wto <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)


codigos_sbc_bp06 <-  c('SH','SI2','SJ1','SJ2','SJ3','SK1')


df_sbc <- df_wto %>% 
  dplyr::filter(product_code %in% codigos_sbc_bp06) 

df_output <- df_sbc %>% 
  group_by( anio = year, iso3, pais_nombre = country_name_es) %>% 
  summarise(
    value = sum(value, na.rm = T)
  ) %>% 
  ungroup() %>% 
  group_by(anio) %>% 
  mutate(
    prop_expo = value / sum(value)
  ) %>% 
  ungroup() %>% 
  select(iso3, pais_nombre, anio, prop_expo)



df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") 


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c('anio','iso3'),
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
  variable_nombre = c("pais_nombre", 
                      "prop_expo"),
  descripcion = c("Nombre de país",
                  "Participación del país en las exportaciones globales de los 'Servicios basados en conocimiento (SBC)', en proporción")
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

data_comparada <- glue::glue(
"
anio expo_mundial.x	expo_mundial.y	var 100.(x - y) / y
2005 Total			598669.2076	609641	1.83%
2006 Total			721780.1882	734291	1.73%
2007 Total			862633.6696	876113	1.56%
2008 Total			1010365.621	1039022	2.84%
2009 Total			977197.4515	1007515	3.10%
2010 Total			1121760.954	1151435	2.65%
2011 Total			1271814.516	1306929	2.76%
2012 Total			1379880.323	1403763	1.73%
2013 Total			1462734.95	1490173	1.88%
2014 Total			1774603.336	1780254	0.32%
2015 Total			1744886.634	1749027	0.24%
2016 Total			1822148.733	1826312	0.23%
2017 Total			1988124.776	1998065	0.50%
2018 Total			2225777.405	2233503	0.35%
2019 Total			2381467.535	2388122	0.28%
2020 Total			2339012.001	2349546	0.45%
2021 Total			2657371.75	2758115	3.79%
2022 Total			2726748.151	2899547	6.34%
"
)

aclaracion <- glue::glue("En la nueva versión los datos agregados de exportaciones mundiales del sector SBC varían considerablemente respecto a la versión anterior. Eso genera variaciones en la variable prop_expo, aunque la diferencia mayor es del 5%, vern en la tabla a continuación: \n{data_comparada}")


df_output %>%
  argendataR::write_output(
    output_name = output_name,
    fuentes = colectar_fuentes(),
    subtopico = subtopico,
    analista = analista,
    pk = c('anio','iso3'),
    es_serie_tiempo = T,
    columna_geo_referencia = 'iso3',
    control = comparacion, 
    descripcion_columnas = descripcion,
    aclaraciones = aclaracion,
    unidades = list('prop_expo' = 'proporción')
  )



