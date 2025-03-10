################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "ESTPRO"
output_name <- "densidad_nbi"
analista <- "Gisella Pascuariello"

fuente1 <- "R243C113"
fuente2 <- "R107C0"
fuente3 <- "R248C0"
fuente4 <- "R244C0"
fuente5 <- "R249C118"

df_pob_censo <- arrow::read_parquet(argendataR::get_clean_path(fuente1)) %>% 
  mutate(depto_id = str_pad(depto_id, width = 5, side = 'left', pad ="0")) 


df_establecimientos <- read_csv(argendataR::get_raw_path(fuente2)) %>% 
  mutate(depto_id = str_pad(in_departamentos, width = 5, side = 'left', pad ="0"),
         provincia_id = str_pad(provincia_id, width = 2, side = "left", pad = "0")) %>% 
  dplyr::filter(anio == 2022) %>% 
  group_by(anio, depto_id, provincia_id) %>% 
  summarise(
    establecimientos = n(),
  ) %>% 
  ungroup() %>% 
  mutate(
    depto_id = case_when(
      depto_id == "94007" ~ "94008", # Cambio codigo de Rio Grande en Censo 2022, CEP lo tiene mal
      depto_id == "94014" ~ "94015", # Cambió codigo de Ushuaia en Censo 2022, CEP lo tiene mal
      depto_id == "06217" ~ "06218", # Cambió codigo de Chascomus en Censo 2022, CEP lo tiene mal
      TRUE ~ depto_id
    )
  )


diccionario_depto <- argendataR::get_raw_path(fuente3) %>% 
  read.csv(., sep = ";") %>% 
  mutate(depto_id = str_pad(IN1, width = 5, side = 'left', pad ="0")) %>% 
  select(depto_id, depto_nombre = NAM) %>% 
  filter(!((depto_id == "50035") & (depto_nombre == "San Martín")))



provincia_desc <- argendataR::get_raw_path(fuente4) %>% 
  read.csv(., sep = ",") %>% 
  distinct(provincia_id = cod_pcia, provincia = nom_pcia) %>% 
  mutate(provincia_id = str_pad(provincia_id, width = 2, side = "left", pad = "0"))  

df_densidad <- df_pob_censo %>%
  left_join(diccionario_depto, join_by(depto_id)) %>%
  left_join(df_establecimientos, join_by(depto_id)) %>% 
  mutate(densidad_emp = 1000*establecimientos / poblacion) %>% 
  left_join(provincia_desc, join_by(provincia_id)) %>% 
  select(anio, id_depto = depto_id, departamento = depto_nombre, provincia_id, provincia, densidad_emp) 


df_nbi <- arrow::read_parquet(get_clean_path(fuente5)) %>% 
  select(id_depto, porcentaje_hogares_con_nbi) 

df_output <- df_densidad %>% left_join(df_nbi, join_by(id_depto))

df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico)%>% 
  rename(porcentaje_hogares_con_nbi = share_pb_nbi)

codigos <- df_anterior %>% distinct(provincia_id, region)


df_output <- df_output %>% left_join(codigos, join_by(provincia_id))

comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  pk = c("anio","id_depto"), # variables pk del dataset para hacer el join entre bases
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
  dplyr::filter(grepl(paste0(output_name,".csv"), dataset_archivo)) %>% 
  distinct(variable_nombre, descripcion) 


# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output


etiquetas_nuevas <- data.frame(
  variable_nombre = c("porcentaje_hogares_con_nbi"),
  descripcion = c("Porcentaje de hogares con necesidad básicas insatisfechas (NBI)")
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
# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = colectar_fuentes(),
    analista = analista,
    control = comparacion, 
    pk = c("id_depto"),
    es_serie_tiempo = F,
    descripcion_columnas = descripcion,
    unidades = list("densidad_emp" = "unidades", "porcentaje_hogares_con_nbi" = "share_pb_nbi"),
    aclaraciones = "Los datos de NBI actuales provienen del Censo 2022 y fueron extraídos mediante consulta manual en REDATAM, luego fueron limpiados y se corroboró el valor obtenido por depto luego de la limpieza. Por otro lado, el dato de densidad empresarial fue corregido porque el dato anterior se habia construido con la fuente R240C0 que en el origen poseía datos erróneos. Eso fue subsanado con el uso de la fuente R107C0 que corresponde a la misma institución y no tiene errores" 
  )

argendataR::mandar_data("densidad_nbi.csv", subtopico = subtopico, branch ="dev")
argendataR::mandar_data("densidad_nbi.json", subtopico = subtopico, branch ="dev")
