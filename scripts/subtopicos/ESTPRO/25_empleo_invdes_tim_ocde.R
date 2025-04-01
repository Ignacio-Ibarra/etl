################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "ESTPRO"
output_name <- "empleo_invdes_tim_ocde"
analista = "Gisella Pascuariello"

fuente1 <- "R232C103"
fuente3 <- "R234C0"
fuente4 <- "R227C0"



get_clean_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}clean/")
  df_fuentes_clean <- fuentes_clean() 
  path_clean <- df_fuentes_clean[df_fuentes_clean$codigo == codigo,c("path_clean")]
  return(paste0(prefix, path_clean))
}


get_raw_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}raw/")
  df_fuentes_raw <- fuentes_raw() 
  path_raw <- df_fuentes_raw[df_fuentes_raw$codigo == codigo,c("path_raw")]
  return(paste0(prefix, path_raw))
}


# Traducción al castellano de Gise Pascuariello 
descriptores <- haven::read_dta(get_raw_path(fuente3)) 


# Clasificacion intensidad de Daniel Schteingart
clasif_intens <- readxl::read_excel(get_raw_path(fuente4)) %>% 
  select(-economicactivity) %>% 
  rename(code = ind)


# Diccionario final

dicc_actividades <- descriptores %>%  
  left_join(clasif_intens, join_by(tim_code == code)) 

# reemplazo codigo de países
reemplazos_codigos <- list(
  "A5_A7" = "ZSCA",
  "WXD"   = "ROW",
  "E"     = "ZEUR",
  "EU28XEU15" = "EU13",
  "W" = "WLD",
  "F" = "F_ESTPRO", # Agregado al geonomenclador.json
  "W_O" = "ZOTH",
  "S2" = "EASIA",
  "S2_S8" = "ZASI",
  "NAFTA" = "NAFTA_ESTPRO", #Agregado al geonomenclador.json
  "WXOECD" = "NONOECD",
  "W_O" = "ZOTH"
)

# Traigo nomenclador desde google drive
geonomenclador <- argendataR::get_nomenclador_geografico() %>%
  select(iso3 = codigo_fundar, pais_nombre = desc_fundar)

df_clean <- arrow::read_parquet(get_clean_path(fuente1))

df_output <- df_clean %>% 
  select(-desc_act) %>% 
  right_join(dicc_actividades, join_by(codigo_act == tim_code)) %>% 
  dplyr::filter(!is.na(personas_empleadas_miles)) %>% 
  mutate(variable = "Cantidad de personas o de puestos de trabajo, en miles") %>% 
  select(-intensidad_id_ocde) %>% 
  rename(valor = personas_empleadas_miles,
         tim_code = codigo_act,
         intensidad_ocde = intensidad_id_ocde_desc) %>%
  mutate(iso3 = recode(iso3, !!!reemplazos_codigos)) %>% 
  left_join(geonomenclador, join_by(iso3)) %>% 
  select(variable, anio, iso3, pais_nombre, gran_sector_id, gran_sector_desc, letra, letra_desc, tim_code, tim_code_desc, intensidad_ocde, rama_rd, rama_rd_desc,valor)


df_output[df_output$iso3 == "NAFTA_ESTPRO", c("pais_nombre")] <- "Países miembros del NAFTA"
df_output[df_output$iso3 == "F_ESTPRO", c("pais_nombre")] <- "África"


df_output <- df_output %>% 
  dplyr::filter(variable == "Cantidad de personas o de puestos de trabajo, en miles") %>% 
  group_by(iso3, anio, pais_nombre, intensidad_ocde) %>% 
  summarise(
    empleo = sum(valor, na.rm = T)
  ) %>% 
  ungroup() %>% 
  group_by(iso3, anio) %>% 
  mutate(
    share = empleo / sum(empleo, na.rm=T)
  ) %>% 
  ungroup()


# Modifico para que coincida con el nuevo formato
df_anterior <- argendataR::descargar_output(nombre =output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega") %>% 
  rename(intensidad_ocde = intensidad_id_ocde) %>% 
  left_join(geonomenclador, join_by(iso3))

df_anterior[df_anterior$iso3 == "ZNAM", c("iso3")] <- "NAFTA_ESTPRO"
df_anterior[df_anterior$iso3 == "NAFTA_ESTPRO", c("pais_nombre")] <- "Países miembros del NAFTA"
df_anterior[df_anterior$iso3 == "F_ESTPRO", c("pais_nombre")] <- "África"



comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  pk = c("anio","iso3", "intensidad_ocde"), # variables pk del dataset para hacer el join entre bases
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
  variable_nombre = c("intensidad_ocde","pais_nombre"),
  descripcion = c("Nivel de intensidad de uso de la tecnología, basado en OCDE",
                  "Nombre del país de referencia")
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
    pk = c("anio", "iso3", "intensidad_ocde"),
    es_serie_tiempo = T,
    columna_indice_tiempo = 'anio',
    descripcion_columnas = descripcion,
    cambio_nombre_cols = list('intensidad_ocde' = 'intensidad_id_ocde'),
    unidades = list("empleo" = "miles",
                    "share" = "unidades")
  )
