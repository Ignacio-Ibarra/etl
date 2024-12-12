################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "CRECIM"
output_anterior <- "var_part_1953_2022"
output_name <- "var_part_1953_vs_ultimo_anio"
analista = "Pablo Sonzogni"
fuente1 <- "R222C0"
fuente2 <- "R84C0"



get_raw_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}raw/")
  df_fuentes_raw <- fuentes_raw() 
  path_raw <- df_fuentes_raw[df_fuentes_raw$codigo == codigo,c("path_raw")]
  return(paste0(prefix, path_raw))
}

get_clean_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}clean/")
  df_fuentes_clean <- fuentes_clean() 
  path_clean <- df_fuentes_clean[df_fuentes_clean$codigo == codigo,c("path_clean")]
  return(paste0(prefix, path_clean))
}


# Cargo data desde server
empalme_df <- read_csv(get_raw_path(fuente1)) 

# daniel_df <- read_csv("pbg por provincia.xlsx - serie empalmada PIBpc.csv") %>%
#   select(-region_pbg) %>%
#   pivot_longer(-provincia, names_to ="anio", values_to = "vab_pc_dani", names_transform = as.numeric)
# 
# 
# X <- empalme_df %>%
#   left_join(daniel_df, join_by(provincia, anio)) %>%
#   mutate(pob_implicita_dani = vab_pb / vab_pc_dani)
#
#
# X %>% write_csv(., file="revisar.csv")

# A <- X %>% group_by(anio) %>% summarise(vab_pb = sum(vab_pb, na.rm = T),
#                                         pob_total = sum(pob_total, na.rm = T),
#                                         pob_dani = sum(pob_implicita_dani, na.rm=T))


dicc_provs <- read_csv(get_raw_path(fuente2)) %>% 
  select(prov_cod, prov_desc, reg_desc_fundar) %>% 
  mutate(region = case_when(
    reg_desc_fundar %in% c("Partidos del GBA", "Pampeana", "CABA") ~ "Pampeana y AMBA",
    reg_desc_fundar == "Noreste" ~ "NEA",
    reg_desc_fundar == "Noroeste" ~ "NOA",
    TRUE ~ reg_desc_fundar
  )) %>% 
  distinct(provincia_id = prov_cod, provincia_nombre = prov_desc, region) %>% 
  dplyr::filter(!(provincia_id == 6 & region == "Patagonia")) %>% 
  mutate(provincia_id = stringr::str_pad(provincia_id, width = 2, pad = "0", side = "left")) %>% 
  select(provincia_id, provincia_nombre, region_pbg = region)

ultimo_anio <- max(empalme_df$anio)

vab_1953_nacional <- sum(empalme_df[empalme_df$anio == 1953, c("vab_pb")])
vab_ultimo_anio_nacional <- sum(empalme_df[empalme_df$anio == ultimo_anio, c("vab_pb")])

df_output <- empalme_df %>% 
  dplyr::filter(provincia != "No distribuido") %>% 
  rename(provincia_nombre = provincia) %>% 
  dplyr::filter(anio %in% c(1953, max(anio))) %>% 
  pivot_wider(id_cols =provincia_nombre, names_from = anio, names_prefix = "vab_", values_from = vab_pb) %>% 
  mutate(participacion_1953 = vab_1953 / vab_1953_nacional,
         participacion_ultimo_anio = get(paste0("vab_", ultimo_anio))/vab_ultimo_anio_nacional,
         var_participacion = participacion_ultimo_anio - participacion_1953) %>% 
  left_join(dicc_provs, by = join_by(provincia_nombre)) %>% 
  select(provincia_id, provincia_nombre, region_pbg, var_participacion)



df_anterior <- argendataR::descargar_output(nombre = output_anterior, subtopico = subtopico, entrega_subtopico = "primera_entrega") %>% 
  mutate(provincia_nombre = ifelse(provincia_nombre=="Ciudad Autónoma de Buenos Aires", "CABA", provincia_nombre)) 

comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_anterior,
  pk = c("provincia_id"), # variables pk del dataset para hacer el join entre bases
  drop_joined_df =  F
)


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
  dplyr::filter(grepl(paste0("^", output_anterior,".csv"), dataset_archivo)) %>% 
  distinct(variable_nombre, descripcion) 


etiquetas_nuevas = data.frame(
  variable_nombre = c("var_participacion"),
  
  descripcion = c("Variación de la partipacion en el VABpb nacional entre 1895 y último año disponible (en pp)") 
)

# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output


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
    subtopico = subtopico,
    fuentes = colectar_fuentes(),
    analista = analista,
    pk = c("provincia_id"),
    control = comparacion, 
    columna_geo_referencia = "provincia_id",
    cambio_nombre_output = list('nombre_nuevo' = output_name,
                                'nombre_anterior' = output_anterior),
    nivel_agregacion = "provincia",
    descripcion_columnas = descripcion,
    unidades = list("var_participacion" = "puntos porcentuales")
  )
Y
