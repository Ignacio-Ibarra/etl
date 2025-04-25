################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "CRECIM"
output_name <- "provincia_pib_pc_scatter"
analista = "Pablo Sonzogni"
fuente1 <- "R222C0"
fuente2 <- "R84C0"



# Cargo data desde server
empalme_df <- argendataR::get_raw_path(fuente1) %>% 
  read_csv()


dicc_provs <- argendataR::get_raw_path(fuente2) %>% 
  read_csv() %>% 
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

geo <- argendataR::get_nomenclador_geografico_front()

df_output <- empalme_df %>% 
  dplyr::filter(provincia != "No distribuido") %>% 
  rename(provincia_nombre = provincia) %>% 
  dplyr::filter(anio %in% c(1895, max(anio))) %>% 
  pivot_wider(id_cols =provincia_nombre, names_from = anio, names_prefix = "pib_pc_", values_from = vab_pb_per_capita) %>% 
  mutate(var_pib_pc_1895_ultimo_anio = (get(paste0("pib_pc_",ultimo_anio))/pib_pc_1895) - 1) %>% 
  rename_with(
    ~"pib_pc_ultimo_anio",
    .cols = paste0("pib_pc_", ultimo_anio)
  )%>% 
  left_join(dicc_provs, by = join_by(provincia_nombre)) %>% 
  select(provincia_id, provincia_nombre, region_pbg, pib_pc_1895, pib_pc_ultimo_anio, var_pib_pc_1895_ultimo_anio) %>%
  mutate(provincia_nombre = textclean::replace_non_ascii(tolower(provincia_nombre))) %>% 
  left_join(geo %>% 
              mutate(name_short = textclean::replace_non_ascii(tolower(name_short))),
            by = c("provincia_nombre" = "name_short")
  ) %>% 
  select(-c(provincia_nombre, iso_2, provincia_id)) %>% 
  rename(provincia_id = geocodigo, provincia_nombre = name_long) %>% 
  select(provincia_id, provincia_nombre, region_pbg, pib_pc_1895, pib_pc_ultimo_anio, var_pib_pc_1895_ultimo_anio)


df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega") 
 


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("provincia_id"), # variables pk del dataset para hacer el join entre bases
  drop_joined_df =  F
)


check_iso3(df_output$provincia_id)





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
  dplyr::filter(grepl(paste0("^", output_name), dataset_archivo)) %>% 
  distinct(variable_nombre, descripcion) 


# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output

etiquetas_nuevas <- data.frame(
  variable_nombre = c("pib_pc_ultimo_anio", 
                      "var_pib_pc_1895_ultimo_anio"),
  descripcion = c("PIB a precios básicos per cápita en pesos constantes de 2004, del ultimo año disponible",
                  "Variación proporcional entre el PIBpc del ultimo año contra el de 1895")
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
    subtopico = subtopico,
    fuentes = colectar_fuentes(),
    control = comparacion,
    aclaraciones = "Se corrigieron los codigos de provincia de acuerdo al geonomenclador de Argendata. Se agrega columna pib_pc_ultimo_anio",
    analista = analista,
    pk = c("provincia_id"),
    columna_geo_referencia = "provincia_id",
    nivel_agregacion = "provincia",
    descripcion_columnas = descripcion, 
    etiquetas_indicadores = list("pib_pc_1895" = "PIB a precios básicos per cápita a pesos constantes de 2004 del año 1895",
                                 "pib_pc_ultimo_anio" = "PIB a precios básicos per cápita en pesos constantes de 2004, del ultimo año disponible",
                                 "var_pib_pc_1895_ultimo_anio" = "Variación proporcional entre el PIB a precios básicos per capita de 1895 y ultimo año disponible"),
    unidades = list("pib_pc_1895" = "pesos constantes de 2004",
                    "pib_pc_ultimo_anio" = "pesos constantes de 2004",
                    "var_pib_pc_1895_ultimo_anio" = "unidades")
  )

mandar_data(paste0(output_name, ".csv"), subtopico = "CRECIM", branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = "CRECIM",  branch = "dev")


