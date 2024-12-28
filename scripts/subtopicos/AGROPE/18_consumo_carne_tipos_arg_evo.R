################################################################################
##                              Dataset: nombre                               ##
################################################################################


#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "AGROPE"
output_name <- "consumo_carne_tipos_arg_evo"
analista = "Franco A. Mendoza y Kevin Corfield"
fuente1 <- "R299C167" # FAO FBS
fuente2 <- "R300C168" # FAO FBSH


# nombres_fao <- c("Meat, beef | 00002731 || Food available for consumption | 0645pc || kilograms per year per capita" = "Vacuna",
#                  "Fish and seafood | 00002960 || Food available for consumption | 0645pc || kilograms per year per capita" = "Pescados y mariscos",
#                  "Meat, Other | 00002735 || Food available for consumption | 0645pc || kilograms per year per capita" = "Otras carnes",
#                  "Meat, sheep and goat | 00002732 || Food available for consumption | 0645pc || kilograms per year per capita" = "Caprina y ovina",
#                  "Meat, pig | 00002733 || Food available for consumption | 0645pc || kilograms per year per capita" = "Porcina",
#                  "Meat, poultry | 00002734 || Food available for consumption | 0645pc || kilograms per year per capita" = "Aviar")
# 
# 
# df_owid_arg <- read_csv("per-capita-meat-type.csv") %>%
#   select(-Entity) %>%
#   rename(anio = Year, iso3 = Code) %>%
#   pivot_longer(!matches("anio|iso3"),
#                names_to = 'grupo_carne',
#                values_to = 'value_owid') %>%
#   mutate(
#     grupo_carne = recode(grupo_carne, !!!nombres_fao)
#   )

df_fao_fbs <- arrow::read_parquet(argendataR::get_clean_path(fuente1)) 

df_fao_fbsh <- arrow::read_parquet(argendataR::get_clean_path(fuente2))


carnes <- c("Bovine Meat" = "Vacuna",
            "Fish, Body Oil" = "Pescados y mariscos",
            "Fish, Liver Oi" = "Pescados y mariscos",
            "Freshwater Fish" = "Pescados y mariscos",
            "Demersal Fish" = "Pescados y mariscos",
            "Pelagic Fish" = "Pescados y mariscos",
            "Marine Fish, Other" = "Pescados y mariscos",
            "Crustaceans" = "Pescados y mariscos",
            "Cephalopods" = "Pescados y mariscos", 
            "Molluscs, Other" = "Pescados y mariscos",
            "Aquatic Animals, Others" = "Pescados y mariscos",
            "Meat, Other" = "Otras carnes",
            "Mutton & Goat Meat" = "Caprina y ovina",
            "Pigmeat" = "Porcina",
            "Poultry Meat" = "Aviar")

df_fao_fbs_filtered <- df_fao_fbs %>% 
  dplyr::filter(item %in% names(carnes), element == "Food supply quantity (kg/capita/yr)")  %>% 
  mutate( grupo_carne = carnes[item]) %>% 
  group_by(year, iso3, pais, grupo_carne) %>% 
  summarise(value_new = sum(value, na.rm = T)) %>% 
  ungroup()


df_fao_fbsh_filtered <- df_fao_fbsh %>% 
  dplyr::filter(item %in% names(carnes), element == "Food supply quantity (kg/capita/yr)")   %>% 
  mutate( grupo_carne = carnes[item]) %>% 
  group_by(year, iso3, pais, grupo_carne) %>% 
  summarise(value_old = sum(value, na.rm = T)) %>% 
  ungroup()

impute_backward <- function(A, B) {
  # Calcular las variaciones relativas de B
  result <- rep(NA_real_, length(A))
  
  VarB <- B / dplyr::lag(B)
  
  # Encontrar el primer índice con un valor no nulo en A
  t0 <- min(which(!is.na(A)))
  
  result[t0] = A[t0]
  
  # Imputar hacia atrás
  for (t in (t0 - 1):1) {
    if (!is.na(VarB[t + 1]) & is.na(A[t])) {
      result[t] <- result[t + 1] / VarB[t + 1]
    }
  }
  
  return(result)
}



df_fao_fbs_empalme <- df_fao_fbs_filtered %>% 
  full_join(df_fao_fbsh_filtered, join_by(year, iso3, pais, grupo_carne)) %>% 
  group_by(iso3, grupo_carne) %>%
  filter(any(year == 2010 & !is.na(value_new) & !is.na(value_old))) %>%
  ungroup() %>% 
  arrange(iso3, grupo_carne, year) %>% 
  group_by(iso3, grupo_carne) %>% 
  mutate(valor_ = impute_backward(value_new, value_old),
         valor_empalme = ifelse(is.na(value_new), valor_, value_new)) %>% 
  ungroup() 


# comparacion_df <- df_fao_fbs_empalme %>%
#   select(anio = year,
#          iso3,
#          pais,
#          grupo_carne,
#          value_new,
#          value_old,
#          valor_empalme) %>%
#   left_join(df_owid_arg, join_by(anio, iso3, grupo_carne)) %>%
#   pivot_longer(cols = starts_with("val"),
#                names_to = "variable",
#                values_to = 'consumo_kg_anio_capita')
# 
# ggplot(comparacion_df %>% filter(iso3 == "USA"), aes(x = anio, y = consumo_kg_anio_capita, color = variable)) +
#   geom_line()+
#   facet_wrap(~grupo_carne, scales = 'free')
# 
# ggplot(comparacion_df %>%
#          filter(iso3 == "USA") %>%
#          filter(grupo_carne == "Vacuna"),
#        aes(x = anio, y = consumo_kg_anio_capita, color = variable)) +
#   geom_line()


df_output <- df_fao_fbs_empalme %>% 
  dplyr::filter(iso3 == "ARG") %>% 
  select(anio = year, grupo_carne, valor = valor_empalme)
  
  



df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  mutate(anio = as.integer(anio)) %>% 
  mutate(
    grupo_carne = case_when(
      grupo_carne == "aviar" ~ "Aviar",
      grupo_carne == "vacuna" ~ "Vacuna",
      grupo_carne == "caprina_ovina" ~ "Caprina y ovina",
      grupo_carne == "porcina" ~ "Porcina",
      grupo_carne == "otras_carnes" ~ "Otras carnes",
      TRUE ~ "Pescados y mariscos",
    )
  )


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c('grupo_carne','anio'), # variables pk del dataset para hacer el join entre bases
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
  variable_nombre = c("grupo_carne"),
  descripcion = c("Tipo de carne: Aviar, Porcina, Vacuna, Caprina y ovina y Otras carnes")
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
    pk = c("grupo_carne",'anio'),
    es_serie_tiempo = T,
    control = comparacion, 
    columna_indice_tiempo = 'anio',
    descripcion_columnas = descripcion,
    unidades = list("valor" = "kilogramos por año por persona"),
    aclaraciones = "Se modificó la fuente de información, antes era OWID ahora es FAO. FAO no posee datos de consumo de Pescado y Mariscos para Argentina"
  )