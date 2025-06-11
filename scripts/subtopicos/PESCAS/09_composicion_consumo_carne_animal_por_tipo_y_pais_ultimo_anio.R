################################################################################
##                              Dataset: nombre                               ##
################################################################################


#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "PESCAS"
output_name <- "09_composicion_consumo_carne_animal_por_tipo_y_pais_ultimo_anio.csv"
analista = "Ignacio Ibarra"
fuente1 <- 'R299C167' # FAO FBS


df_fao_fbs <- arrow::read_parquet(argendataR::get_clean_path(fuente1)) 


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
  select(-flags, -notes, -element_code, -element) %>% 
  drop_na(value)


df_paises <- df_fao_fbs_filtered %>% 
  mutate(tipo_carne = carnes[item],
         value = replace_na(value, 0)) %>% 
  group_by(anio = year, iso3, pais, tipo_carne) %>% 
  summarise( valor = sum(value, na.rm = T)) %>% 
  ungroup() %>% 
  dplyr::filter(anio == max(anio)) %>% 
  complete(anio, iso3, tipo_carne, fill = list(valor = 0)) %>%  
  group_by(iso3) %>% 
  mutate(share = 100*valor / sum(valor)) %>%
  fill(pais, .direction = 'down') %>% 
  ungroup()


df_mundo <- df_paises %>% 
  group_by(anio, iso3 = "WLD", pais = "Mundo", tipo_carne) %>% 
  summarise(
    valor = mean(valor, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(
    share = 100*valor / sum(valor)
  )


df_output <- df_paises %>% bind_rows(df_mundo)

df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  mutate(anio = as.integer(anio))


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("iso3", "tipo_carne"), # variables pk del dataset para hacer el join entre bases
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
    pk =  c("iso3", "tipo_carne"),
    es_serie_tiempo = F,
    control = comparacion,
    columna_indice_tiempo = NULL,
    columna_geo_referencia = 'iso3',
    nivel_agregacion = NULL,
  )
