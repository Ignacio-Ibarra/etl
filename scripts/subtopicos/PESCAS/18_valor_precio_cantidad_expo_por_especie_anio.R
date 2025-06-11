#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "PESCAS"
output_name <- "18_valor_precio_cantidad_expo_por_especie_anio.csv"
analista <- "Ignacio Ibarra"
fuente1 <- 'R324C197' # FAO Global Aquatic Trade - All parteners aggregated - Trade Value
fuente2 <- 'R324C198' # FAO Global Aquatic Trade - All parteners aggregated - Trade Quantity
fuente3 <- 'R324C199' # FAO Global Aquatic Trade - All parteners aggregated - Commodity ISSCFC


df_valores <- argendataR::get_clean_path(fuente1) %>%
  arrow::read_parquet(.) %>% 
  dplyr::filter(country_reporter_un_code == "32", trade_flow_alpha_code == "E") %>% 
  select(-country_reporter_un_code, -trade_flow_alpha_code)

df_cantidades <- argendataR::get_clean_path(fuente2) %>%
  arrow::read_parquet(.) %>% 
  dplyr::filter(country_reporter_un_code == "32", trade_flow_alpha_code == "E") %>% 
  select(-country_reporter_un_code, -trade_flow_alpha_code)

df_comodity <- argendataR::get_clean_path(fuente3) %>%
  arrow::read_parquet(.) %>% 
  select(code, isscfc_parent, name_es)


df_expo_arg <- df_valores %>% 
  left_join(df_cantidades, join_by(commodity_fao_code, period), suffix = c("_valores","_cantidades")) %>% 
  left_join(df_comodity, join_by(commodity_fao_code == code))

df_output <- df_expo_arg %>% 
  mutate(
    especie = case_when(
      grepl("(.*langostino.*|.*Langostino.*|.*camarones.*|.*Camarones.*)", name_es) ~ "Langostino",
      # commodity_fao_code == "036.0.1.4.5.60" ~ "Langostino",
      grepl("(.*Calamares.*Illex.*|.*calamares.*illex.*|.*calamares.*|.*Calamar.*|.*calamar.*)", name_es) ~ "Calamar", 
      # commodity_fao_code == "036.0.2.4.7.40" ~ "Calamar", 
      commodity_fao_code %in% c("034.3.1.5.2.63","034.4.1.5.2.63","034.1.5.2.63","034.2.5.2.63") ~ "Merluza Hubbsi",
      TRUE ~ "Otras especies"
      )
  ) %>% 
  group_by(anio = period, especie) %>%
  summarise(
    valores = sum(value_valores, na.rm = T),
    cantidades = sum(value_cantidades, na.rm = T),
    precio = valores / cantidades
  ) %>%
  ungroup() %>% 
  dplyr::filter(anio>=2002) %>% 
  group_by(anio) %>% 
  dplyr::filter(n() == 4) %>% 
  ungroup() %>% 
  arrange(anio, especie) %>% 
  mutate(
    indice_valores = 100 * valores / valores[anio == min(anio)],
    indice_toneladas = 100 * cantidades / cantidades[anio == min(anio)],
    indice_precio = 100 * precio / precio[anio == min(anio)]
  ) %>% select(anio, especie, starts_with("indice_")) %>%
  pivot_longer(starts_with("indice_"), 
               names_to = "variable",
               names_transform = function(x){str_remove(x, "indice_") %>% tools::toTitleCase(.)},
               values_to = "valor") 



df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  mutate(anio = as.integer(anio))



pk <- c("anio", "especie", "variable")

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
    columna_indice_tiempo = 'anio',
    columna_geo_referencia = NULL,
    nivel_agregacion = NULL,
  )