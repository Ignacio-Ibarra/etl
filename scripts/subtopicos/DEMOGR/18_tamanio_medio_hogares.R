# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "DEMOGR"
output_name <- "tamanio_medio_hogares.csv"
analista <- "Rafael Rofman & Ximena de la Fuente"

fuente1 <- 'R445C0' # INDEC. Censo 2022, Historia (1869-2010).
fuente2 <- "R447C288" # INDEC. Censo 2022 - Viviendas particulares por jurisdiccion
fuente3 <- 'R448C289' # INDEC. Censo 2022 - Población en viviendas particulares por jurisdisccion. 

df_historia <- argendataR::get_raw_path(fuente1) %>% 
  read.csv() 

df_viv2022 <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet() %>% 
  summarise(
    viviendas = sum(viviendas_particulares),
    anio = 2022
  )

df_pob2022 <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet() %>% 
  dplyr::filter(sexo == "Ambos") %>% 
  summarise(
    habitantes = sum(poblacion_viviendas_particulares),
    anio = 2022
  )


df_tamanio_2010 <- df_historia %>% 
  drop_na(viviendas) %>% 
  mutate(tamanio_medio_hogares = habitantes / viviendas) %>% 
  select(anio, tamanio_medio_hogares)




df_tamanio_2022 <-  df_viv2022 %>% 
  left_join(df_pob2022, join_by(anio)) %>% 
  mutate(tamanio_medio_hogares = habitantes / viviendas) %>% 
  select(anio, tamanio_medio_hogares)


df_output <- df_tamanio_2010 %>% 
  bind_rows(df_tamanio_2022)
