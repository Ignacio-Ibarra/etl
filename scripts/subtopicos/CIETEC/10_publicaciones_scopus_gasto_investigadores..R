# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "CIETEC"
output_name <- "10_publicaciones_scopus_gasto_investigadores.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R348C223' # RICyT Publicaciones en Scopus en relación al gasto en I+D
fuente2 <- 'R349C224' # RICyT Publicaciones en SCOPUS cada 100 investigadores

df_ricyt_gasto <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

df_ricyt_investigadores <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet()

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(iso3 = geocodigo, pais = name_long)


america_no <- c("F5205", "LAC", "TLA", "DESHUM_ZZH.LAC", "DESHUM_AHDI.LAC")

df_output_gasto <- df_ricyt_gasto %>% 
  dplyr::filter(medida == "Publicaciones en SCOPUS en relacion al Gasto en I+D (cada millón de U$S PPC)") %>% 
  group_by(pais) %>% 
  dplyr::filter(anio == max(anio)) %>% 
  ungroup()  %>% 
  left_join(geo_front %>% dplyr::filter(!(iso3 %in% america_no)), join_by(pais)) %>% 
  mutate(iso3 = case_when(
    pais == "Iberoamérica" ~ "RICYT_IBE",
    pais == "República Dominicana" ~ "DOM",
    TRUE ~ iso3)) %>% 
  arrange(-valor) 


df_output_investigadores <- df_ricyt_investigadores %>% 
  dplyr::filter(calculo == "Publicaciones en SCOPUS cada 100 investigadores (EJC)") %>% 
  group_by(pais) %>% 
  dplyr::filter(anio == max(anio)) %>% 
  ungroup()  %>% 
  left_join(geo_front %>% dplyr::filter(!(iso3 %in% america_no)), join_by(pais)) %>% 
  mutate(iso3 = case_when(
    pais == "Iberoamérica" ~ "RICYT_IBE",
    pais == "República Dominicana" ~ "DOM",
    TRUE ~ iso3)) %>% 
  arrange(-valor) %>% 
  rename(medida = calculo)


df_output <- bind_rows(df_output_gasto, df_output_investigadores)

df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("~/data/{subtopico}/{output_name}")
  )


