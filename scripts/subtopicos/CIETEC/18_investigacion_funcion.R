# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "CIETEC"
output_name <- "18_investigacion_funcion.csv"
analista <- "Ignacio Ibarra"
# Defino las fuentes
fuente1 <- 'R360C235' # DNIC Sistema Integrado de Indicadores. Recursos humanos dedicados a I+D según sexo. Años 2003 – 2023

fuente2 <- 'R406C257' #RICyT Personal de ciencia y tecnología (PF)


df_dnic <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

df_ricyt <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet()

df_dnic_inv_funcion <- df_dnic %>% 
  mutate(funcion = case_when(
                      grepl("Becario.*", funcion) ~ "Investigadores",
                      grepl("Investigado.*", funcion) ~ "Investigadores",
                      grepl("Técnic.*", funcion) ~ "Técnicos y Personal Asimilado", # uso categorias de RICyT
                      grepl("apoyo", funcion) ~ "Otro Personal de Apoyo", # uso categorias de RICyT
                      TRUE ~ NA_character_),
        iso3 = "ARG", 
        pais = "Argentina"
  ) %>% 
  group_by(iso3, pais, anio, funcion) %>% 
  summarise(personas_fisicas_dnic = sum(personas_fisicas, na.rm = T)) %>%
  ungroup() %>% 
  dplyr::filter(anio>=2017) 

america_no <- c("F5205", "LAC", "TLA", "DESHUM_ZZH.LAC", "DESHUM_AHDI.LAC")

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(iso3 = geocodigo, pais = name_long)

df_ricyt_inv_funcion <- df_ricyt %>% 
  mutate(pais = ifelse(pais == "República Dominicana", "Rep. Dominicana", pais)) %>% 
  left_join(geo_front %>% dplyr::filter(!(iso3 %in% america_no)), join_by(pais)) %>% 
  mutate(iso3 = ifelse(pais == "Iberoamérica", "RICYT_IBE", iso3)) %>% 
  rename(personas_fisicas_ricyt = valor) %>% 
  dplyr::filter(funcion != "Total")


ricyt_ult_anio_arg <- df_ricyt_inv_funcion %>% dplyr::filter(pais == "Argentina", anio == max(anio)) %>% distinct(anio) %>% pull()

df_output <- full_join(df_dnic_inv_funcion, df_ricyt_inv_funcion, join_by(iso3, pais, anio, funcion)) %>% 
  mutate(personas_fisicas = ifelse(anio>ricyt_ult_anio_arg & pais == "Argentina", personas_fisicas_dnic, personas_fisicas_ricyt)) %>% 
  dplyr::filter(pais == "Argentina") %>% 
  select(iso3, pais, anio, funcion, personas_fisicas)

df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )



ggplot(df_output, aes(x = anio, y = personas_fisicas, fill = funcion))  + 
  geom_col() +
  theme_minimal() +
  labs(y="Personal en I+D", x="")+
  theme(
    legend.position = "bottom", 
    legend.title = element_blank(),
    axis.text = element_text(color = "black"),  
    axis.title = element_text(color = "black")  
  )
