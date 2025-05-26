# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "PESCAS"
output_name <- "11_total_desembarques.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R321C193' # FAO Fisheries and Aquaculture Data Collection. Global Aquaculture Production - File: Capture_Quantity.csv
fuente2 <- 'R320C190' # FAO Fisheries and Aquaculture Data Collection. Global Aquaculture Production - File: CL_FI_SPECIES_GROUPS
fuente3 <- 'R320C191' # FAO Fisheries and Aquaculture Data Collection. Global Aquaculture Production - File: CL_FI_COUNTRY_GROUPS
fuente4 <- 'R320C192' # FAO Fisheries and Aquaculture Data Collection. Global Aquaculture Production - File: CL_FI_WATERAREA_GROUPS
fuente5 <- 'R331C204' # MAGyP Desembarque por puerto, flota, especie, anio, mes (2022- ultimo anio)
fuente6 <- 'R330C205' # MAGyP Desembarque por puerto, flota, especie, anio, mes (2013- 2021)
fuente7 <- 'R329C206' # MAGyP Desembarque por especie, anio, mes (1989 - 2012)

# Lectura de archivos Parquet
df_quantity <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

df_species <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet(.) %>% 
  select(x3a_code, especie_es = name_es, scientific_name, isscaap_group_es, yearbook_group_en)

df_areas <- argendataR::get_clean_path(fuente4) %>% 
  arrow::read_parquet(.) %>% 
  select(code, area_es = name_es, inland_marine_group_es)



df_desembarque_fao <- df_quantity %>% 
  dplyr::filter(measure != "Q_no_1") %>% 
  dplyr::filter(country_un_code == 32) %>% 
  left_join(df_species, join_by(species_alpha_3_code == x3a_code)) %>% 
  dplyr::filter(yearbook_group_en == "Aquatic animals (Fish, crustaceans and molluscs, etc.)") %>% 
  left_join(df_areas, join_by(area_code == code)) %>% 
  dplyr::filter(inland_marine_group_es == "Áreas marítimas") %>% 
  group_by(anio = period) %>% 
  summarise(
    desembarque_toneladas_fao = sum(value, na.rm = T)
  ) %>% 
  ungroup()


df_magyp_puerto_flota_especie <- argendataR::get_clean_path(fuente6) %>% 
  arrow::read_parquet(.) %>% 
  bind_rows(
    argendataR::get_clean_path(fuente5) %>% 
  arrow::read_parquet(.)
  )


df_magyp_especie <- argendataR::get_clean_path(fuente7) %>% 
  arrow::read_parquet(.)



df_desembarque_magyp <- df_magyp_puerto_flota_especie %>% 
  group_by(anio) %>% 
  summarise(
    desembarque_toneladas_magyp = sum(desembarque_toneladas, na.rm = T)
  ) %>% 
  ungroup() %>% 
  bind_rows(
    df_magyp_especie %>% 
      group_by(anio) %>% 
      summarise(
        desembarque_toneladas_magyp = sum(desembarque_toneladas, na.rm = T)
      ) %>% 
      ungroup()
  )


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


df_dos_fuentes <- df_desembarque_fao %>% 
  full_join(df_desembarque_magyp, join_by(anio)) 

model <- stats::lm(formula = desembarque_toneladas_magyp ~ desembarque_toneladas_fao, data = df_dos_fuentes %>% filter(!is.na(desembarque_toneladas_magyp)))


df_output <- df_dos_fuentes %>%
  mutate(valor_empalme = case_when(
    !is.na(desembarque_toneladas_magyp) ~ desembarque_toneladas_magyp,
    TRUE ~ as.numeric(stats::predict(model, newdata = tibble(desembarque_toneladas_fao = desembarque_toneladas_fao)))
  ))

df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )


ggplot(df_output, aes(x = anio, y = valor_empalme)) + 
  geom_line(color = "#7ab5c5") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = " mil")) +
  theme_minimal() +
  theme(
    axis.text = element_text(color = "black"),  # Color negro para los números de los ejes
    axis.title = element_text(color = "black")  # Color negro para los títulos de los ejes
  )+
  labs(y = "Capturas marinas (en miles de toneladas)", x = "") 
