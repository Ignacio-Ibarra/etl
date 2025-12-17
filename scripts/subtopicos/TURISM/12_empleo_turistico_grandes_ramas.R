# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "TURISM"
output_name <- "empleo_turistico_grandes_ramas.csv"
analista <- "Carola della Paolera & Joan Manuel Vezzato"

fuente1 <- 'R470C317' # CST Empleo


df_cst <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

df_turismo <- df_cst %>%
  dplyr::filter(anio == max(anio), !grepl("Total.*", industria_turistica), indicador == "Total") %>% 
  mutate(agregado = "Turística") %>% 
  select(anio, agregado, industria = industria_turistica, empleo_total = value)


df_resto <- df_cst %>% 
  dplyr::filter(anio == max(anio), grepl("Total.*", industria_turistica), indicador == "Total") %>% 
  mutate(agregado = "No turístico") %>% 
  select(anio, agregado, industria_turistica, value) %>% 
  pivot_wider(., id_cols = c(anio, agregado), names_from = industria_turistica, values_from = value) %>% 
  mutate(empleo_total = `Total Economía` - `Total Industrias turísticas`, 
         industria = "No turístico") %>% 
  select(anio, agregado, industria, empleo_total)


df_output <- bind_rows(df_turismo, df_resto) %>% 
  mutate(prop =  100 * empleo_total / sum(empleo_total), 
         prop_intraturismo = 100 * ifelse(industria == "No turístico", NA_real_, empleo_total) / sum(ifelse(industria == "No turístico", NA_real_, empleo_total), na.rm=T)
         ) 


df_output %>% 
  write_csv_fundar(
    .,
    glue::glue("~/data/{subtopico}/{output_name}")
  )
