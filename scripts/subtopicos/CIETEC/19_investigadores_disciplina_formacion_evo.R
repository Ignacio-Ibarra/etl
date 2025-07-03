# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "CIETEC"
output_name <- "19_investigadores_disciplina_formacion_evo.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R359C234' # DNIC Sistema Integrado de Indicadores. Investigadores y becarios según sector de ejecución y disciplina de formación. Años 2009 – 2023

df_dnic <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()


df_output <- df_dnic %>% 
  group_by(anio, disciplina_de_formacion) %>% 
  summarise(
    personas_fisicas = sum(personas_fisicas, na.rm = T)
  ) %>% 
  ungroup() %>% 
  group_by(anio) %>% 
  mutate(
    share = 100*personas_fisicas / sum(personas_fisicas)
  ) %>% 
  ungroup()




df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )


plot_data <- df_output %>% 
  dplyr::filter(anio %in% c(2013,2023)) %>% 
  pivot_wider(id_cols = disciplina_de_formacion, names_from = anio, values_from = share)

plot_data %>% write_csv_fundar("cietec_g19.csv")
