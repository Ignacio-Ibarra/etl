# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "CIETEC"
output_name <- "17_i_d_y_act_pib.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R423C271' # DNIC Gasto en I+D en relación al PBI
fuente2 <- 'R352C227'# RICYT Gasto en ACT en relación al PBI


df_dnic_i_d <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet() %>%
  rename( i_d_pib = inversion_en_i_d_en_relacion_al_pbi_en_porcentaje ) %>% 
  select(anio, i_d_pib)
  

df_dnic_act <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet() %>% 
  dplyr::filter(pais == "Argentina") %>% 
  mutate(act_pib = 100*valor) %>% 
  select(anio, act_pib)


df_output <- df_dnic_i_d %>% 
  full_join(
    df_dnic_act ,
    join_by(anio)
  ) %>% 
  arrange(anio) 


df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("~/data/{subtopico}/{output_name}")
  )

