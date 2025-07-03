# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "CIETEC"
output_name <- "17_i_d_y_act_pib.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R351C226' # RICYT Gasto en I+D en relación al PBI
fuente2 <- 'R352C227'# RICYT Gasto en ACT en relación al PBI


df_dnic_i_d <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

df_dnic_act <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet()


df_output <- df_dnic_i_d %>% 
  dplyr::filter(pais == "Argentina") %>% 
  mutate(i_d_pib = 100*valor) %>% 
  select(-valor) %>% 
  full_join(
    df_dnic_act %>% 
      dplyr::filter(pais == "Argentina") %>% 
      mutate(act_pib = 100*valor) %>% 
      select(-valor),
    join_by(anio, pais)
  ) %>% 
  arrange(anio) %>% 
  select(-pais)



df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )


plot_data <- df_output %>% pivot_longer(
  !all_of(c("pais","anio")),
  names_to = "medida",
  values_to = "porcentaje_pib"
) %>% 
  mutate(medida = ifelse(medida == "act_pib", "Actividades Científico Tecnológicas", "Investigación y Desarrollo (I+D)"))


ggplot(plot_data, aes(x = anio, y = porcentaje_pib, color = medida))  + 
  geom_line() +
  theme_minimal() +
  labs(y="Porcentaje del PIB", x="")+
  theme(
    legend.position = "bottom", 
    legend.title = element_blank(),
    axis.text = element_text(color = "black"),  
    axis.title = element_text(color = "black")  
  )+
  ylim(c(0,NA))
  
