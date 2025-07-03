# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "CIETEC"
output_name <- "20_investigadoras_y_becarias_evo.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R360C235' # DNIC Sistema Integrado de Indicadores. Recursos humanos dedicados a I+D según sexo. Años 2003 – 2023


df_dnic <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()


df_output <- df_dnic %>% 
  mutate(funcion = case_when(
    grepl("Becario.*", funcion) ~ "Becarios",
    grepl("Investigado.*", funcion) ~ "Investigadores",
    grepl("Técnic.*", funcion) ~ "Técnicos",
    grepl("apoyo", funcion) ~ "Personal de apoyo",
    TRUE ~ NA_character_
    )
  ) %>% 
  dplyr::filter(funcion %in% c("Becarios", "Investigadores")) %>% 
  group_by(anio, funcion, sexo) %>% 
  summarise(personas_fisicas = sum(personas_fisicas, na.rm = T)) %>%
  ungroup() %>% 
  group_by(anio, funcion) %>% 
  mutate(
    share_mujeres = 100*personas_fisicas / sum(personas_fisicas, na.rm = T)
  ) %>% 
  ungroup() %>% 
  drop_na(share_mujeres) %>% 
  dplyr::filter(sexo == "Mujer") %>% 
  select(-sexo)




df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )



ggplot(df_output, aes(x = anio, y = share_mujeres, color = funcion)) +
  geom_line()+
  theme_minimal()+
  labs(y="Porcentaje de mujeres")+
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.text = element_text(color = "black"),  
    axis.title = element_text(color = "black")  
  )+
  ylim(0,NA)
