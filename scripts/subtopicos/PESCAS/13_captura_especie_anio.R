# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "PESCAS"
output_name <- "13_captura_especie_anio.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R331C204' # MAGyP Desembarque por puerto, flota, especie, anio, mes (2022- ultimo anio)
fuente2 <- 'R330C205' # MAGyP Desembarque por puerto, flota, especie, anio, mes (2013- 2021)
fuente3 <- 'R329C206' # MAGyP Desembarque por especie, anio, mes (1989 - 2012)

df_magyp_especie <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet(.)

df_magyp_puerto_flota_especie <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.) %>% 
  bind_rows(
    argendataR::get_clean_path(fuente2) %>% 
      arrow::read_parquet(.)
  ) 


df_output <- df_magyp_especie %>% 
  mutate(
    especie_agregada = case_when(
      grepl("Merluza hubbsi.*", especie) ~ "Merluza Hubbsi",
      grepl("Calamar Illex*", especie) ~ "Calamar Illex",
      grepl("Langostino.*", especie) ~ "Langostino",
      TRUE ~ "Otras especies"
    )
  ) %>% 
  group_by(anio, especie_agregada) %>% 
  summarise(
    desembarque_toneladas = sum(desembarque_toneladas, na.rm = T)
  ) %>% 
  ungroup() %>% 
  bind_rows(
    
    df_magyp_puerto_flota_especie %>% 
      mutate(
        especie_agregada = case_when(
          grepl("Merluza hubbsi.*", especie) ~ "Merluza Hubbsi",
          grepl("Calamar Illex*", especie) ~ "Calamar Illex",
          grepl("Langostino.*", especie) ~ "Langostino",
          TRUE ~ "Otras especies"
        )
      ) %>% 
      group_by(anio, especie_agregada) %>% 
      summarise(
        desembarque_toneladas = sum(desembarque_toneladas, na.rm = T)
      ) %>% 
      ungroup()
  )


df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )


ggplot(df_output, aes(x = anio, y = desembarque_toneladas, color = especie_agregada)) + 
  geom_line(linewidth = 0.8) +  # Alpha para suavizar colores
  scale_fill_brewer(palette = "Set2") +  # Paleta de colores suaves
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = " mil")) +
  theme_minimal() +  
  theme(
    axis.text = element_text(color = "black"),  
    axis.title = element_text(color = "black")  
  ) +
  labs(y = "Capturas marinas (en miles de toneladas)", x = "", color = "Especie")
