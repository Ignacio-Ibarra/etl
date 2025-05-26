# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "PESCAS"
output_name <- "12_capturas_grupos_anio.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R331C204' # MAGyP Desembarque por puerto, flota, especie, anio, mes (2022- ultimo anio)
fuente2 <- 'R330C205' # MAGyP Desembarque por puerto, flota, especie, anio, mes (2013- 2021)
fuente3 <- 'R329C206' # MAGyP Desembarque por especie, anio, mes (1989 - 2012)

df_magyp_especie <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet(.)

diccionario_especie_grupo <- df_magyp_especie %>% distinct(especie, grupo)

df_magyp_puerto_flota_especie <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.) %>% 
  bind_rows(
    argendataR::get_clean_path(fuente2) %>% 
      arrow::read_parquet(.)
  ) %>% 
  left_join(diccionario_especie_grupo, join_by(especie)) %>% 
  mutate(
    grupo = case_when(
      grepl("Caballa.*|Anchoíta.*|Merluza hubbsi.*|Otros peces.*|Raya platana.*|Tiburón.*|Morena.*|Bathyraja.*|Lurión.*", especie) ~ "PEZ",
      grepl("Vieira.*|Almeja.*|Navaja.*|Panopea.*|Calamar.*|Pulpitos.*", especie) ~ "MOLU",
      grepl("Cangrejo.*", especie) ~ "CRUS",
      TRUE ~ grupo
    )
  )
      

df_output <- df_magyp_especie %>% 
  mutate(grupo = case_when(
    grepl("MOLU", grupo) ~ "Moluscos",
    grepl("PEZ", grupo) ~ "Peces",
    grepl("CRUS", grupo) ~ "Crustáceos",
    TRUE ~ NA_character_
  )) %>% 
  group_by(anio, grupo) %>% 
  summarise(
    desembarque_toneladas = sum(desembarque_toneladas, na.rm = T)
  ) %>% 
  ungroup() %>% 
  bind_rows(
    
    df_magyp_puerto_flota_especie %>% 
      mutate(grupo = case_when(
        grepl("MOLU", grupo) ~ "Moluscos",
        grepl("PEZ", grupo) ~ "Peces",
        grepl("CRUS", grupo) ~ "Crustáceos",
        TRUE ~ NA_character_
      )) %>% 
      group_by(anio, grupo) %>% 
      summarise(
        desembarque_toneladas = sum(desembarque_toneladas, na.rm = T)
      ) %>% 
      ungroup()
    
  ) %>% 
  group_by(anio) %>% 
  mutate(share = 100*desembarque_toneladas/sum(desembarque_toneladas)) %>% 
  ungroup()


df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )

total_df <- df_output %>%
  group_by(anio, grupo = "Total") %>%
  summarise(total_desembarque = sum(desembarque_toneladas))

ggplot(df_output, aes(x = anio, y = share, fill = grupo)) + 
  geom_area(alpha = 0.6) +  # Alpha para suavizar colores
  # geom_line(data = total_df, aes(x = anio, y = total_desembarque), 
  #           color = "black", linewidth = 0.8) + 
  scale_fill_brewer(palette = "Set1") +  # Paleta de colores suaves
  # scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = " mil")) +
  theme_minimal() +  
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text = element_text(color = "black"),  
    axis.title = element_text(color = "black")  
  ) +
  labs(y = "Capturas marinas (en porcentaje)", x = "")

