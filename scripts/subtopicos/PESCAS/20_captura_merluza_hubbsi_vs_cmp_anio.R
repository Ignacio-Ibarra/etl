# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "PESCAS"
output_name <- "20_captura_merluza_hubbsi_vs_cmp_anio.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R331C204' # MAGyP Desembarque por puerto, flota, especie, anio, mes (2022- ultimo anio)
fuente2 <- 'R330C205' # MAGyP Desembarque por puerto, flota, especie, anio, mes (2013- 2021)
fuente3 <- 'R329C206' # MAGyP Desembarque por especie, anio, mes (1989 - 2012)
fuente4 <- 'R332C207' # MAGyP Capturas Máximas Permisibles

df_magyp_especie <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet(.)

df_magyp_puerto_flota_especie <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.) %>% 
  bind_rows(
    argendataR::get_clean_path(fuente2) %>% 
      arrow::read_parquet(.)
  ) 


df_merluza <- df_magyp_especie %>% 
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
  ) %>% 
  dplyr::filter(especie_agregada == "Merluza Hubbsi")


# Los datos anteriores a 2022 de CMP se ingresan manualmente ya que la fuente proviene de las
# Resoluciones del Consejo Nacional Pesquero. Se buscaron manualmente cada una de las 
# resoluciones y se ingresó el dato aquí. 
# TODO procesamiento de descarga. 
df_cmp_anterior <- read.csv(text = "anio,cmp_norte,cmp_sur,cmp_total
2010,48000,290000,338000
2011,48000,273000,321000
2012,40000,273000,313000
2013,35000,277000,312000
2014,32000,290000,322000
2015,30000,290000,320000
2016,30000,290000,320000
2017,30000,290000,320000
2018,35000,290000,325000
2019,33000,280000,313000
2020,42000,290000,332000
2021,42000,305000,347000") %>% 
  select(anio, cmp_total)


df_cmp_actual <- argendataR::get_clean_path(fuente4) %>% 
  arrow::read_parquet(.) %>% 
  dplyr::filter(grepl("merluza hubbsi.*", especie, ignore.case=T)) %>% 
  group_by(anio) %>% 
  summarise(cmp_total = sum(cmp, na.rm = T)) %>% 
  ungroup()

df_cmp <- bind_rows(df_cmp_anterior, df_cmp_actual)

df_output <- df_merluza %>% 
  left_join(
    df_cmp , join_by(anio)
  ) %>% 
  select(-especie_agregada)

df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )




plot_data <- df_output %>% rename(`Merluza Hubbsi` = desembarque_toneladas,
                                  `Captura Máxima Permisible` = cmp_total) %>% 
  pivot_longer(!anio)

ggplot(plot_data, aes(x = anio, y = value, color = name)) + 
  geom_line() +  
  geom_vline(xintercept = 2010, alpha = 0.6, linetype = "dashed") + 
  scale_color_brewer(palette = "Set2") +  
  scale_y_continuous(labels = scales::label_comma(scale = 1e-3, suffix = " mil"),
                     limits = c(0, NA)) +
  theme_minimal() +  
  theme(
    axis.text = element_text(color = "black"),  
    axis.title = element_text(color = "black")  
  ) +
  labs(y = "Capturas marinas (en miles de toneladas)", x = "", color = "") +
  geom_curve(
    aes(x = 2014, y = 500000, xend = 2011, yend = 400000),  # Ajusta posiciones
    curvature = 0.3,  # Controla la curvatura de la flecha
    arrow = arrow(length = unit(0.1, "cm")),  # Tamaño de la flecha
    color = "black"
  ) +
  annotate("text", x = 2014.5, y = 510000, label = "Implementación de las \nCuotas Individuales Transferibles \nde Captura (CITC)",
           hjust = 0, size = 3, color = "black")  # hjust = 0 justifica a la izquierda
