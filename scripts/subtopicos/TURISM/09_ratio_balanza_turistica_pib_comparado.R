# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "TURISM"
output_name <- "ratio_balanza_turistica_pib_comparado.csv"
analista <- "Carola della Paolera & Joan Manuel Vezzato"

fuente1 <- 'R479C0'
fuente2 <- 'R213C0'


df_imf <- argendataR::get_raw_path(fuente1) %>% 
  read.csv()


df_wb <- argendataR::get_raw_path(fuente2) %>% 
  read.csv()

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long)


df_balanza_turistica_periodo <- df_imf %>% 
  select(geocodigoFundar = COUNTRY,
         anio = TIME_PERIOD,
         valor = OBS_VALUE,
         indicador = INDICATOR,
         cuenta_bop = BOP_ACCOUNTING_ENTRY) %>% 
  dplyr::filter(indicador == "SD", anio %in% 2016:max(anio)) %>%  
  left_join(geo_front, join_by(geocodigoFundar)) %>%
  group_by(geocodigoFundar, geonombreFundar, cuenta_bop) %>% 
  summarise(
    valor = sum(valor, na.rm = T)
  ) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(geocodigoFundar, geonombreFundar),
              names_from = cuenta_bop, values_from = valor) %>% 
  mutate(balanza = CD_T - DB_T) %>% 
  select(geocodigoFundar, geonombreFundar, balanza) %>% 
  drop_na(balanza) %>% 
  drop_na(geonombreFundar) 



df_gdp_periodo <- df_wb %>% 
  select(
    anio = year, 
    geocodigoFundar = iso3c,
    gdp_current_prices = `NY.GDP.MKTP.CD`
  ) %>% 
  dplyr::filter(anio %in% 2016:max(df_imf$TIME_PERIOD)) %>% 
  group_by(geocodigoFundar) %>% 
  summarise(
    gdp_current_prices = sum(gdp_current_prices, na.rm = T)
  ) %>% 
  ungroup() %>% 
  drop_na(gdp_current_prices)



df_output <- df_balanza_turistica_periodo %>% 
  inner_join(df_gdp_periodo, join_by(geocodigoFundar)) %>% 
  dplyr::filter(gdp_current_prices >0) %>% 
  mutate(ratio_balanza_pib = 100 * balanza / gdp_current_prices) %>% 
  arrange(-ratio_balanza_pib)


seleccion <- c("MAC", "ARG", "MDV", "ABW", "HRV", "GEO", "JOR", "GRC",
               "KHM", "ESP", "URY", "FRA", "USA", "CHL", "BRA", "ARG", "DEU",
               "BEL", "NOR", "UKR")


df_plot <- df_output %>% 
  dplyr::filter(
    geocodigoFundar %in% seleccion
  ) %>% 
  mutate(
    geonombreFundar = stats::reorder(geonombreFundar, ratio_balanza_pib),
    color_deficit= ifelse(ratio_balanza_pib < 0, "darkred", "lightblue")
  ) 


ggplot(df_plot, aes(
  y = geonombreFundar, 
  x = ratio_balanza_pib,
  fill = color_deficit
)) +
  geom_col() +
  geom_text(
    aes(
      label = scales::percent(ratio_balanza_pib/100, accuracy = 0.1),
      hjust = ifelse(ratio_balanza_pib > 0, -0.1, 1.1)   # <--- clave
    ),
    size = 2.8
  ) +
  scale_fill_identity() +
  theme_minimal() +
  coord_cartesian(
    xlim = c(
      min(df_plot$ratio_balanza_pib) * 1.1,
      max(df_plot$ratio_balanza_pib) * 1.1
    )
  ) +
  theme(legend.position = "none")
