# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "CIETEC"
output_name <- "06_inversion_por_investigador.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R346C221' # RICyT Investigadores cada 1000 integrantes de la PEA (EJC)
fuente2 <- 'R361C238' # OECD Main Science and Technology Indicators (MSTI database)


df_ricyt <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

df_oecd_msti <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet()


geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(iso3 = geocodigo, pais = name_long)

#########################
# ARMO INDICADOR OECD 
#########################

df_oecd_gasto_i_d_ppp <- df_oecd_msti %>% 
  dplyr::filter(measure == "G",
                unit_measure == "USD_PPP", 
                price_base == "V", # Precios corrientes
                transformation == "_Z",
                freq == "A") %>% 
  select(measure_2, time_period, ref_area, unit_of_measure, price_base_2, unit_multiplier, gasto_i_d = obs_value, base_per)


df_oecd_ejc <- df_oecd_msti %>% 
  dplyr::filter(measure == "T_RS", # Researchers
                unit_measure == "FTE", # Full time equivalent
                transformation == "_Z", # Not applicable
                price_base == "_Z", # Not applicable
                freq == "A", # Annual
  ) %>% 
  select(ref_area, time_period, ejc = obs_value) %>% 
  drop_na(ejc)


df_oecd_gasto_ejc <- inner_join(df_oecd_gasto_i_d_ppp, df_oecd_ejc, join_by(ref_area, time_period)) %>% 
  mutate(gasto_investigador = gasto_i_d * 1000 / ejc, 
         fuente = "OECD") %>% 
  select(anio = time_period, iso3 = ref_area, gasto_investigador, gasto_i_d, ejc, fuente)


##############
# Tomo RICyT
##############


america_no <- c("F5205", "LAC", "TLA", "DESHUM_ZZH.LAC", "DESHUM_AHDI.LAC")

df_ricyt_gasto_ejc <- df_ricyt %>% 
  dplyr::filter(calculo == "EJC") %>% 
  left_join(geo_front %>% dplyr::filter(!(iso3 %in% america_no)), join_by(pais)) %>% 
  mutate(iso3 = case_when(
    pais == "Iberoamérica" ~ "RICYT_IBE",
    TRUE ~ iso3),
    fuente = "RICYT") %>% 
  select(iso3, anio, gasto_investigador = valor, fuente)


#####################
# Argentina (DNIC)
# Resto (OECD)
# Resto Resto (RICyT)
#####################

df_output <- bind_rows(
  df_oecd_gasto_ejc,
  df_ricyt_gasto_ejc %>% anti_join(df_oecd_gasto_ejc, join_by(iso3, anio))
  ) %>% 
  dplyr::filter(anio == max(anio)-1) %>% 
  left_join(geo_front, join_by(iso3)) %>% 
  mutate(pais = ifelse(iso3 == "RICYT_IBE", "Iberoamérica", pais)) %>% 
  arrange(-gasto_investigador) %>% 
  select(iso3, ultimo_anio_disponible = anio, pais, gasto_investigador, fuente )




df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )


paises_seleccionados <- c(
  "Argentina",
  "Chile",
  "México",
  "Brasil",
  "Paraguay",
  "Colombia", 
  "Rusia",
  "Japón",
  "Países miembros de OCDE",
  "Canadá",
  "España",
  "Estados Unidos",
  "Alemania",
  "Francia",
  "Corea del Sur",
  "Dinamarca",
  "Canadá",
  "América Latina y el Caribe", 
  "Países miembros de OCDE",
  "Panamá",
  "Taiwán",
  "Irlanda"
)



plot_data <- df_output %>% 
  mutate(label_pais = stringr::str_wrap(pais, width = 20)) %>% 
  mutate(label_pais = factor(label_pais, levels = rev(unique(label_pais)))) %>% 
  arrange(-gasto_investigador) %>% 
  dplyr::filter(pais %in% paises_seleccionados) 

regular_texto <- 1.5


especiales <- c("América Latina y el Caribe",
                "Países miembros de OCDE")


ggplot(plot_data, aes(x = gasto_investigador, y = label_pais, 
                      fill = case_when(
                        pais == "Argentina" ~ "Argentina",
                        pais %in% especiales ~ "Especiales",
                        TRUE ~ "Otros"
                      ))) + 
  geom_col(color = "black", linewidth = 0.15, position = position_nudge(y = 0.2), width = 0.8) +  
  scale_fill_manual(values = c("Argentina" = "#45bcc5", "Especiales" = "grey", "Otros" = "#FC5A0A")) +  # Colores condicionales
  geom_text(
    aes(label = round(gasto_investigador, 1), 
        x = gasto_investigador + regular_texto),  
    vjust = -0.5, hjust = 0, color = "black", size = 3) +  # Color de las etiquetas en blanco
  labs(x = "", 
       y = "") +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    title = element_text(color = "#7F396C"),
    plot.margin = margin(5, 35, 5, 5) # arriba, derecha, abajo, izquierda
  )

