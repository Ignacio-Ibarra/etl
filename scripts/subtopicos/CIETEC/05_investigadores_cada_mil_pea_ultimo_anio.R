# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "CIETEC"
output_name <- "05_investigadores_cada_mil_pea_ultimo_anio.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R400C251' # DNIC Sistema Integrado de Indicadores. Investigadores y becarios cada 1.000 integrantes de la PEA.
fuente2 <- 'R345C220' # RICyT Investigadores cada 1000 integrantes de la PEA (EJC)
fuente3 <- 'R361C238' # OECD Main Science and Technology Indicators (MSTI database)
fuente4 <- 'R401C252' # OECD Annual labour force survey, summary tables


df_dnic <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

df_ricyt <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet()

df_oecd_msti <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet()

df_oecd_labour <- argendataR::get_clean_path(fuente4) %>% 
  arrow::read_parquet()

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(iso3 = geocodigo, pais = name_long)

#########################
# ARMO INDICADOR OECD 
#########################

df_oecd_pea <- df_oecd_labour %>% 
  dplyr::filter(measure == "LF", # Labour Force
                transformation == "_Z", # Not applicable
                sex == "_T", # Total
                worker_status == "_Z", # Not applicable
                freq == "A", # Annual
                age == "Y_GE15" # 15 years or over
                ) %>% 
  mutate(lab_force = obs_value * 1000) %>% 
  select(ref_area, time_period, lab_force) %>% 
  drop_na(lab_force)


df_oecd_ejc <- df_oecd_msti %>% 
  dplyr::filter(measure == "T_RS", # Researchers
                unit_measure == "FTE", # Full time equivalent
                transformation == "_Z", # Not applicable
                price_base == "_Z", # Not applicable
                freq == "A", # Annual
  ) %>% 
  select(ref_area, time_period, ejc = obs_value) %>% 
  drop_na(ejc)


df_oecd_ejc_pea <- inner_join(df_oecd_pea, df_oecd_ejc, join_by(ref_area, time_period)) %>% 
  mutate(ejc_pea_1000 = 1000 * ejc / lab_force,
         fuente = "OECD") %>% 
  select(iso3 = ref_area, anio = time_period, ejc_pea_1000, fuente)


df_dnic_ejc_pea <- df_dnic %>% 
  mutate(iso3 = "ARG", fuente = "DNIC") %>% 
  select(iso3, anio, ejc_pea_1000 = investigadores_y_becarios_en_ejc_1_000_pea, fuente)


america_no <- c("F5205", "LAC", "TLA", "DESHUM_ZZH.LAC", "DESHUM_AHDI.LAC")

df_ricyt_ejc_pea <- df_ricyt %>% 
  left_join(geo_front %>% dplyr::filter(!(iso3 %in% america_no)), join_by(pais)) %>% 
  mutate(iso3 = case_when(
    pais == "República Dominicana" ~ "DOM",
    pais == "Iberoamérica" ~ "RICYT_IBE",
    TRUE ~ iso3),
    fuente = "RICYT") %>% 
  select(iso3, anio, ejc_pea_1000 = valor, fuente)


#####################
# Argentina (DNIC)
# Resto (OECD)
# Resto Resto (RICyT)
#####################

df_output <- bind_rows(
  df_dnic_ejc_pea,
  df_oecd_ejc_pea %>% anti_join(df_dnic_ejc_pea, join_by(iso3, anio)),
  df_ricyt_ejc_pea %>% anti_join(df_oecd_ejc_pea, join_by(iso3, anio)) %>% 
    anti_join(df_dnic_ejc_pea, join_by(iso3, anio))) %>% 
  group_by(iso3) %>% 
  dplyr::filter(anio == max(anio)) %>% 
  ungroup() %>% 
  left_join(geo_front, join_by(iso3)) %>% 
  mutate(pais = ifelse(iso3 == "RICYT_IBE", "Iberoamérica", pais)) %>% 
  arrange(-ejc_pea_1000) %>% 
  select(iso3, ultimo_anio_disponible = anio, pais, ejc_pea_1000, fuente )




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
  "Países miembros de OCDE"
)



plot_data <- df_output %>% 
  mutate(pais_anio = paste(stringr::str_wrap(pais, width = 20), "(", ultimo_anio_disponible, ")")) %>% 
  mutate(pais_anio = factor(pais_anio, levels = rev(unique(pais_anio)))) %>% 
  arrange(-ejc_pea_1000) %>% 
  dplyr::filter(pais %in% paises_seleccionados) 

regular_texto <- 0.05


especiales <- c("América Latina y el Caribe",
                "Países miembros de OCDE")


ggplot(plot_data, aes(x = ejc_pea_1000, y = pais_anio, 
                      fill = case_when(
                        pais == "Argentina" ~ "Argentina",
                        pais %in% especiales ~ "Especiales",
                        TRUE ~ "Otros"
                      ))) + 
  geom_col(color = "black", linewidth = 0.15, position = position_nudge(y = 0.2), width = 0.8) +  
  scale_fill_manual(values = c("Argentina" = "#45bcc5", "Especiales" = "grey", "Otros" = "#FC5A0A")) +  # Colores condicionales
  geom_text(
    aes(label = paste0(round(ejc_pea_1000, 1)," (", fuente,")"), 
        x = ejc_pea_1000 + regular_texto),  
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
    plot.margin = margin(5, 30, 5, 5) # arriba, derecha, abajo, izquierda
  )
