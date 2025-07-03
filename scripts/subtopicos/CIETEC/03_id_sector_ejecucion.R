# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "CIETEC"
output_name <- "03_id_sector_ejecucion.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R390C243' # RICyT Gasto en I+D por sector de ejecución
fuente2 <- 'R361C238' # OECD Main Science and Technology Indicators (MSTI database)


geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(iso3 = geocodigo, pais = name_long)

# Lectura de archivos Parquet
df_ricyt <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.) 



df_oecd <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet(.) 


selected_measures <- c("GV","H", "PNP", "B")

df_oecd_sector_ejecucion_ult_anio <- df_oecd %>% 
  dplyr::filter(measure %in% selected_measures, 
                unit_of_measure == "Percentage of gross domestic expenditure on R&D") %>% 
  select(iso3 = ref_area, anio = time_period, sector_id = measure, sector = measure_2, valor = obs_value) %>% 
  group_by(iso3, sector_id) %>% 
  dplyr::filter(anio == max(anio)) %>% 
  ungroup() %>% 
  mutate(
    sector = case_when(
    sector_id == "PNP" ~ "Org. priv. sin fines de lucro",
    sector_id == "GV" ~ "Gobierno",
    sector_id == "H" ~ "Educación Superior",
    sector_id == "B" ~ "Empresas (Públicas y Privadas)"
    )
  ) %>% 
  select(-sector_id) %>% 
  left_join(geo_front, join_by(iso3)) %>% 
  mutate(fuente = "OECD")
  

america_no <- c("F5205", "LAC", "TLA", "DESHUM_ZZH.LAC", "DESHUM_AHDI.LAC")


df_ricyt_ult_anio <- df_ricyt %>% 
  complete(anio, pais, sector, fill = list(valor = 0)) %>% 
  group_by(pais, sector) %>% 
  dplyr::filter(anio == max(anio)) %>% 
  ungroup() %>% 
  left_join(geo_front %>% dplyr::filter(!(iso3 %in% america_no)), join_by(pais)) %>% 
  mutate(iso3 = ifelse(pais == "Iberoamérica", "RICYT_IBE", iso3),
         fuente = "RICYT") %>% 
  mutate(valor = valor * 100)


df_output <- bind_rows(df_oecd_sector_ejecucion_ult_anio, df_ricyt_ult_anio) %>% 
  group_by(iso3, anio, sector) %>% 
  mutate( n_fuentes = n()) %>% 
  ungroup() %>% 
  group_by(iso3, sector) %>% 
  dplyr::filter(anio == max(anio), 
                (n_fuentes == 2 & fuente == "OECD") | n_fuentes == 1) %>% 
  ungroup() %>% 
  select(iso3, pais_nombre = pais, ultimo_anio_disponible = anio, sector, valor, fuente)
  



df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )


paises_seleccionados <- c(
  
  "Paraguay",
  "México",
  "Uruguay",
  "América Latina y el Caribe",
  "Costa Rica",
  "Chile",
  "Argentina",
  "Brasil",
  "Colombia",
  "España",
  "Unión Europea (27 países)",
  "Suiza",
  "Reino Unido",
  "Países miembros de OCDE",
  "Estados Unidos",
  "Japón",
  "Corea del Sur",
  "Irlanda",
  "Taiwán",
  "Israel"
)


plot_data <- df_output %>% 
  dplyr::filter(pais_nombre %in% paises_seleccionados) %>% 
  mutate(label = paste(pais_nombre, "(", fuente, " - ", ultimo_anio_disponible, ")")) %>%
  mutate(label = stats::reorder(label, valor * (sector == "Empresas (Públicas y Privadas)"))) %>% 
  mutate(
    sector = factor(sector, levels = c(
      "Org. priv. sin fines de lucro", 
      "Gobierno", 
      "Educación Superior", 
      "Empresas (Públicas y Privadas)"
      )
    )
  )

ggplot(plot_data, aes(x = label, y = valor, fill = sector)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  scale_fill_manual(values = c(
    "Gobierno" = "#f28985", 
    "Empresas (Públicas y Privadas)" = "#6d389c", 
    "Educación Superior" = "#3b8df2", 
    "Org. priv. sin fines de lucro" = "#f38f1c" 
  )) +
  labs(
    title = "",
    x = "",
    y = "",
    fill = "Sector"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black")
  )
