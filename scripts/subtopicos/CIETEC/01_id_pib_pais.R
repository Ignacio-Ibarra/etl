# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "CIETEC"
output_name <- "01_id_pib_pais.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R343C218' # UNESCO GERD as a percentage of GDP

# Lectura de archivos Parquet
df_unesco <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.) %>% 
  rename(iso3 = geoUnit, anio = year, gerd_gdp = value)

df_unesco_ult_anio <- df_unesco %>% 
  group_by(iso3) %>% 
  dplyr::filter(anio == max(anio)) %>% 
  ungroup()


geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(iso3 = geocodigo, pais_nombre = name_long) 

df_paises <- df_unesco_ult_anio  %>% 
  inner_join(geo_front, join_by(iso3)) %>% 
  select(ultimo_anio_disponible = anio, iso3, pais_nombre, gerd_gdp)


df_no_paises <- df_unesco_ult_anio %>% 
  anti_join(df_paises, join_by(iso3)) %>% 
  dplyr::filter(grepl("WB:", iso3)) %>% 
  mutate(iso3 = case_when(
    
    grepl("High income", iso3) ~ "HIC",
    grepl("Low income", iso3) ~ "LIC",
    grepl("Middle income", iso3) ~ "MIC",
    grepl("Upper middle income", iso3) ~ "UMC",
    grepl("Lower middle income", iso3) ~ "LMC",
    grepl("World", iso3) ~ "WLD"
    )
  ) %>% 
  left_join(geo_front, join_by(iso3)) %>% 
  select(ultimo_anio_disponible = anio, iso3, pais_nombre, gerd_gdp)


df_output <- bind_rows(df_paises, df_no_paises) %>% 
  arrange(-gerd_gdp) %>% 
  mutate(gerd_gdp = gerd_gdp/100)


df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )


paises_seleccionados <- c(
  "Argentina",
  "Chile",
  "Perú",
  "Brasil",
  "México",
  "Uruguay",
  "Colombia", 
  "Rusia",
  "India",
  "Países de ingreso medio",
  "Países de altos ingresos",
  "Mundo",
  "Canadá",
  "España",
  "Estados Unidos",
  "Japón",
  "Italia",
  "Bélgica",
  "Alemania",
  "Israel",
  "Corea del Sur"
)

plot_data <- df_output %>% 
  mutate(pais_anio = paste(pais_nombre, "(", ultimo_anio_disponible, ")")) %>% 
  mutate(pais_nombre = factor(pais_nombre, levels = rev(unique(pais_nombre)))) %>% 
  dplyr::filter(pais_nombre %in% paises_seleccionados)

regular_texto <- 0.001

ggplot(plot_data, aes(x = gerd_gdp, y = pais_nombre, 
                      fill = case_when(
                        pais_nombre == "Argentina" ~ "Argentina",
                        pais_nombre %in% c("Países de ingreso medio",
                                           "Países de altos ingresos",
                                           "Mundo") ~ "Especiales",
                        TRUE ~ "Otros"
                      ))) + 
  geom_col(color = "black", linewidth = 0.15, position = position_nudge(y = 0.2), width = 0.8) +  
  scale_fill_manual(values = c("Argentina" = "#45bcc5", "Especiales" = "#404B84", "Otros" = "#FC5A0A")) +  # Colores condicionales
  geom_text(
    aes(label = scales::percent(gerd_gdp, accuracy = 0.01),x = gerd_gdp + regular_texto),  
    vjust = 0, hjust = 0, color = "black", size = 3) +  # Color de las etiquetas en blanco
  labs(y = "", x = "") +
  theme_minimal() +
  theme(
    legend.position = "none",  # Oculta la leyenda
    axis.text = element_text(color = "black"),  
    axis.title = element_text(color = "black")  
  )+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))

