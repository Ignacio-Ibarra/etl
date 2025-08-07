# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "FISCAL"
output_name <- "gasto_publico_consolidado_pib_per_capita_paises.csv"
analista <- "María Fernanda Villafañe & Micaela Fernandez Erlauer"

fuente1 <- 'R424C272'
fuente2 <- 'R325C200'
fuente3 <- 'R126C0'


df_imf <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

df_mecon <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet(.)

df_wb <- argendataR::get_raw_path(fuente3) %>% 
  readr::read_csv(.) %>% 
  select(iso3 = iso3c, anio = year, gdp_pc_ppp_kd = `NY.GDP.PCAP.PP.KD`)


df_arg <- df_mecon %>% 
  dplyr::filter(nombre_apertura == "GASTO PÚBLICO TOTAL") %>%
  mutate(iso3 = 'ARG', 
         pais_nombre = 'Argentina') %>% 
  select(iso3, pais_nombre, anio, gasto_pub_gdp = valores)


df_gasto <- df_imf %>% 
  dplyr::filter(iso3 != "ARG") %>% 
  select(iso3, pais_nombre, anio, gasto_pub_gdp = exp) %>% 
  bind_rows(df_arg) 


geonomenclador <- argendataR::get_nomenclador_geografico() %>%
  dplyr::filter(nivel_agregacion == 'pais') %>% 
  select(iso3 = codigo_fundar, continente = continente_fundar)



df_output <- df_wb %>% 
  inner_join(df_gasto, join_by(anio, iso3)) %>% 
  group_by(iso3) %>% 
  dplyr::filter(anio == max(anio)) %>% 
  ungroup() %>% 
  drop_na(gasto_pub_gdp) %>% 
  drop_na(gdp_pc_ppp_kd) %>%
  left_join(geonomenclador, join_by(iso3)) %>%  
  rename(`Gasto público consolidado como porcentaje del PIB` = gasto_pub_gdp, `PIB per cápita PPP (en dólares internacionales constantes de 2021)` =  gdp_pc_ppp_kd) %>% 
  pivot_longer(., !all_of(c('anio', 'iso3', 'pais_nombre', 'continente')),
               names_to = 'variable',
               values_to = 'valor') 
  

cb_palette <- c(
  "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
)

p <- ggplot(df_output ,
            aes(x = gdp_pc_ppp_kd, y = gasto_pub_gdp,
                color = factor(continente),
                text = pais_nombre)) +  # Cambiá "pais" si el nombre es distinto
  geom_point(size = 3) +
  scale_color_manual(values = cb_palette) +
  labs(
    x = "PIB per cápita PPP (en dólares internacionales constantes de 2021)",
    y = "Gasto público consolidado como porcentaje del PIB",
    color = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


plotly::ggplotly(p, tooltip = "text")

