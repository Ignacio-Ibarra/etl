# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "PESCAS"
output_name <- "04_desembarque_especie_ultimo_anio.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R331C204' # MAGyP Desembarque por puerto, flota, especie, anio, mes (2022- ultimo anio)

# Lectura de archivos Parquet
df_magyp <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)


df_output <- df_magyp %>% 
  dplyr::filter(anio == max(anio)) %>% 
  mutate(
    especie = case_when(
      especie %in% c("Langostino", "Calamar Illex") ~ especie,
      grepl("hubbsi", especie) ~ "Merluza Hubbsi",
      TRUE ~ "Otras especies"
    )
  ) %>% 
  group_by(anio, especie) %>% 
  summarise(
    desembarque_toneladas = sum(desembarque_toneladas, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(
    share = desembarque_toneladas / sum(desembarque_toneladas)
  )

df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )


df_plot <- df_output %>% 
  mutate(valor_waffle = round(share * 100))

library(waffle)

# Definir colores para cada sector
colores <- c("Merluza Hubbsi" = "#E41A1C", 
             "Langostino" = "#377EB8", 
             "Calamar Illex" = "#4DAF4A", 
             "Otras especies" = "#FF7F00")


waffle(
  parts = setNames(df_plot$valor_waffle, df_plot$especie),  # Asignar valores con nombres de sectores
  rows = 10, 
  colors = colores,
  legend_pos = "bottom"
)
