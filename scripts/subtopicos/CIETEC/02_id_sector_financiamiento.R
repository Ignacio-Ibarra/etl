# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "CIETEC"
output_name <- "02_id_sector_financiamiento.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R399C250' # DNIC I+D por sector de financiamiento

# Lectura de archivos Parquet
df_dnic <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.) 


df_output <- df_dnic %>% 
  dplyr::filter(anio == max(anio)) %>% 
  select(anio, sector_de_financiamiento, x_de_inversion_por_sector)


df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )


df_plot <- df_output %>% 
  mutate(valor_waffle = round(x_de_inversion_por_sector))

library(waffle)

# Definir colores para cada sector
colores <- c("Sector Público" = "#E41A1C", 
             "Sector Privado" = "#377EB8", 
             "Sector Externo" = "#FF7F00")


waffle(
  parts = setNames(df_plot$valor_waffle, df_plot$sector_de_financiamiento),  # Asignar valores con nombres de sectores
  rows = 10, 
  colors = colores,
  legend_pos = "bottom"
)
