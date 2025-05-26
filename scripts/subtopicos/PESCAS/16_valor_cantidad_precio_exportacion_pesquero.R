#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "PESCAS"
output_name <- "16_valor_cantidad_precio_exportacion_pesquero.csv"
analista <- "Ignacio Ibarra"
fuente1 <- 'R319C188' # INDEC_COMEX


df_indec <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)


pescado <- c("03","1504","1604","1605","230120", "05080000", "051191","16030090") #obtenidos de https://www.indec.gob.ar/ftp/cuadros/economia/nota_metodologica_complejos_exportadores_2024.pdf

pattern_pescado <- paste0("^(", paste(pescado, collapse = "|"), ")")


df_output <- df_indec %>% 
  drop_na(fob) %>% 
  dplyr::filter(grepl(pattern_pescado, ncm8)) %>%
  group_by(anio) %>% 
  summarise(
    toneladas = sum(pnet_kg, na.rm = T)/1000,
    fob = sum(fob, na.rm = T),
    precio = sum(fob, na.rm = T) / sum(pnet_kg, na.rm = T)/1000
  ) %>% 
  ungroup() %>% 
  mutate(
    fob_index = (fob / fob[anio == 2002]) * 100,
    toneladas_index = (toneladas / toneladas[anio == 2002]) * 100,
    precio_index = (precio / precio[anio == 2002]) * 100
  ) %>% 
  select(-toneladas, -precio, -fob)

df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )


plot_data <- df_output %>%
  select(anio, Valor = fob_index, Toneladas = toneladas_index, Precio = precio_index) %>% 
  pivot_longer(
    !anio, 
    names_to = "variable",
    values_to = "indice"
  )

ggplot(plot_data, aes(x = anio, y = indice, color = variable)) + 
  geom_line(linewidth = 1.2) +  # Grosor de línea para mejor visualización
  geom_point(size = 2) +   # Puntos para destacar cada año
  labs(
    # title = "Evolución de las Exportaciones Pesqueras (Base 100)",
    x = "Año",
    y = "Índice (Base 100)",
    color = "Variable"
  ) + 
  theme_minimal() + # Tema limpio y profesional
  theme(legend.position = "bottom") # Ubicar la leyenda abajo
  
