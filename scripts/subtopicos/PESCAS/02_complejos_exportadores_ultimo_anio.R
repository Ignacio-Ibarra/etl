#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "PESCAS"
output_name <- "02_complejos_exportadores_ultimo_anio.csv"
analista <- "Ignacio Ibarra"
fuente1 <- 'R338C211' # INDEC Complejos Exportadores. Revisión 2018. 2021 a ultimo anio

df_indec <- argendataR::get_clean_path(fuente1) %>%
  arrow::read_parquet(.) 


df_output <- df_indec %>% 
  dplyr::filter(anio == max(anio), grepl("^Complejo.*", complejos)) %>%
  mutate(complejos = stringr::str_to_sentence(str_extract(complejos, "^Complejo (.*)", group = 1))) %>% 
  arrange(-expo) 
  


df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )




plot_data <- df_output %>% 
  slice_head(n = 15) %>%  
  arrange(expo) %>% 
  mutate(complejos = factor(complejos, levels = unique(complejos))) 



regular_texto <- -5

ggplot(plot_data, aes(x = expo, y = complejos, 
                      fill = case_when(
                        complejos == "Pesquero" ~ "Pesquero",
                        TRUE ~ "Otros"
                      ))) + 
  geom_col(color = "black", linewidth = 0.2, position = position_nudge(y = 0.2), width = 0.8) +  
  scale_fill_manual(values = c("Pesquero" = "#45bcc5", "Otros" = "#fc5a0a")) +  # Colores condicionales
  geom_text(aes(
    label = format(round(expo, 0), big.mark = ".", decimal.mark = ",", scientific = FALSE),
    x = expo - regular_texto), 
    size = 3,
    vjust = 0, hjust = 0, color = "black", fontface = "bold") +  # Color de las etiquetas en blanco
  labs(y = "", x = "Exportaciones (en millones de dólares)") +
  theme_minimal() +
  theme(
    legend.position = "none",  # Oculta la leyenda
    axis.text = element_text(color = "black"),  
    axis.title = element_text(color = "black")  
  )
