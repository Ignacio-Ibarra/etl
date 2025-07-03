# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "CIETEC"
output_name <- "07_investigadores_conicet_categoria_genero_ultimo_anio.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R354C230' # Base de Datos del CONICET - Personas. Investigadores. Investigadores por Categoría y Género 2023


df_conicet <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()


df_output <- df_conicet %>% 
  dplyr::filter(genero !="OTRO") %>% 
  mutate(
    genero = ifelse(genero == "HOMBRES", "Varones", "Mujeres"),
    categoria = tools::toTitleCase(tolower(categoria))
  ) %>% 
  group_by(categoria, genero) %>% 
  summarise(cantidad = n()) %>% 
  ungroup() %>% 
  group_by(categoria) %>% 
  mutate(share = 100*cantidad / sum(cantidad)) %>% 
  ungroup() 



df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )






plot_data <- df_output %>% 
  mutate(categoria_ordenada = factor(categoria, levels = c("Asistente",
                                                            "Adjunto",
                                                            "Independiente",
                                                            "Principal",
                                                            "Superior"
                                                            ),
                                      ordered = TRUE) 
         ) 

ggplot(plot_data, aes(x = categoria_ordenada, y = share, fill = genero)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(
    "Varones" = "#1f77b4", # azul
    "Mujeres" = "#ff7f0e"  # naranja
  )) +
  theme_minimal() +
  labs(x = "", y = "", fill = "") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black")
  )

