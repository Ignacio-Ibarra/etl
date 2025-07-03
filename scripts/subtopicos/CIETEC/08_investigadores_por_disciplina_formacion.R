# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "CIETEC"
output_name <- "08_investigadores_por_disciplina_formacion.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R359C234' # DNIC Sistema Integrado de Indicadores. Investigadores y becarios según sector de ejecución y disciplina de formación. Años 2009 – 2023

df_dnic <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()


df_output <- df_dnic %>% 
  dplyr::filter(anio == max(anio)) %>% 
  group_by(anio, disciplina_de_formacion) %>% 
  summarise(
    personas_fisicas = sum(personas_fisicas, na.rm = T)
  ) %>% 
  ungroup() %>% 
  group_by(anio) %>% 
  mutate(
    share = personas_fisicas / sum(personas_fisicas)
  ) %>% 
  ungroup()
  



df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )



escalar_a_100 <- function(vec) {
  if (abs(sum(vec) - 1) > 1e-8) {
    stop("El vector debe sumar 1.")
  }
  
  valores_raw <- vec * 100
  valores_redondeados <- floor(valores_raw)
  diferencia <- 100 - sum(valores_redondeados)
  
  residuos <- valores_raw - valores_redondeados
  indices_ordenados <- order(residuos, decreasing = TRUE)
  
  
  if (diferencia > 0) {
    valores_redondeados[indices_ordenados[1:diferencia]] <- 
      valores_redondeados[indices_ordenados[1:diferencia]] + 1
  }
  
  return(valores_redondeados)
}



df_plot <- df_output

df_plot$valor_waffle <- escalar_a_100(df_plot$share)

library(waffle)
library(RColorBrewer)

disciplinas <- df_plot$disciplina_de_formacion
paleta <- brewer.pal(length(disciplinas), "Set2")
colores <- setNames(paleta, disciplinas)

waffle(
  parts = setNames(df_plot$valor_waffle, df_plot$disciplina_de_formacion),  # Asignar valores con nombres de sectores
  rows = 10, 
  colors = colores,
  legend_pos = "bottom"
)
