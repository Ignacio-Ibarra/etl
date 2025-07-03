# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "CIETEC"
output_name <- "09_publicaciones_scopus_disciplina.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R364C236' # RICyT Publicaciones en SCOPUS según disciplina



df_ricyt <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(iso3 = geocodigo, pais = name_long)


america_no <- c("F5205", "LAC", "TLA", "DESHUM_ZZH.LAC", "DESHUM_AHDI.LAC")

df_output <- df_ricyt %>% 
  dplyr::filter(pais != "Total") %>% 
  group_by(pais) %>% 
  dplyr::filter(anio == max(anio)) %>% 
  ungroup()  %>% 
  left_join(geo_front %>% dplyr::filter(!(iso3 %in% america_no)), join_by(pais)) %>% 
  mutate(iso3 = case_when(
    pais == "Iberoamérica" ~ "RICYT_IBE",
    pais == "Haiti" ~ "HTI",
    pais == "República Dominicana" ~ "DOM",
    TRUE ~ iso3)) %>% 
  group_by(pais) %>% 
  mutate(share = 100*valor / sum(valor)) %>% 
  ungroup() %>% 
  select(anio, iso3, pais, disciplina, share)

df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  mutate(anio = as.integer(anio))


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("iso3"), # variables pk del dataset para hacer el join entre bases
  drop_joined_df =  F
)

colectar_fuentes <- function(pattern = "^fuente.*"){
  
  # Genero un vector de codigos posibles
  posibles_codigos <- c(fuentes_raw()$codigo,fuentes_clean()$codigo)
  
  # Usar ls() para buscar variables en el entorno global
  variable_names <- ls(pattern = pattern, envir = globalenv())
  
  # Obtener los valores de esas variables
  valores <- unlist(mget(variable_names, envir = globalenv()))
  
  # Filtrar aquellas variables que sean de tipo character (string)
  # Esto es para que la comparacion sea posible en la linea de abajo
  strings <- valores[sapply(valores, is.character)]
  
  # solo devuelvo las fuentes que existen
  return(valores[valores %in% posibles_codigos])
}



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = colectar_fuentes(),
    analista = analista,
    pk =  c("iso3"),
    es_serie_tiempo = F,
    control = comparacion,
    columna_indice_tiempo = NULL,
    columna_geo_referencia = 'iso3',
    nivel_agregacion = 'pais',
  )

# df_output %>%
#   argendataR::write_csv_fundar(.,
#                                glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
#   )
# 
# require(RColorBrewer)
# 
# orden_paises <- df_output %>% 
#   filter(disciplina == "Ciencias Físicas") %>%
#   arrange(desc(share)) %>%
#   pull(pais)
# 
# plot_data <- df_output %>% 
#   mutate(
#     disciplina = factor(disciplina, levels = c(
#     "Ciencias Sociales",   
#     "Ciencias de la Salud",
#     "Ciencias de la Vida",
#     "Ciencias Físicas"
#   ), ordered = TRUE),
#     pais = factor(pais, levels = orden_paises)
#   )
# 
# 
# disciplinas <- levels(plot_data$disciplina)
# paleta <- brewer.pal(length(disciplinas), "Set2")
# colores <- setNames(paleta, disciplinas)
# 
# 
# ggplot(plot_data, aes(x = share, y = pais, fill = disciplina)) + 
#   geom_bar(stat = "identity") +
#   scale_fill_manual(values = colores) +
#   guides(fill = guide_legend(reverse = TRUE)) +
#   theme_minimal() +
#   labs(x = "", y = "", fill = "") +
#   theme_minimal() +
#   theme(
#     legend.position = "bottom",
#     axis.text = element_text(color = "black"),
#     axis.title = element_text(color = "black")
#   )
