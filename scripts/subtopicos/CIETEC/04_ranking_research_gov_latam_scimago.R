# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "CIETEC"
output_name <- "04_ranking_research_gov_latam_scimago.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R353C229' # SCImago Institutions Rankings - Reaserch Ranking - Sector: Government - Country: Latin America
fuente2 <- 'R391C244' # SCImago Ranking Evolution - Sector: Government - Country: Latin America

# Lectura de archivos Parquet
df_countries_institutions <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.) %>% select(iso3, institution = institucion) %>% 
  mutate(institution = trimws(gsub("\\*","", institution)),
         iso3 = trimws(iso3)) %>% 
  distinct() %>% 
  bind_rows(
    data.frame(
      iso3 = c(
        "BRA",
        "PAN",
        "MEX"
      ),
      institution = c(
        "Ministerio da Defesa",
        "Instituto Smithsonian de Investigaciones Tropicales, Panama",
        "Secretaria de Agricultura y Desarrollo Rural"
      )
    )
  )

df_scimago <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet(.) %>% 
  mutate(institution = trimws(gsub("\\*","", institution))) %>% 
  left_join(df_countries_institutions, join_by(institution))

X <- df_scimago %>% 
  dplyr::filter(is.na(iso3)) 


if(nrow(X)>0){stop("Hay instituciones que no tienen su respectivo iso3")}


geonomenclador <- argendataR::get_nomenclador_geografico_front() %>% 
  select(iso3 = geocodigo, nombre_pais = name_long)

df_output <- df_scimago %>% 
  left_join(geonomenclador, join_by(iso3)) %>% 
  mutate(ranking = ceiling(pos)) %>% 
  select(anio = year, iso3, nombre_pais, idp, institucion=institution, ranking) %>% 
  dplyr::filter(anio < year(Sys.Date())) 

df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )

library(ggrepel)

colores_paises <- c(
  "Argentina" = "#1f77b4",
  "Brasil" = "#ff7f0e",
  "Chile" = "#2ca02c",
  "Uruguay" = "#d62728",
  "Panamá" = "#9467bd",
  "Máxico" = "#8c564b",
  "Perú" = "#e377c2",
  "Cuba" = "#7f7f7f"
)


ggplot(df_output, aes(x = as.factor(anio), y = ranking, group = institucion)) +
  geom_line(aes(color = nombre_pais), linewidth = 0.6) +
  geom_point(aes(color = nombre_pais),size = 2) +
  geom_text_repel(
    data = df_output %>% filter(anio == "2024"),
    aes(label = institucion),
    size = 2.5,
    direction = "y",
    segment.size = 0.2,
    hjust = 1,                  # << Etiqueta alineada a la izquierda
    nudge_x = 8               # << Empuja el texto un poco a la derecha
  ) +
  # scale_x_discrete(expand = expansion(add = c(0.2, 0.2))) +
  scale_y_reverse(breaks = 1:max(df_output$ranking)) +
  scale_color_manual(values = colores_paises) +
  theme_minimal() +
  theme(legend.position = "top", 
        legend.title = element_blank()) +
  labs(title = NULL, x = NULL, y = "Ranking")



