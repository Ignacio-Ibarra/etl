#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "PESCAS"
output_name <- "08_consumo_pescado_mariscos_per_capita_pais_ultimo_anio.csv"
fuente1 <- 'R299C167' # FAO FBS
fuente2 <- 'R300C168' # FAO FBSH


# nombres_fao <- c("Meat, beef | 00002731 || Food available for consumption | 0645pc || kilograms per year per capita" = "Vacuna",
#                  "Fish and seafood | 00002960 || Food available for consumption | 0645pc || kilograms per year per capita" = "Pescados y mariscos",
#                  "Meat, Other | 00002735 || Food available for consumption | 0645pc || kilograms per year per capita" = "Otras carnes",
#                  "Meat, sheep and goat | 00002732 || Food available for consumption | 0645pc || kilograms per year per capita" = "Caprina y ovina",
#                  "Meat, pig | 00002733 || Food available for consumption | 0645pc || kilograms per year per capita" = "Porcina",
#                  "Meat, poultry | 00002734 || Food available for consumption | 0645pc || kilograms per year per capita" = "Aviar")
# 
# 
# df_owid_arg <- read_csv("per-capita-meat-type.csv") %>%
#   select(-Entity) %>%
#   rename(anio = Year, iso3 = Code) %>%
#   pivot_longer(!matches("anio|iso3"),
#                names_to = 'grupo_carne',
#                values_to = 'value_owid') %>%
#   mutate(
#     grupo_carne = recode(grupo_carne, !!!nombres_fao)
#   )

df_fao_fbs <- arrow::read_parquet(argendataR::get_clean_path(fuente1)) 

df_fao_fbsh <- arrow::read_parquet(argendataR::get_clean_path(fuente2))


pescados_mariscos <- c(
  "Fish, Body Oil" = "Pescados y mariscos",
  "Fish, Liver Oi" = "Pescados y mariscos",
  "Freshwater Fish" = "Pescados y mariscos",
  "Demersal Fish" = "Pescados y mariscos",
  "Pelagic Fish" = "Pescados y mariscos",
  "Marine Fish, Other" = "Pescados y mariscos",
  "Crustaceans" = "Pescados y mariscos",
  "Cephalopods" = "Pescados y mariscos", 
  "Molluscs, Other" = "Pescados y mariscos",
  "Aquatic Animals, Others" = "Pescados y mariscos"
  )

df_fao_fbs_filtered <- df_fao_fbs %>% 
  dplyr::filter(item %in% names(pescados_mariscos), element == "Food supply quantity (kg/capita/yr)")  %>% 
  group_by(anio = year, iso3, pais) %>% 
  summarise(value_new = sum(value, na.rm = T)) %>% 
  ungroup()


df_fao_fbsh_filtered <- df_fao_fbsh %>% 
  dplyr::filter(item %in% names(pescados_mariscos), element == "Food supply quantity (kg/capita/yr)")  %>% 
  group_by(anio = year, iso3, pais) %>% 
  summarise(value_old = sum(value, na.rm = T)) %>% 
  ungroup()

impute_backward <- function(A, B) {
  # Calcular las variaciones relativas de B
  result <- rep(NA_real_, length(A))
  
  VarB <- B / dplyr::lag(B)
  
  # Encontrar el primer índice con un valor no nulo en A
  t0 <- min(which(!is.na(A)))
  
  result[t0] = A[t0]
  
  # Imputar hacia atrás
  for (t in (t0 - 1):1) {
    if (!is.na(VarB[t + 1]) & is.na(A[t])) {
      result[t] <- result[t + 1] / VarB[t + 1]
    }
  }
  
  return(result)
}



df_fao_fbs_empalme <- df_fao_fbs_filtered %>% 
  full_join(df_fao_fbsh_filtered, join_by(anio, iso3, pais)) %>% 
  group_by(iso3) %>%
  filter(any(anio == 2010 & !is.na(value_new) & !is.na(value_old))) %>%
  ungroup() %>% 
  arrange(iso3, anio) %>% 
  group_by(iso3) %>% 
  mutate(valor_ = impute_backward(value_new, value_old),
         valor_empalme = ifelse(is.na(value_new), valor_, value_new)) %>% 
  ungroup() 



df_paises <- df_fao_fbs_empalme %>% 
  dplyr::filter(anio == max(anio)) %>% 
  select(iso3, pais, consumo_per_capita = valor_empalme) 


df_mundial <- df_fao_fbs_empalme %>% 
  dplyr::filter(anio == max(anio)) %>% 
  group_by(iso3 = "WLD", pais = "Mundo") %>% 
  summarise(
    consumo_per_capita = mean(valor_empalme, na.rm = T)
  )

df_output <- df_paises %>% 
  bind_rows(df_mundial) %>% 
  arrange(-consumo_per_capita)

df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )




nselect <- 10

paises_seleccionados <- c("Francia", "Canadá", "Estados Unidos", 
                          "Japón", "España", "Argentina", "Uruguay", 
                          "Brasil", "México", "Chile", "Perú", 
                          "Reino Unido", "Colombia", "Ecuador", "Mundo")

plot_data <- df_output %>%
  arrange(desc(consumo_per_capita)) %>%  # Ordenar de mayor a menor consumo
  slice_head(n = nselect) %>%  # Tomar los 10 primeros (más consumo)
  bind_rows(
    df_output %>% filter(pais %in% paises_seleccionados)  # Agregar Argentina si no está en los extremos
  ) %>%
  distinct() %>% 
  arrange(consumo_per_capita)%>% 
  mutate(pais = factor(pais, levels = unique(pais)))

regular_texto <- 5

ggplot(plot_data, aes(x = consumo_per_capita, y = pais, 
                      fill = case_when(
                        pais == "Argentina" ~ "Argentina",
                        pais == "Mundo" ~ "Mundo",
                        TRUE ~ "Otros"
                      ))) + 
  geom_col(color = "black", linewidth = 0.3, position = position_nudge(y = 0.2), width = 0.8) +  
  scale_fill_manual(values = c("Argentina" = "#45bcc5", "Mundo" = "#383636", "Otros" = "#4285f4")) +  # Colores condicionales
  geom_text(aes(
    label = format(round(consumo_per_capita, 2), decimal.mark = ".", scientific = FALSE),
    x = consumo_per_capita - regular_texto),  
    vjust = 0, hjust = 0, color = "white", fontface = "bold") +  # Color de las etiquetas en blanco
  labs(y = "", x = "Consumo per cápita (kg por año)") +
  theme_minimal() +
  theme(
    legend.position = "none",  # Oculta la leyenda
    axis.text = element_text(color = "black"),  
    axis.title = element_text(color = "black")  
  )


