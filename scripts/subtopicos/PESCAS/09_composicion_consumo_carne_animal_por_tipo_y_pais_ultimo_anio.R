################################################################################
##                              Dataset: nombre                               ##
################################################################################


#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "PESCAS"
output_name <- "09_composicion_consumo_carne_animal_por_tipo_y_pais_ultimo_anio.csv"
analista = "Ignacio Ibarra"
fuente1 <- 'R299C167' # FAO FBS


df_fao_fbs <- arrow::read_parquet(argendataR::get_clean_path(fuente1)) 


carnes <- c("Bovine Meat" = "Vacuna",
            "Fish, Body Oil" = "Pescados y mariscos",
            "Fish, Liver Oi" = "Pescados y mariscos",
            "Freshwater Fish" = "Pescados y mariscos",
            "Demersal Fish" = "Pescados y mariscos",
            "Pelagic Fish" = "Pescados y mariscos",
            "Marine Fish, Other" = "Pescados y mariscos",
            "Crustaceans" = "Pescados y mariscos",
            "Cephalopods" = "Pescados y mariscos", 
            "Molluscs, Other" = "Pescados y mariscos",
            "Aquatic Animals, Others" = "Pescados y mariscos",
            "Meat, Other" = "Otras carnes",
            "Mutton & Goat Meat" = "Caprina y ovina",
            "Pigmeat" = "Porcina",
            "Poultry Meat" = "Aviar")

df_fao_fbs_filtered <- df_fao_fbs %>% 
  dplyr::filter(item %in% names(carnes), element == "Food supply quantity (kg/capita/yr)")  %>% 
  select(-flags, -notes, -element_code, -element) %>% 
  drop_na(value)


df_paises <- df_fao_fbs_filtered %>% 
  mutate(tipo_carne = carnes[item],
         value = replace_na(value, 0)) %>% 
  group_by(anio = year, iso3, pais, tipo_carne) %>% 
  summarise( valor = sum(value, na.rm = T)) %>% 
  ungroup() %>% 
  complete(anio, iso3, tipo_carne, fill = list(valor = 0)) %>%  
  dplyr::filter(anio == max(anio)) %>% 
  group_by(iso3) %>% 
  mutate(share = 100*valor / sum(valor)) %>% 
  ungroup()


df_mundo <- df_paises %>% 
  group_by(anio, iso3 = "WLD", pais = "Mundo", tipo_carne) %>% 
  summarise(
    valor = mean(valor, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(
    share = 100*valor / sum(valor)
  )


df_output <- df_paises %>% bind_rows(df_mundo)

df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )


nselect <- 5

paises_fijos <- c("Francia", "Canadá", "Estados Unidos", "China", "Japón",
                   "España", "Argentina", "Uruguay", "Brasil", 
                   "México", "Chile", "Perú", "Reino Unido", 
                  "Colombia", "Ecuador", "Mundo")


paises_topn_pescado <- df_output %>% 
  dplyr::filter(!(pais %in% paises_fijos), tipo_carne == "Pescados y mariscos") %>% 
  arrange(-share) %>% 
  slice_head(n = nselect) %>% 
  pull(pais)
  

paises_seleccionados <- c(paises_fijos, paises_topn_pescado)


orden_cat <- c("Otras carnes", "Caprina y ovina", "Porcina", "Vacuna", "Aviar", "Pescados y mariscos")

plot_data <- df_output %>%
  dplyr::filter(pais %in% paises_seleccionados) %>% 
  group_by(iso3) %>% 
  mutate(
    share_pescado = sum(ifelse(tipo_carne == "Pescados y mariscos", share, 0))
  ) %>% 
  ungroup()%>% 
  mutate(pais = factor(pais, levels = unique(pais)),
         tipo_carne = factor(tipo_carne, 
                             levels = orden_cat
                             )
           )



colores_carne <- c(
  "Vacuna" = "#91BD17",
  "Pescados y mariscos" = "#52436E",
  "Otras carnes" = "#B6244F",
  "Caprina y ovina" = "#A47133",
  "Porcina" = "#9B9725",
  "Aviar" = "#728043"
)
  

  

# Gráfico de barras apiladas
ggplot(plot_data, aes(x = share, y = forcats::fct_reorder(pais, share_pescado), fill = tipo_carne)) +
  geom_col() +
  geom_text(aes(label = ifelse(share >= 15, sprintf("%.1f%%", share), "")), 
            position = position_stack(vjust = 0.5), 
            size = 3.5, color = "white") + 
  scale_fill_manual(values = colores_carne) +
  labs(
    x = "Porcentaje del consumo total (%)",
    y = "País",
    fill = "Tipo de carne"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(color = "black"),  
    axis.title = element_text(color = "black")  
  )


