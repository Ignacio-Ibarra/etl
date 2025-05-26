#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "PESCAS"
output_name <- "28_consumo_pescado_decil.csv"
analista <- "Ignacio Ibarra"
fuente1 <- 'R394C248' # ENGHO 2017/2018 Base de hogares
fuente2 <- 'R392C245' # ENGHO 2017/2018 Base de gasto
fuente3 <- 'R393C247' # ENGHO 2017/2018 Articulos


df_hogares <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet() %>% 
  select(id, dinth_t) %>% 
  mutate(dinth_t = as.integer(dinth_t))


df_gasto <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet() 


df_articulos <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet()

df_alimentos <- df_articulos %>% 
  dplyr::filter(grepl("^A011.*", articulo)) %>% 
  select(articulo, articulo_desc, clase_desc, subclase_desc, grupo_desc)


df_gasto_alimentos <- df_gasto %>% 
  right_join(df_alimentos, join_by(articulo)) 


df_gasto_deciles <- df_hogares %>% 
  left_join(df_gasto_alimentos, join_by(id)) %>% 
  mutate(pondera = ifelse(is.na(pondera),0, pondera),
         monto = ifelse(is.na(monto), 0, monto),
         ponderamonto = pondera*monto) %>% 
  group_by(articulo, articulo_desc, clase_desc, subclase_desc, grupo_desc, dinth_t) %>% 
  summarise(
    ponderamonto = sum(ponderamonto, na.rm = T)
  ) %>% 
  ungroup() %>% 
  group_by(dinth_t) %>% 
  mutate(
    share_gasto = round(100* ponderamonto / sum(ponderamonto),3)
  ) %>% 
  ungroup() %>% 
  dplyr::filter(clase_desc == "Pescado") %>% 
  select(-ponderamonto)


df_output <- df_gasto_deciles %>% 
  group_by(dinth_t, subclase_desc) %>% 
  summarise(
    share_gasto = sum(share_gasto)
  ) %>% 
  ungroup()



df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )




ggplot(df_output, aes(x = as.factor(dinth_t), y = share_gasto, fill = subclase_desc)) + 
  geom_col()  + 
  labs(y = "Porcentaje del gasto en alimentos", x = "Decil") +
  theme_minimal() +
  theme(
    legend.position = "bottom",                          # Mueve la leyenda abajo
    legend.direction = "horizontal",                    # Dirección horizontal
    legend.box = "vertical",                            # Pone la leyenda debajo del gráfico
    legend.text = element_text(size = 8),               # Tamaño del texto
    legend.title = element_blank(),              # Tamaño del título
    legend.spacing.x = unit(0.5, 'cm')                  # Espacio entre elementos
  ) 
