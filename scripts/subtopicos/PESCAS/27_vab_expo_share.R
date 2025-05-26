#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "PESCAS"
output_name <- "27_vab_expo_share.csv"
analista <- "Ignacio Ibarra"
fuente1 <- 'R223C94' # INDEC VABpb corrientes por sector.
fuente2 <- 'R245C133' # INDEC BOP
fuente3 <- 'R338C211' # INDEC Complejos Exportadores. Revisión 2018. 2021 a ultimo anio


df_vabpb <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)


df_bop <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet(.)


df_complejos <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet(.)

df_vabpb_pesca <- df_vabpb %>% 
  dplyr::filter(trimestre == "Total", sub_sector == "Total sector", anio == max(anio)) %>% 
  mutate(participacion = vab_pb / sum(vab_pb),
         indicador = "PIB") %>% 
  dplyr::filter(sector == "Pesca") %>% 
  select(anio, sector, indicador, participacion)


df_expo_totales_ultimo_anio <- df_bop %>% 
  mutate(sdmx_id = paste(FREQ,
                         ADJUSTMENT,
                         REF_AREA,
                         COUNTERPART_AREA,
                         REF_SECTOR,
                         COUNTERPART_SECTOR,
                         FLOW_STOCK_ENTRY,
                         ACCOUNTING_ENTRY,
                         INT_ACC_ITEM,
                         FUNCTIONAL_CAT,
                         INSTR_ASSET,
                         MATURITY,
                         UNIT_MEASURE,
                         CURRENCY_DENOM,
                         VALUATION,
                         COMP_METHOD,
                         sep = "."
                         ),
         anio = as.integer(str_extract(TIME_PERIOD, "(\\d{4})\\-.*", group = 1)) ) %>% 
  dplyr::filter(sdmx_id == "Q.N.AR.W1.S1.S1.T.C.GS._Z._Z._Z.USD._T._X.N", anio == max(anio)) %>% 
  summarise(expo_total = sum(OBS_VALUE))


df_expo_pesca <- df_complejos %>% 
  dplyr::filter(complejos == "Complejo pesquero", anio == max(anio)) %>% 
  mutate(sector = "Pesca",
         indicador = "Exportaciones",
         participacion = expo /df_expo_totales_ultimo_anio$expo_total) %>% 
  select(anio, sector, indicador, participacion)


df_output <- bind_rows(df_vabpb_pesca, df_expo_pesca)
  

df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )


offset_texto <- 0.0001

ggplot(df_output, aes(x = indicador, y = participacion)) + 
  geom_col(fill = "#7ab5c5") + 
  geom_text(aes(label = scales::percent(participacion, accuracy = 0.1), 
                y = participacion + offset_texto), 
            fontface = "bold", 
            vjust = 0) +  # Ajusta la posición vertical de las etiquetas
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    x = "",
    y = "Porcentaje"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(color = "black"),  # Color negro para los números de los ejes
    axis.title = element_text(color = "black")  # Color negro para los títulos de los ejes
  )
