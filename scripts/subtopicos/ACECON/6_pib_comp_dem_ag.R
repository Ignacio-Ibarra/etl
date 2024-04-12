# 6_pib_comp_dem_ag
# descripcion

# vars config del script
output_name <- "6_pib_comp_dem_ag"
# periodo, etc.,

# Insumos -------

# oferta y demanda en precios corrientes R36C10
oyd_fnys <-  read_csv(fuentes_files[grepl("R36C10", fuentes_files)]) 

# consumo e inversion en precios corrientes R36C11
cei_fnys <- read_csv(fuentes_files[grepl("R36C11", fuentes_files)]) 

# pib en usd “PIB a precios de mercado, en miles de $” 2017 -R36C9 
pbiusd_fnys <- read_csv(fuentes_files[grepl("R36C9", fuentes_files)])
  

# oferta y demanda R38C6
oyd_globales_indec <- read_csv(fuentes_files[grepl("R38C6", fuentes_files)])


# Procesamiento -------

pbiusd_fnys <- pbiusd_fnys %>% 
  filter(anio == 2017 & unidad == "miles de US$") %>% 
  pivot_wider(names_from = indicador, values_from = valor) %>% 
  janitor::clean_names() %>% 
  select(-unidad)


cei_fnys <- cei_fnys %>%
  filter(anio %in% 1935:2017 & grepl("Consumo Total|Consumo Privado|Consumo Público", indicador))


cei_fnys <- cei_fnys %>% 
  pivot_wider(names_from = indicador, values_from = valor) %>% 
  select(-unidad) %>% 
  janitor::clean_names() %>% 
  rename_with(.cols = matches("consumo_"), function(x) {gsub("consumo_consumo", "consumo",x)})

cei_fnys <- cei_fnys %>%
  mutate(ratio_publico = consumo_publico/consumo_total,
         ratio_privado = consumo_privado/consumo_total)

cei_fnys$ratio_publico[cei_fnys$anio %in% 1987:1993] <- zoo::na.approx(cei_fnys$ratio_publico[cei_fnys$anio %in% 1987:1993])
cei_fnys$ratio_privado[cei_fnys$anio %in% 1987:1993] <- zoo::na.approx(cei_fnys$ratio_privado[cei_fnys$anio %in% 1987:1993])

cei_fnys <- cei_fnys %>%
  mutate(consumo_publico = ifelse(is.na(consumo_publico), consumo_total*ratio_publico,consumo_publico ),
         consumo_privado = ifelse(is.na(consumo_privado), consumo_total*ratio_privado, consumo_privado))

cei_fnys <- cei_fnys %>%
  select(-matches("ratio"))

# oferta y demanda fundacion norte y sur

# seleccion columnas
oyd_fnys <- oyd_fnys %>% 
  filter(
    indicador %in% c("Importaciones de bienes y servicios reales",
                     "Exportaciones de bienes y servicios reales",
                     "Formación Bruta de Capital",
                     "Variación de Existencias"
                     )
  )


# filtra años
oyd_fnys <- oyd_fnys %>% 
  filter(anio %in% 1935:2017)

oyd_fnys <- oyd_fnys %>% 
  pivot_wider(names_from = indicador, values_from = valor) %>% 
  select(-unidad)

oyd_fnys <- oyd_fnys %>% 
  janitor::clean_names()

df_fnys <- left_join(oyd_fnys, cei_fnys) 

df_fnys_extrapolacion <- df_fnys %>% 
  filter(anio == 2017) %>% 
  left_join(pbiusd_fnys) %>% 
  mutate(across(-c(anio, iso3, pib_a_precios_de_mercado), \(x) {x/100*pib_a_precios_de_mercado}))


oyd_globales_indec <- 
  %>% 
  .[c(4,5,7:8,12:16),] %>% 
  t() %>% 
  as_tibble(.name_repair = "unique")

names(oyd_globales_indec) <- oyd_globales_indec[1,] %>%
  janitor::make_clean_names()

oyd_globales_indec <- oyd_globales_indec %>% 
  rename(anio = na, trim = na_2)

oyd_globales_indec <- oyd_globales_indec %>% 
  fill(anio)

oyd_globales_indec <- oyd_globales_indec %>% 
  filter(trim == "Total") %>% 
  select(-trim)

oyd_globales_indec <- oyd_globales_indec %>% 
  mutate(across(everything(), \(x) {as.numeric(gsub(" .*", "", x))}))

oyd_globales_indec <- oyd_globales_indec %>% 
  mutate(across(-anio, \(x) {x/lag(x)}, .names = "var_{.col}"))

oyd_globales_indec <- oyd_globales_indec %>% 
  filter(anio > 2017)

oyd_globales_indec <- oyd_globales_indec %>% 
  select(anio, matches("var_"))

df_fnys_extrapolacion <-df_fnys_extrapolacion %>% 
  bind_rows(oyd_globales_indec)

df_fnys_extrapolacion <- df_fnys_extrapolacion %>% 
  mutate(pib = expansor_xvar(pib, var_producto_interno_bruto),
         importaciones_de_bienes_y_servicios_reales = expansor_xvar(importaciones_de_bienes_y_servicios_reales,
                                                                    var_importaciones_fob_bienes_y_servicios_reales),
         exportaciones_de_bienes_y_servicios_reales = expansor_xvar(exportaciones_de_bienes_y_servicios_reales,
                                                                    var_exportaciones_fob_bienes_y_servicios_reales),
         formacion_bruta_de_capital = expansor_xvar(formacion_bruta_de_capital,
                                                                    var_formacion_bruta_de_capital_fijo),
         variacion_de_existencias = expansor_xvar(variacion_de_existencias,
                                                    var_variacion_de_existencias_3),
         consumo_privado = expansor_xvar(consumo_privado,
                                                  var_consumo_privado_5),
         consumo_publico = expansor_xvar(consumo_publico,
                                         var_consumo_publico_5)
         )

df_fnys_extrapolacion <- df_fnys_extrapolacion %>% 
  select(anio, pib, consumo_privado, consumo_publico,
         importaciones_de_bienes_y_servicios_reales,
         exportaciones_de_bienes_y_servicios_reales, variacion_de_existencias,
         formacion_bruta_de_capital)

df_fnys_extrapolacion <- df_fnys_extrapolacion %>% 
  mutate(across(-anio, \(x) {x/pib*100}))

df_fnys_extrapolacion <- df_fnys_extrapolacion %>% 
  select(-pib)
  

df_output <- bind_rows(df_fnys, df_fnys_extrapolacion)

df_output <- df_output %>% 
  select(anio, consumo_hogares = consumo_privado,
         consumo_gobierno = consumo_publico,
         formacion_bruta_de_capital, variacion_de_existencias,
         exportaciones = exportaciones_de_bienes_y_servicios_reales,
         importaciones = importaciones_de_bienes_y_servicios_reales
          )

df_output <- df_output %>% 
  mutate(importaciones = -importaciones)

df_output <- df_output %>% 
  mutate(variacion_de_existencias = replace_na(variacion_de_existencias, 0))

# Control vs output previo -------

# descargo outout primera entrega del drive
# se puede leer outoput del drive directo desde la url
out_prev <- read.csv2(file = glue::glue("https://drive.usercontent.google.com/download?id={outputs$id[grepl(output_name, outputs$name)]}"))

out_prev <- out_prev %>% 
  mutate(across(everything(), as.numeric))

vs <- out_prev %>% 
  left_join(df_output, by = c("anio"))

vs <-  vs %>% 
  mutate(across(where(is.numeric), \(x) round(x, 2))) 


diff <- comparar_cols(vs) %>% 
  filter(if_any(-anio,\(x) x != 0))
  
diff %>% 
  write_argendata(file_name = glue::glue("_diff_{output_name}.csv"),
  subtopico =  subtopico)

# Write output ------


df_output %>% 
  write_argendata(file_name = glue::glue("{output_name}.csv"),
  subtopico = subtopico)
