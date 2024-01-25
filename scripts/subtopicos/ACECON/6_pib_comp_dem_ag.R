# 6_pib_comp_dem_ag
# descripcion

# vars config del script
output_name <- "6_pib_comp_dem_ag"
# periodo, etc.,

# Insumos -------

oyd_fnys <- readxl::read_excel(path = glue::glue("data/{subtopico}/datasets/raw/cuentas-nacionales-fund-norte-y-sur.xlsx"),
                                  sheet = "OyD %PIB, Precios corr. ", col_names = T, skip = 1)

cei_fnys <- readxl::read_excel(path = glue::glue("data/{subtopico}/datasets/raw/cuentas-nacionales-fund-norte-y-sur.xlsx"),
                               sheet = "CeI %PIB, Precios corr. ", col_names = T, skip = 2)

# pib en usd “PIB a precios de mercado, en miles de $” 2017
pbiusd_fnys <- readxl::read_excel(path = glue::glue("data/{subtopico}/datasets/raw/cuentas-nacionales-fund-norte-y-sur.xlsx"),
                               sheet = "PBI en US$", range = "B224", col_names = F) %>% 
  rename(pib = ...1) %>% mutate(anio = 2017)

oyd_globales_indec <- readxl::read_xls(glue::glue("data/{subtopico}/datasets/raw/sh_oferta_demanda_12_23.xls"),
                                       sheet = "cuadro 8", col_names = F)


# Procesamiento -------

cei_fnys <- cei_fnys %>% 
  janitor::clean_names()

cei_fnys <- cei_fnys %>% 
  select(anio = x1, consumo_total, consumo_privado, consumo_publico)

cei_fnys <- cei_fnys %>%
  mutate(across(everything(), as.numeric))

cei_fnys <- cei_fnys %>%
  filter(anio %in% 1935:2017)

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
# limpia nombres variables
oyd_fnys <- oyd_fnys %>% 
  janitor::clean_names()

# seleccion columnas
oyd_fnys <- oyd_fnys %>% 
  select(anio = ano, importaciones_de_bienes_y_servicios_reales, exportaciones_de_bienes_y_servicios_reales, formacion_bruta_de_capital, variacion_de_existencias)

# filtra años
oyd_fnys <- oyd_fnys %>% 
  filter(anio %in% 1935:2017)

oyd_fnys <- oyd_fnys %>% 
  mutate(across(everything(), as.numeric))

df_fnys <- left_join(oyd_fnys, cei_fnys) 

df_fnys_extrapolacion <- df_fnys %>% 
  filter(anio == 2017) %>% 
  left_join(pbiusd_fnys) %>% 
  mutate(across(-c(anio, pib), \(x) {x/100*pib}))


oyd_globales_indec <- oyd_globales_indec %>% 
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
