# 3_pibpc_prec_cons
# descripcion

# vars config del script
output_name <- "3_pibpc_prec_cons"
# periodo, etc.,

# Insumos -------

# R36C9
cn_arg_fnys <- read_csv(fuentes_files[grepl("R36C9", fuentes_files)])


# oferta y demanda global trimestral INDEC cuentas nacionales
# R38C6
pib_indec <- read_csv(fuentes_files[grepl("R38C6", fuentes_files)])

# R39C8
pob_indec <- read_csv(fuentes_files[grepl("R39C8", fuentes_files)])

# Procesamiento -------


# proceso cuentas nacionales fund norte y sur (orlando ferreres)

# pibpc = PIB per capita moneda nacional constante 2004
cn_arg_fnys <- cn_arg_fnys %>% 
  filter(indicador == "PIB per capita a precios de mercado" &
           unidad == "$ de 2004 / hab") %>% 
  pivot_wider(names_from = indicador, values_from = valor) %>% 
  janitor::clean_names()

# pib indec
# pib en millones de pesos a precios de 2004

# selecciono filas de anio, pib y trimestre y traspongo
pib_indec <- pib_indec  %>% 
  filter(indicador == "producto_interno_bruto" & trim == "Total" & anio %in% 2019:2022)

pib_indec <- pib_indec %>%
  pivot_wider(names_from = indicador, 
              values_from = valor )

# datos de poblacion indec
pob_indec <- pob_indec %>% 
  filter(anio %in% 2019:2022 & indicador == "total") %>% 
  rename(pob = valor)

# calculo pib per capita
df_indec <- left_join(pib_indec, pob_indec)

df_indec <- df_indec %>% 
  mutate(pibpc = producto_interno_bruto/pob)

df_indec <- df_indec %>% 
  select(anio, pib_per_capita_a_precios_de_mercado  = pibpc)

df_output <- bind_rows(cn_arg_fnys, df_indec) 

df_output <- df_output %>% 
  select(-iso3, -unidad)

df_output <- df_output %>% 
  rename(pbi_per_capita_pconst2004 = pib_per_capita_a_precios_de_mercado)

sum(is.na(df_output$pbi_per_capita_pconst2004))

# Control vs output previo -------

comparacion <- comparar_outputs(df = df_output, nombre = output_name,
                                pk = "anio", drop_output_drive = F)


# Write output ------

write_output(data = df_output, output_name = output_name, subtopico = subtopico, analista = analista,
             fuentes = c("R36C9", "R38C6", "R39C8"),pk = "anio", es_serie_tiempo = T, columna_indice_tiempo = "anio", nivel_agregacion = "pais", etiquetas_indicadores = list("pbi_per_capita_pconst2004" = "PBI per capita"),
             unidades = list("pbi_per_capita_pconst2004" = "pesos constantes 2004 / habitante"))

rm(list = ls())
