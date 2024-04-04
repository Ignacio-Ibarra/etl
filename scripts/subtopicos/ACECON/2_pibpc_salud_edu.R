# 2_pibpc_salud_edu
# Se arman 2 gráficos. Uno para “Esperanza de vida” (eje vertical) y “PIB per cápita PPA” (eje horizontal). 
# Y otro para “Años de educación” (eje vertical) y “PIB per cápita PPA” (eje horizontal).
# Se hacen gráficos de dispersión, con línea de tendencia.
# En el eje horizontal, que tiene el PIB per cápita PPA se utiliza escala logarítmica. 


output_name <- "2_pibpc_salud_edu"

# Insumos -------

# anios de expectativa de vida al nacer anio 2018 undp
# R41C0
le_undp <- read_csv(fuentes_files[grepl("R41C0", fuentes_files)])
  
  
# mean schooling years undp anio 2018
# R40C0
mys_undp <- read_csv(fuentes_files[grepl("R40C0", fuentes_files)])
  
# population maddison db 2018
# pibpc maddison db anio 2018
# R37C1
mpd2020 <- read_csv(fuentes_files[grepl("R37C1", fuentes_files)])


# procesamiento -------------------------------------------------


# anios de expectativa de vida al nacer anio 2018 undp
le_undp <- le_undp %>% 
  mutate(across(c(value, year), as.numeric))

le_undp <- le_undp %>% 
  mutate(country = gsub(pattern = " - .*", "", country)) %>% 
  filter(!grepl("\\.",country))

le_undp <- le_undp %>% 
  select(year, country, value)

le_undp <- le_undp %>% 
  rename(iso3 = country,
         esperanza_de_vida_al_nacer = value,
         anio = year)



# mean schooling years undp anio 2018

mys_undp <- mys_undp %>% 
  mutate(across(c(value, year), as.numeric))

mys_undp <- mys_undp %>% 
  mutate(country = gsub(pattern = " - .*", "", country)) %>% 
  filter(!grepl("\\.",country)) 

mys_undp <- mys_undp %>% 
  select(year, country, value)

mys_undp <- mys_undp %>% 
  rename(iso3 = country,
         anios_de_educacion = value,
         anio = year)


# maddisn

mpd2020 <- mpd2020 %>%
  pivot_wider(names_from = indicador, values_from = valor) %>% 
  filter(anio == 2018 & pop >= 2.5e6 & !is.na(gdppc))


seleccion_paises <- mpd2020 %>% 
  pull(iso3) %>% unique()

pibpc_salud_edu <- mpd2020 %>% 
  left_join(le_undp) %>% 
  left_join(mys_undp) #%>% 
  # select(-anio)

# no dupes por pais
pibpc_salud_edu %>% count(iso3) %>% filter(n > 1) %>% nrow() == 0

iso_countrycodes <- get_nomenclador_geografico() %>% 
  select(codigo_fundar, pais = desc_fundar)

pibpc_salud_edu$iso3[!pibpc_salud_edu$iso3 %in% iso_countrycodes$codigo_fundar]

pibpc_salud_edu <- pibpc_salud_edu %>% 
  select(-country)

nrow(pibpc_salud_edu)

pibpc_salud_edu <- left_join(pibpc_salud_edu, iso_countrycodes, by = c("iso3" = "codigo_fundar"))

nrow(pibpc_salud_edu)

sum(is.na(pibpc_salud_edu$pais))

output <- pibpc_salud_edu %>% 
  select(-c(pop, anio)) %>%
  rename(pbi_per_capita = gdppc) %>% 
  filter(if_all(everything(), function(x) !is.na(x)))



# comparo contra output previo -----

comparacion <- comparar_outputs(df = output %>% 
                                  mutate(pais = textclean::replace_non_ascii(pais)), nombre = output_name,
                                pk = c("iso3"), drop_output_drive = F)

# write output ------


pibpc_salud_edu %>% 
  write_output(data = ., output_name = output_name,
               subtopico = "ACECON",
               fuentes = c("R40C0", "R41C0", "R37C1"),
               analista = "andressalles@hotmail.com",
               exportar = T,
               pk = c("iso3"),
               es_serie_tiempo = F,
               columna_indice_tiempo = "anio",
               columna_geo_referencia = "iso3", 
               nivel_agregacion = "pais",
               etiquetas_indicadores = list(anio = "Año",
                                            iso3 = "País Código ISO3",
                                            gdppc = "PBI per cápita PPA", 
                                            esperanza_de_vida_al_nacer = "Esperanza de vida al nacer",
                                            anios_de_educacion = "Años de educación promedio"),
               unidades = list(gdppc = "PPA/habitante",
                               esperanza_de_vida_al_nacer = "Años",
                               anios_de_educacion = "Años"))
 
rm(list = ls())
