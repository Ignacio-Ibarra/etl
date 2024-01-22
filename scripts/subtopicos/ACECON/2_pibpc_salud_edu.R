# 2_pibpc_salud_edu
# Se arman 2 gráficos. Uno para “Esperanza de vida” (eje vertical) y “PIB per cápita PPA” (eje horizontal). 
# Y otro para “Años de educación” (eje vertical) y “PIB per cápita PPA” (eje horizontal).
# Se hacen gráficos de dispersión, con línea de tendencia.
# En el eje horizontal, que tiene el PIB per cápita PPA se utiliza escala logarítmica. 


output_name <- "2_pibpc_salud_edu"

# Insumos -------

# anios de expectativa de vida al nacer anio 2018 undp
le_undp <- jsonlite::fromJSON('https://api.hdrdata.org/CountryIndicators/filter?year=2018&indicator=le')

# mean schooling years undp anio 2018
mys_undp <- jsonlite::fromJSON('https://api.hdrdata.org/CountryIndicators/filter?year=2018&indicator=mys')

# population maddison db 2018
pop_maddison_db <- readxl::read_excel(glue::glue("data/{subtopico}/datasets/raw/mpd2020.xlsx"),
                                      sheet = "Population", skip = 1)

# pibpc maddison db anio 2018
pibpc_maddison_db <- readxl::read_excel(glue::glue("data/{subtopico}/datasets/raw/mpd2020.xlsx"), sheet = "GDP pc", skip = 1)


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


pop_maddison_db <- pop_maddison_db %>% 
  #  paso a formato largo
  pivot_longer(cols = -year, names_to = "iso", values_to = "poblacion")

seleccion_paises <- pop_maddison_db %>% 
  filter(year == 2018 & poblacion*1e3 >= 2.5e6) %>% 
  pull(iso) %>% unique()


pibpc_maddison_db <- pibpc_maddison_db %>%
  #  paso a formato largo
  pivot_longer(cols = -year, names_to = "iso", values_to = "pbi_per_capita")

pibpc_maddison_db <- pibpc_maddison_db %>% 
  filter(year == 2018 & ! is.na(pbi_per_capita) & iso %in% seleccion_paises)

pibpc_maddison_db <- pibpc_maddison_db %>% 
  rename(iso3 = iso, anio  = year)

pibpc_salud_edu <- pibpc_maddison_db %>% 
  left_join(le_undp) %>% 
  left_join(mys_undp) %>% 
  select(-anio)

# no dupes por pais
pibpc_salud_edu %>% count(iso3) %>% filter(n > 1) %>% nrow() == 0

iso_countrycodes <- get_iso_paises()

pibpc_salud_edu <- pibpc_salud_edu %>% 
  left_join(iso_countrycodes) %>% 
  relocate(pais, .after = iso3)

pibpc_salud_edu <- pibpc_salud_edu %>% 
  mutate(pbi_per_capita = as.integer(pbi_per_capita),
         across(c(esperanza_de_vida_al_nacer,
                  anios_de_educacion), \(x) round(x, digits = 1)))

# comparo contra output previo -----

# descargo outout primera entrega del drive
# se puede leer outoput del drive directo desde la url
out_prev <- read.csv2(file = glue::glue("https://drive.usercontent.google.com/download?id={outputs$id[grepl(output_name, outputs$name)]}"))

out_prev <- out_prev %>% 
  mutate(across(-c(pais, iso3), as.numeric))

vs <- out_prev %>% 
  left_join(pibpc_salud_edu, by = c("pais", "iso3"))

diff <-  vs %>% 
  mutate(across(where(is.numeric), \(x) round(x, 2))) %>% 
  filter(anios_de_educacion.x !=  anios_de_educacion.y |
           esperanza_de_vida_al_nacer.x  != esperanza_de_vida_al_nacer.y |
           pbi_per_capita.x != pbi_per_capita.y
  ) 

diff %>% 
  write_argendata(file_name = glue::glue("_diff_{output_name}.csv"),
                  subtopico =  subtopico)

# write output ------


pibpc_salud_edu %>% 
  write_argendata(file_name = glue::glue("{output_name}.csv"),
                  subtopico = subtopico)

rm(list = ls())
