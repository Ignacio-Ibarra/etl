# 3_pibpc_prec_cons
# descripcion

# vars config del script
output_name <- "3_pibpc_prec_cons"
# periodo, etc.,

# Insumos -------


cn_arg_fnys <- readxl::read_excel(path = glue::glue("data/{subtopico}/datasets/raw/cuentas-nacionales-fund-norte-y-sur.xlsx"),
                                  sheet = "PBI en US$", col_names = F)


# oferta y demanda global trimestral INDEC cuentas nacionales
oyd_cn_indec_url <- "https://www.indec.gob.ar/ftp/cuadros/economia/sh_oferta_demanda_12_23.xls"  

download.file(url = oyd_cn_indec_url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("data/{subtopico}/datasets/raw/sh_oferta_demanda_12_23.xls"))

pib_indec <- readxl::read_xls(glue::glue("data/{subtopico}/datasets/raw/sh_oferta_demanda_12_23.xls"), sheet = 2, col_names = F)


# estimacion nacional de poblacion indec
pob_indec_url <- "https://www.indec.gob.ar/ftp/cuadros/poblacion/c1_proyecciones_nac_2010_2040.xls"


download.file(url = pob_indec_url,
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("data/{subtopico}/datasets/raw/c1_proyecciones_nac_2010_2040.xls"))

pob_indec <- readxl::read_xls(glue::glue("data/{subtopico}/datasets/raw/c1_proyecciones_nac_2010_2040.xls"), col_names = F) 


# Procesamiento -------


# proceso cuentas nacionales fund norte y sur (orlando ferreres)

cn_arg_fnys <- cn_arg_fnys %>% 
  select(1, 11) %>% .[107:225,] # definicion del analista de datos a usar

# pibpc = PIB per capita moneda nacional constante 2004
colnames(cn_arg_fnys) <- c("anio", "pibpc")

cn_arg_fnys <- cn_arg_fnys %>% 
  mutate(across(everything(), as.numeric))

# pib indec
# pib en millones de pesos a precios de 2004

# selecciono filas de anio, pib y trimestre y traspongo
pib_indec <- pib_indec[c(4,5,7),] %>%
  t() %>%
  as_tibble()

# renombro columnas
pib_indec <- pib_indec %>% 
  rename(anio = V1, trim = V2, pib_cons = V3)

# completo valores faltantes en anio up to down
pib_indec <- pib_indec %>% 
  fill(anio) %>% 
  filter(!is.na(trim))

# limpio numeros de anio
pib_indec <- pib_indec %>% 
  mutate(anio = as.numeric(gsub("\\(.*", "", anio)))

# me quedo con filas de total anual y anios 2018 a 2022
pib_indec <- pib_indec %>% 
  filter(trim == "Total" & anio %in% 2019:2022)

pib_indec <- pib_indec %>%
  mutate(pib_cons = as.numeric(pib_cons))

# datos de poblacion indec
pob_indec <- pob_indec %>% 
  rename(anio = ...1, pob = ...2)

pob_indec <- pob_indec %>%
  select(1,2) %>% 
  mutate(across(everything(), as.numeric)) %>% 
  filter(anio %in% 2019:2022)

# calculo pib per capita
df_indec <- left_join(pib_indec, pob_indec)

df_indec <- df_indec %>% 
  mutate(pibpc = pib_cons*1e6/pob)

#
df_indec <- df_indec %>% 
  select(-c(trim, pib_cons, pob))

df_output <- bind_rows(cn_arg_fnys, df_indec) 

df_output <- df_output %>% 
  rename(pbi_per_capita_pconst2004= pibpc)

# Control vs output previo -------

# descargo outout primera entrega del drive

# se puede leer outoput del drive directo desde la url
out_prev <- read.csv2(file = glue::glue("https://drive.usercontent.google.com/download?id={outputs$id[grepl(output_name, outputs$name)]}"))

out_prev <- out_prev %>% 
  mutate(across(everything(), as.numeric))

vs <- out_prev %>% 
  left_join(df_output, by = "anio")

diff <-  vs %>% 
  mutate(across(where(is.numeric), \(x) round(x, 2))) %>% 
  filter(pbi_per_capita_pconst2004.x !=  pbi_per_capita_pconst2004.y ) 

diff %>% 
  write_argendata(file_name = glue::glue("_diff_{output_name}.csv"),
                  subtopico =  subtopico)


# Write output ------

df_output %>% 
  write_argendata(file_name = glue::glue("{output_name}.csv"),
  subtopico = subtopico)

rm(list = ls())
