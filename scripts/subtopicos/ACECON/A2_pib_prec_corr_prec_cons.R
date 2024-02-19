# A2_pib_prec_corr_prec_cons
# descripcion

# vars config del script
output_name <- "A2_pib_prec_corr_prec_cons"

# periodo, etc.,

# Descargas -------


# Lectura -------

# pib a precios constantes 2004 - serie oferta y demanda indec
pib_ctes <- readxl::read_excel("data/ACECON/datasets/raw/sh_oferta_demanda_12_23.xls",
                   sheet = 2) 

# pib a precios corrientes 2004 - serie oferta y demanda indec
pib_corr <- readxl::read_excel("data/ACECON/datasets/raw/sh_oferta_demanda_12_23.xls",
                               sheet = 9) 

# Procesamiento -------

# formato tidy de pib_cts
pib_ctes <- pib_ctes %>% 
  .[-c(1:2),] %>% 
  t() %>% 
  as_tibble(.name_repair = "unique")

names(pib_ctes) <- pib_ctes[1,] %>%
  janitor::make_clean_names() 

pib_ctes <- pib_ctes %>% 
  rename(anio = na, trim = na_2)

pib_ctes <- pib_ctes %>% 
  select(-na_3)

pib_ctes <- pib_ctes %>% 
  mutate(anio = as.numeric(gsub(" .*", "", anio )))

pib_ctes <- pib_ctes %>% 
  fill(anio)

pib_ctes <- pib_ctes %>% 
  filter(!is.na(trim))

pib_ctes <- pib_ctes %>% 
  filter(trim == "Total") %>% 
  select(anio, producto_interno_bruto)

pib_ctes <- pib_ctes %>%
  rename(pbi_precios_const2004 = producto_interno_bruto)

# formato tidy de pib_corr
pib_corr <- pib_corr %>% 
  .[-c(1:2),] %>% 
  t() %>% 
  as_tibble(.name_repair = "unique")

names(pib_corr) <- pib_corr[1,] %>%
  janitor::make_clean_names() 

pib_corr <- pib_corr %>% 
  rename(anio = na, trim = na_2)

pib_corr <- pib_corr %>% 
  select(-na_3)

pib_corr <- pib_corr %>% 
  mutate(anio = as.numeric(gsub(" .*", "", anio )))

pib_corr <- pib_corr %>% 
  fill(anio)

pib_corr <- pib_corr %>% 
  filter(!is.na(trim))

pib_corr <- pib_corr %>% 
  filter(trim == "Total") %>% 
  select(anio, producto_interno_bruto)



# Control vs output previo -------

# descargo outout primera entrega del drive
# se puede leer outoput del drive directo desde la url
out_prev <- read.csv2(file = glue::glue("https://drive.usercontent.google.com/download?id={outputs$id[grepl(output_name, outputs$name)]}"))

out_prev <- out_prev %>% 
  mutate(across(-c(), as.numeric))

vs <- out_prev %>% 
  left_join(df_output, by = c())

vs <-  vs %>% 
   mutate(across(where(is.numeric), \(x) round(x, 2))) 

diff <- comparar_cols(vs) 

diff <- diff %>% 
  filter(if_any(-anio, \(x) x != 0))

diff %>% 
  write_argendata(file_name = glue::glue("_diff_{output_name}.csv"),
  subtopico =  subtopico)

# Write output ------


df_output %>% 
  write_argendata(file_name = glue::glue("{output_name}.csv"),
  subtopico = subtopico)