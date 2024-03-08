# A1_inb_pib
# descripcion

# vars config del script
output_name <- "A1_inb_pib"
# periodo, etc.,

# Descargas -------

#
download.file(url = "https://databank.worldbank.org/data/download/WDI_CSV.zip",
              destfile = glue::glue("data/{subtopico}/datasets/raw/WDI_CSV.zip"))

unzip(zipfile = glue::glue("data/{subtopico}/datasets/raw/WDI_CSV.zip"),
      exdir = glue::glue("data/{subtopico}/datasets/raw/"))

# Lectura -------

wdi_data <- read_csv(glue::glue("data/{subtopico}/datasets/raw/WDIData.csv"))

# Procesamiento -------

paises_sel <- c("ARG", "CHL", "USA", "DEU")

wdi_data <- wdi_data %>% 
 janitor::clean_names()



wdi_data <- wdi_data %>% 
  filter(indicator_code %in% c("NY.GNP.MKTP.CD",
                               "NY.GDP.MKTP.CD"))


wdi_data <- wdi_data %>% 
  pivot_longer(cols = starts_with("x"), values_to = "values", names_to = "anio")

wdi_data <- wdi_data %>% 
  select(anio, country_code, indicator_code, values)

wdi_data <- wdi_data %>% 
  pivot_wider(names_from = indicator_code, values_from = values) %>% 
  janitor::clean_names()

wdi_data <- wdi_data %>% 
  mutate(anio = as.numeric(gsub("\\D","", anio)))

wdi_data <- wdi_data %>% 
  filter(! is.na(ny_gdp_mktp_cd) & ! is.na(ny_gnp_mktp_cd))

wdi_data <- wdi_data %>% 
  mutate(diferencia_inb_pbi = (ny_gnp_mktp_cd/ny_gdp_mktp_cd-1)*100)

wdi_data <- wdi_data %>% 
  filter(country_code %in% paises_sel & anio %in% 2018:2022)

wdi_data <- wdi_data %>% 
  left_join(get_iso_paises(), by = c("country_code" = "iso3"))

df_output <- wdi_data %>% 
  select(anio, pais, iso3 = country_code, diferencia_inb_pbi)

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


write_output( data = df_output,
                extension = 'csv',
                output_name = 'A1_inb_pib',
                subtopico = 'ACECON',
                fuentes = 'World Development Indicators',
                analista = 'Andrés Salles',
                aclaraciones = NULL,
                exportar = TRUE,
                pk = c("anio", "iso3"),
                es_serie_tiempo = TRUE,
                columna_indice_tiempo = "anio",
                columna_geo_referencia = "iso3",
                nivel_agregacion = "pais",
                nullables = FALSE,
                etiquetas_indicadores = list("diferencia_inb_pbi" = "Diferencia entre Ingreso Bruto Nacional y PBI"),
                unidades = "Porcentaje respecto al PBI",
                classes = NULL)
