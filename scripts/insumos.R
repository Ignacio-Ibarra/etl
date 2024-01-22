library(tidyverse)


# cuentas nacionales fundacion norte y sur   -----------
# ngdp_r = PIB moneda nacional constante 2004 (esta en miles)
# ngdprpc = PIB per capita moneda nacional constante 2004

download.file("https://docs.google.com/spreadsheets/d/e/2PACX-1vTAGGfIqDw18YDI5zasGBRa4sG1ddUfMcKT87fzTkvz8HMe8Ipl6zJU0M2788oZrw/pub?output=xls",
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("data/_INSUMOS/raw/cuentas-nacionales-fund-norte-y-sur.xlsx"))


# IMF outlook database   ----------- 
# descargo la base entera por mayor facilidad de referencia
# unidades 
# "NGDP_R" (pib) esta en miles de millones de moneda nacional constantes (1e9)
# "NGDPRPC" (pib per capita) esta en moneda nacional constantes
# "LP" (poblacion) esta en millones (1e6)

download.file(url = "https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2023/WEOOct2023all.ashx",
              destfile = glue::glue("data/_INSUMOS/raw/WEOOct2023all.xls"))



# Maddison database  ----------- 
# GDP pc	Real GDP per capita in 2011$
# Population	Population, mid-year (thousands)

download.file(url = "https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2020.xlsx", 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("data/_INSUMOS/raw/mpd2020.xlsx"))

# api undp human development report office -----------
# https://api.hdrdata.org/swagger/index.html

# oferta y demanda global trimestral INDEC cuentas nacionales  -----------
oyd_cn_indec_url <- "https://www.indec.gob.ar/ftp/cuadros/economia/sh_oferta_demanda_12_23.xls"  

download.file(url = oyd_cn_indec_url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("data/_INSUMOS/raw/sh_oferta_demanda_12_23.xls"))

# proyeccion nacional poblacion indec   -----------
pob_indec_url <- "https://www.indec.gob.ar/ftp/cuadros/poblacion/c1_proyecciones_nac_2010_2040.xls"


download.file(url = pob_indec_url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("data/_INSUMOS/raw/c1_proyecciones_nac_2010_2040.xls"))
