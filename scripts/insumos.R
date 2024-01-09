library(tidyverse)

# Insumos -------


# cuentas nacionales fundacion norte y sur 

download.file("https://docs.google.com/spreadsheets/d/e/2PACX-1vTAGGfIqDw18YDI5zasGBRa4sG1ddUfMcKT87fzTkvz8HMe8Ipl6zJU0M2788oZrw/pub?output=xls",
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("data/_INSUMOS/raw/cuentas-nacionales-fund-norte-y-sur.xlsx"))


# IMF outlook database 
# descargo la base entera por mayor facilidad de referencia

download.file(url = "https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2023/WEOOct2023all.ashx",
              destfile = glue::glue("data/_INSUMOS/raw/WEOOct2023all.xls"))



# Maddison database

download.file(url = "https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2020.xlsx", 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("data/_INSUMOS/raw/mpd2020.xlsx"))

# Lectura y procesamiento -----------
