# 1_pib_pibpc_pob_arg_esp
# output con el pib, pib per capita y poblacion de argentina y españa
# a partir de los datos de la web de Fundación Norte y Sur, World Economic Outlook y Maddison Project Database

library(tidyverse)


subtopico <- "ACECON"

# Insumos


# cuentas nacionales fundacion norte y sur 

download.file("https://docs.google.com/spreadsheets/d/e/2PACX-1vTAGGfIqDw18YDI5zasGBRa4sG1ddUfMcKT87fzTkvz8HMe8Ipl6zJU0M2788oZrw/pub?output=xls",
              mode = "wb",
              destfile = glue::glue("data/{subtopico}/datasets/raw/cuentas-nacionales-fund-norte-y-sur.xlsx"))

readxl::read_excel(glue::glue("data/{subtopico}/datasets/raw/cuentas-nacionales-fund-norte-y-sur.xlsx"))

# IMF outlook database 

# revisar si la wsid vence o se puede reusar
url <- "https://www.imf.org/imf/weodatabase/downloadreport?c=213,&s=NGDP_R,NGDPRPC,LP,&sy=2018&ey=2022&ssm=0&scsm=0&scc=0&ssd=1&ssc=0&sic=1&sort=country&ds=.&br=1&wsid=69dccfdb-9723-44a6-99d3-6a0c1eac4300"

download.file(url = url,
              mode = "wb",
              destfile = glue::glue("data/{subtopico}/datasets/raw/imf-odb-pib-pibpc-pop-arg.xls"))

readxl::read_xls(glue::glue("data/{subtopico}/datasets/raw/imf-odb-pib-pibpc-pop-arg.xls"))

download.file(url = "https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2020.xlsx")
